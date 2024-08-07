# get tide data from noaa ----------------------------------
library(tidyverse)
library(lubridate)
library(rvest)
library(duckdb)
library(duckplyr)
library(arrow)
library(progressr)

tides_noaa <- duckplyr_df_from_parquet("data/tides_noaa.parquet")
wq_tide_key <- df_from_parquet("data/wq_meta_station_key.parquet") %>%
  select(site_id,tide_noaa_id) %>%
  na.omit()

wq_data_times <- df_from_parquet("data/wq_data.parquet") %>%
  select(site,site_id,sample_time) %>%
  left_join(wq_tide_key, by = "site_id") %>%
  na.omit()

# use duckdb for every tidyverse function
methods_overwrite()

#  default station is the battery NYC
battery <- "8518750"
# functions to assign tide data water quality spreadsheet ----------------------

# Function to impute current as an index based on tide range and duration
# and time since last slack tide, using a sine function.
# Negative current is ebbing, positive is flooding.
impute_current <- function(hours_since_last=0.1,tide_range_ft=5, tide_duration_hrs=6) {
  # Constants for the sine function
  amplitude <- tide_range_ft  # Maximum current speed index is proportional to tide range
  # Calculate the current speed using the sine function
  current_speed <- amplitude * sin(pi * (hours_since_last / tide_duration_hrs))
  return(current_speed)
}


# get tide position for a specific time
get_tide_position <- function(obs_time = as.POSIXct("2011-10-20 18:43:00"), station = "8530645") {
  # find the closest tide time to the observation time
  # use global tides_noaa. Is it faster?
  try_tide <- function(station){
    tide_pos <- tides_noaa |>
      filter(tide_noaa_id == station) |>
      mutate(hours_since_last = as.numeric(difftime(obs_time, datetime, units = "hours"))) |>
      filter(hours_since_last > 0) |>
      slice_min(order_by = hours_since_last, n = 1) |>
      #  take estimated_tide == FALSE when a proxy is also returned
      arrange(hours_since_last) |>
      tail(1) |>
      rename(tide_time = datetime) |>
      # mutate(flood_or_ebb = if_else(hi_lo == "H", -1, 1)) |>
      mutate(current = impute_current(hours_since_last,tide_range_ft,tide_duration_hrs)) |>
      select(-date, -hi_lo,-tide_range_ft,-tide_duration_hrs,-tide_noaa_id)
  }
  tide_pos <- try_tide(station)
  if (tide_pos$hours_since_last[1] > 8){
    tide_pos <- try_tide(battery)
    tide_pos$good_tide_station <- FALSE
  }
  return(tide_pos)
}

get_tide_position_v <- Vectorize(get_tide_position)


get_tide_time <- function(obs_time, station = "8530645") {
  # find the closest tide time to the observation time
  # use global tides_noaa. Is it faster?
  tide_time <- tides_noaa |>
    filter(tide_noaa_id == station) |>
    mutate(hours_since_last = as.numeric(difftime(obs_time, datetime, units = "hours"))) |>
    filter(hours_since_last > 0) |>
    slice_min(order_by = hours_since_last, n = 1) |>
    #  take estimated_tide == FALSE when a proxy is also returned
    arrange(hours_since_last) |>
    head(1) |>
    pull(datetime)
  return(tide_time)
}

get_tide_time_v <- Vectorize(get_tide_time)


# assign best available tide data to each sample -------------------------------

# test
get_tide_position(ymd_hms("2011-10-20 12:00:00",tz= "America/New_York"))
get_tide_time    (ymd_hms("2011-10-20 12:00:00",tz= "America/New_York"),"8530645")

# get tide position for all sample times in wq_data
# mutate to return a list column

# add tide data for each observation
methods_restore()
wq_data_times <- as_tibble(wq_data_times) %>%
  na.omit()
tides_noaa <- as_tibble(tides_noaa)
# tictoc::tic()
# wq_data_tides <- wq_data_times |>
#   as_tibble() |>
#   # there should be no NA in closest_tide_Id with clean data
#   filter(!is.na(tide_noaa_id)) |>
#   rowwise() %>%
#   mutate(tide_data = map2_dfr(sample_time,tide_noaa_id,get_tide_position)) |>
#   unnest(tide_data)
# tictoc::toc()

# alternate loop method
# about the same speed but
# lets us view progress
tictoc::tic()
tides_data <- tibble()
with_progress({
  p = progressor(along = 1:nrow(wq_data_times))
  for (n in 1:nrow(wq_data_times)) {
    tide_pos <- get_tide_position(wq_data_times$sample_time[n],
                                  wq_data_times$tide_noaa_id[n]) %>%
      mutate(site_id = wq_data_times$site_id[n],
             sample_time = wq_data_times$sample_time[n],.before = 1)
    tides_data <- bind_rows(tides_data,tide_pos)
    p()
  }
})
tictoc::toc()

# write to parquet
arrow::write_parquet(tides_data,"data/wq_tide_data.parquet")
# temp <- left_join(wq_data,tides_data,by = c("site_id","sample_time"))
