# get tide data from noaa ----------------------------------
# update existing data as needed
library(tidyverse)
library(lubridate)
#library(rnoaa)
library(rvest)
library(duckdb)
library(duckplyr)
library(arrow)

# use duckdb for every tidyverse function
methods_overwrite()

#  default station is the battery NYC
battery <- "8518750"

# functions to get tide data from NOAA -----------------------------------------
get_json_response <- function(url) {
  response <- httr::GET(url) |>
    httr::content("text") |>
    jsonlite::fromJSON(simplifyDataFrame = TRUE)
  # one level of error checking
  # default to battery if no data
  estimated_tide <- !is.null(response$error)
  if (estimated_tide) {
    print("No data. Defaulting to The Battery")
    station = battery
    url <- str_replace(url, "station=[0-9]+", paste0("station=", station))
    response <- httr::GET(url) |>
      httr::content("text") |>
      jsonlite::fromJSON(simplifyDataFrame = TRUE)
  }
  response$good_tide_station <- !estimated_tide
  return(response)
}

# get tide data for a specific period
get_tide_data_noaa <- function(station = "8518750", # the battery
                               begin_date = "2021-01-01",
                               end_date = "2021-01-01") {
  # for error handling
  max_attempts <- 3
  attempt <- 1

  begin_date <- gsub("-","",as.character(begin_date))
  end_date <- gsub("-","",as.character(end_date))
  # debug
  print(paste(begin_date,end_date,station))

  url <- paste0("https://tidesandcurrents.noaa.gov/api/prod/datagetter?",
                "begin_date=",begin_date,
                "&end_date=",end_date,
                "&station=",station,
                "&product=predictions",
                "&datum=MLLW",
                "&time_zone=lst_ldt&units=english&interval=hilo&format=json")

  while(attempt <= max_attempts) {
    tryCatch({
      response <- get_json_response(url)
      break
    }, error = function(e) {
      if (attempt == max_attempts) {
        stop("JSON parse failure", max_attempts, " attempts.")
      }
      print(paste("JSON parse Attempt", attempt, "failed. Retrying..."))
      attempt <- attempt + 1
    })
  }

    tides_noaa <- response$predictions |>
    set_names(c("datetime","tide_level","hi_lo")) |>
    mutate(date = as.Date(datetime),.before = datetime) |>
    mutate(datetime = ymd_hms(paste0(datetime,":00"),tz= "America/New_York")) |>
    mutate(tide_level = as.numeric(tide_level)) |>
    mutate(tide_noaa_id = station,.before = tide_level)
    tides_noaa$good_tide_station <- response$good_tide_station

  attr(tides_noaa,"station_id") <- station
  return(tides_noaa)

}

get_tide_data_noaa_year <- function(year=2011, station = "8518750"){
  print(paste(year, station))
  begin_date <- paste0(year,"-01-01")
  end_date <- paste0(year,"-12-31")
  return(get_tide_data_noaa(station = station,
                            begin_date = begin_date,
                            end_date = end_date))
}

# retrieve tide data for each combination of  observation date and station -----
# get tide data for each date and station in wq_data_2
# map() returns nothing if there is an error at any point.  Instead, this builds
# data frame one iteration at a time, allowing a manual restart from
# the last successful iteration. Change start_index to current value of n

# get tide stations needed
wq_data <- duckplyr_df_from_file("data/wq_data.parquet","read_parquet")
wq_meta_station_key <- duckplyr_df_from_file("data/wq_meta_station_key.parquet","read_parquet")

# load tide data retrieved previously
tides_noaa <- duckplyr_df_from_file("data/tides_noaa.parquet","read_parquet")

# get all needed combinations of stations and dates
needed_tides <- wq_meta_station_key %>%
  select(site_id,tide_noaa_id) %>%
  left_join(select(wq_data,site_id,date)) %>%
  select(tide_noaa_id,date) %>%
  distinct()

# only get dates needed not already in tide data
missing_tides <- needed_tides %>%
  anti_join(tides_noaa)


missing_tides

# test
new_tides <- get_tide_data_noaa(missing_tides$tide_noaa_id[1],
                                begin_date = missing_tides$date[1]-1,
                                end_date = missing_tides$date[1])

# retrieve tide data from NOAA for missing stations and dates
start_index = 1
new_tides <- tibble()
# we use a loop instead of map() so we can restart from the last successful iteration
for (n  in start_index:nrow(missing_tides)) {
# for (n  in 1:2) {
    # for() converts date to numeric
    print(missing_tides[n,])
    new_tides<- bind_rows(
      new_tides,
      get_tide_data_noaa(
        station = missing_tides$tide_noaa_id[n],
        begin_date = missing_tides$date[n]-1,
        end_date = missing_tides$date[n])
    )

}

# add tide_level range and phase duration so we can impute current later
new_tides <- new_tides |>
  arrange(tide_noaa_id,date) |>
  # positive tide range is incoming tide
  mutate(.by = tide_noaa_id,tide_range_ft = lead(tide_level) - tide_level) |>
  mutate(.by = tide_noaa_id,tide_duration_hrs = as.numeric(lead(datetime)-datetime)) |>
  mutate(.by = tide_noaa_id,tide_duration_hrs = ifelse(tide_duration_hrs > 8, NA, tide_duration_hrs))

# update existing data as needed
tides_noaa <- bind_rows(tides_noaa,new_tides) |>
  distinct()

# save as parquet file
arrow::write_parquet(tides_noaa,"data/tides_noaa.parquet")
