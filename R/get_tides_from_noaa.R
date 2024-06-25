# get tide data from noaa ----------------------------------
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
    mutate(station_id = station,.before = tide_level)
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
wq_data_2 <- duckplyr_df_from_file("data/wq_data_2.parquet","read_parquet")
wq_data <- duckplyr_df_from_file("data/wq_data.parquet","read_parquet")

needed_data <- wq_data_2 |>
  select(date,closest_tide_Id,date) |>
  filter(!is.na(closest_tide_Id)) |>
  distinct() |>
  arrange(date)

#test
tides_noaa <- get_tide_data_noaa(wq_data_2$closest_tide_Id[1],begin_date = wq_data_2$date[1]-1,end_date = wq_data_2$date[1])

start_index = 1
tides_noaa <- tibble()
for (n  in start_index:nrow(needed_data)) {
    # for() converts date to numeric
    print(paste(needed_data$closest_tide_Id[n],needed_data$date[n]))
    tides_noaa <- bind_rows(
      tides_noaa,
      get_tide_data_noaa(
        station = needed_data$closest_tide_Id[n],
        end_date = needed_data$date[n],
        begin_date = needed_data$date[n] - 1
      )
    )
}

tides_noaa <- tides_noaa |> distinct()

# or get full years.  More days but faster than single dates
needed_stations <- needed_data$closest_tide_Id |> unique()
start_index = 6
# tides_noaa <- tibble()
for (n  in  start_index:length(needed_stations)) {
  for(yr in 2011:2024){
    print(paste(n,needed_stations[n],yr))
    tides_noaa <- bind_rows(
      tides_noaa,
      get_tide_data_noaa_year(year = yr,station = needed_stations[n])
    )
  }
}
tides_noaa <- tides_noaa |> distinct()


# add tide_level range and phase duration so we can impute current later
tides_noaa <- tides_noaa |>
  arrange(station_id,date) |>
  group_by(station_id) |>
  # positive tide range is incoming tide
  mutate(tide_range_ft = lead(tide_level) - tide_level) |>
  mutate(tide_duration_hrs = as.numeric(lead(datetime)-datetime)) |>
  mutate(tide_duration_hrs = ifelse(tide_duration_hrs > 8, NA, tide_duration_hrs))
# save as parquet file
# df_to_parquet(tides_noaa,"data/tides_noaa.parquet")
arrow::write_parquet(tides_noaa,"data/tides_noaa.parquet")



# select just the needed days and previous day
tides_noaa_sm <- needed_data |>
  mutate(date = date - 1) |>
  bind_rows(needed_data) |>
  arrange(date) |>
  rename(station_id = closest_tide_Id) |>
  inner_join(tides_noaa,by = c("date","station_id"))

arrow::write_parquet(tides_noaa_sm,"data/tides_noaa_sm.parquet")
