# get weather data from NOAA

library(tidyverse)
library(purrr)
library(arrow)
library(duckplyr)
# library(sf)
library(progressr)

# devtools::install_github("ropensci/rnoaa")
#assumes NOAA_KEY is in .renvir
library(rnoaa)

DEBUG = TRUE

# get needed files
wq_data <- duckplyr_df_from_file("data/wq_data.parquet","read_parquet")
wq_meta_station_key <- duckplyr_df_from_file("data/wq_meta_station_key.parquet","read_parquet")
ghcn_stations <- duckplyr_df_from_file("data/ghcnd_stations_ny.parquet","read_parquet") %>%
  rename(ghcn_station_id = id)
weather_ghcn <- duckplyr_df_from_file("data/weather_ghcn.parquet","read_parquet")

# get neeeded stations
wq_meta_station_key


# get NYC area stations

# get station metadata from GHCN ---------------------------------------------------
# ghcnd_station_raw <- ghcnd_stations()
# save ghcnd_station_raw as parquet
#write_parquet(ghcnd_station_raw, "ghcnd_station_raw.parquet")
# read station metadata
# ghcnd_station_raw <- read_parquet("ghcnd_station_raw.parquet")
# ghcnd_stations <- ghcnd_station_raw %>%
#   filter(longitude > -75.5 & longitude < -73.5,
#          latitude > 40.5 & latitude < 41) |>
#   nest(elements = c(first_year,last_year,element))

#ghcnd_stations <- read_parquet("data/ghcnd_stations_ny.parquet") |>
#  filter(element == "TMAX" | element == "PRCP")
# functions to import weather data from GHCN ---------------------------------------------------
# default is Laguardia airport nyc
# central park is GHCND:USW00094728
central_park <- "USW00094728"
LGA <- "USW00014732"

get_weather <-  function(station = "USW00014732",
                         startdate = Sys.Date() - 365,
                         enddate = Sys.Date() - 1,
                         datatypeid = "TMAX") {
  if (DEBUG) print(startdate)
  weather <- ncdc(
    datasetid = 'GHCND',
    stationid = paste0("GHCND:", station),
    datatypeid=datatypeid,
    startdate = startdate,
    enddate = enddate,
    limit = 1000, # max is 1000
    add_units = TRUE
  )
  if (DEBUG) print(weather$data)
  return(weather$data)
}

get_weather_year <-  function(year=2020,
                              station = central_park,
                              datatypeid = datatypeids) {
  # print(year)
  startdate = paste0(year, "-01-01")
  enddate = paste0(year, "-12-31")
  weather <- ncdc(
    datasetid = 'GHCND',
    stationid = paste0("GHCND:", station),
    datatypeid = datatypeid,
    startdate = startdate,
    enddate = enddate,
    limit = 1000,
    add_units = TRUE
  )
  return(weather$data)
}

c_to_f <- function(temp) {
  return(temp *9/5 + 32)
}

# function to convert millimeters to inches
mm_to_in <- function(len) {
  return(len*.039)
}

fix_raw_weather <- function(weather_raw) {
  weather_raw %>%
    distinct() %>%
    transmute(date = as.Date(date),
              ghcn_station_id = str_remove(station,"GHCND:"),
              datatype,
              value) %>%
    pivot_wider(names_from = "datatype",
                values_from = "value") %>%
    left_join(ghcn_stations, by = "ghcn_station_id")  |>
    transmute(date,
              ghcn_station_id,
              ghcn_station_name = name,
              temperature_f = c_to_f((TMIN + TMAX) / 20),
              precip_in = mm_to_in(PRCP / 10)) |>
    distinct()

    # fill in missing days with previous day
    # end of November seems problematic
    # fill(station,temperature) |>
    # assume no rain if date is missing
    # mutate(precipitation = ifelse(is.na(precipitation),0,precipitation))
}

# Get full weather data set

datatypeids <- c("TMIN","TMAX")
years= 2011:year(Sys.Date())

# only ghcn stations which are nearest to at least one wq station
# refactor for precip
temp_stations <- ghcn_stations %>%
  filter(last_year == year(Sys.Date())) %>%
  filter(element == "TMAX") %>%
  mutate(temperature_ghcn_id = ghcn_station_id) %>%
  semi_join(wq_meta_station_key,by = "temperature_ghcn_id") %>%
  distinct()


# don't re-initialize when restarting stopped process
weather_raw <- tibble()
throttle <- 0.4 # seconds between calls. Tweak this to avoid rate limiting.
stations_to_get <- temp_stations # or prcp_stations
elements <- c("TMIN","TMAX") # or "PRCP"
oldest_year <- 2011
start_row = 1 # station
stations_to_get$name[start_row]
start_year = 2011
with_progress({
  p1 = progressor(along = start_row:nrow(stations_to_get))
  for (s in start_row:nrow(stations_to_get)) {
    startyear <- max(stations_to_get$first_years[s], oldest_year)
    # restart point
    if (start_row == s) startyear <- start_year
    endyear <- stations_to_get$last_year[s]
    station <- stations_to_get$ghcn_station_id[s]
    p2 = progressor(along = startyear:endyear)
    for (y in startyear:endyear) {
      weather_raw <- bind_rows(weather_raw,
                               get_weather_year(y, station, datatypeid = elements))
      Sys.sleep(throttle)
      p2(print(y))
    }
    p1(print(temp_stations$name[s]))
  }
})

temp_raw <- weather_raw

# MAIN retrieve weather data ---------------------------------------------------
# for each combination of observation date and station

# get all needed combinations of stations and dates
needed_temperature <- wq_meta_station_key %>%
  select(site_id,temperature_ghcn_id) %>%
  left_join(select(wq_data,site_id,date)) %>%
  select(temperature_ghcn_id,date) %>%
  distinct()|>
  rename(ghcn_station_id = temperature_ghcn_id) |>
  # omit dates that we know won't have data
  left_join(ghcn_stations,by ="ghcn_station_id") |>
  filter(year(date) >= first_year & year(date) <= last_year & element == "TMAX") |>
  distinct()

needed_temperature |> as_tibble()

needed_rain <- wq_meta_station_key %>%
  select(site_id,precip_ghcn_id) %>%
  left_join(select(wq_data,site_id,date)) %>%
  select(precip_ghcn_id,date) %>%
  distinct() |>
  rename(ghcn_station_id = precip_ghcn_id) |>
  # omit dates that we know won't have data
  left_join(ghcn_stations,by ="ghcn_station_id") |>
  filter(year(date) >= first_year & year(date) <= last_year & element == "PRCP") |>
  distinct()

# make sure we get 3 days of rain data for each wq observation date
needed_rain_lag1 <- needed_rain |>
  mutate(date = date - 1)

needed_rain_lag2 <- needed_rain |>
  mutate(date = date-2)

needed_rain <- bind_rows(needed_rain,
                         needed_rain_lag1,
                         needed_rain_lag2) |>
  distinct()

needed_both <- bind_rows(needed_temperature,needed_rain) |>
  distinct()

needed_both |> as_tibble()

missing_both <- needed_both |>
  anti_join(weather_ghcn,by = c("date","ghcn_station_id")) |>
  select(date,ghcn_station_id) %>%
  distinct() |>
  nest(station_ids = ghcn_station_id) %>%
  na.omit()

missing_both |> as_tibble()

# don't re-initialize when restarting stopped process
# weather_raw <- tibble()
throttle <- 0.4 # seconds between calls. Tweak this to avoid rate limiting.
start_row = 856
with_progress({
  p = progressor(along = start_row:nrow(missing_both))
  for (n in start_row:nrow(missing_both)) {
    date <- missing_both[n, ]$date
    stations <- unlist(missing_both[n, ]$station_ids)
    weather_raw <- bind_rows(
      weather_raw,
      get_weather(
        station = stations,
        # we need more than one day of precip data
        startdate = date-2,
        enddate = date,
        datatypeid = c("TMIN", "TMAX", "PRCP")
      )
    )
    Sys.sleep(throttle)
    p(print(n))
  }
})

weather_new <- fix_raw_weather(weather_raw)
weather_ghcn <- weather_new |>
  bind_rows(weather_ghcn) |>
  arrange(date) |>
  distinct()
weather_ghcn <- fix_raw_weather(weather_raw)
# save data as parquet
arrow::write_parquet(weather_ghcn,"data/weather_ghcn.parquet")
