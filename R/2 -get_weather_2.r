# get weather data from NOAA

library(dplyr)
library(lubridate)
library(purrr)
library(arrow)
library(sf)


# devtools::install_github("ropensci/rnoaa")
library(rnoaa)

#assumes NOAA_KEY is in .renvir

# get just an update or full history?
UPDATE <- FALSE

datatypeids <- c("TMIN","TMAX","PRCP")
years= 2011:year(Sys.Date())


# get NYC area stations

# get station metadata from GHCN ---------------------------------------------------
# ghcnd_station_raw <- ghcnd_stations()
# save ghcnd_station_raw as parquet
#write_parquet(ghcnd_station_raw, "ghcnd_station_raw.parquet")
# read station metadata
ghcnd_station_raw <- read_parquet("ghcnd_station_raw.parquet")
ghcnd_stations <- ghcnd_station_raw %>%
  filter(longitude > -75.5 & longitude < -73.5,
         latitude > 40.5 & latitude < 41) |>
  nest(elements = c(first_year,last_year,element))

# import weather data from GHCN ---------------------------------------------------
# default is Laguardia airport nyc
# central park is GHCND:USW00094728
central_park <- "USW00094728"
LGA <- "USW00014732"
get_weather <-  function(station = "USW00014732",
                         startdate = Sys.Date() - 365,
                         enddate = Sys.Date() - 1,
                         datatypeid = "TMAX") {
  weather <- ncdc(
    datasetid = 'GHCND',
    stationid = paste0("GHCND:", station),
    datatypeid=datatypeid,
    startdate = startdate,
    enddate = enddate,
    limit = 1000, # max is 1000
    add_units = TRUE
  )
  return(weather$data)
}

get_weather_year <-  function(year=2020,
                              station = central_park,
                              datatypeid = datatypeids) {
  print(year)
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

fix_raw_weather <- function(weather_raw) {
  weather_raw %>%
    transmute(date = as.Date(date),
              station = "Central Park",
              datatype,
              value) %>%
    pivot_wider(names_from = "datatype",
                values_from = "value") %>%
    transmute(date,
              station,
              temperature = c_to_f((TMIN + TMAX) / 20),
              precipitation = mm_to_in(PRCP / 10)) %>%
    # fill in missing days with previous day
    # end of November seems problematic
    fill(station,temperature) |>
    # assume no rain if date is missing
    mutate(precipitation = ifelse(is.na(precipitation),0,precipitation))
}

if (file.exists("data/weather.rdata")){
  load("data/weather.rdata")
} else {
  weather_raw <- tibble()
  # if this fails at a certain year keep weather_raw, set years to remaining
  # years and restart
  for (y in years) {
    # have to separate out because max request is 1000 rows
    weather_raw <- bind_rows(weather_raw, get_weather_year(y,datatypeid = c("TMIN","TMAX")))
    weather_raw <- bind_rows(weather_raw, get_weather_year(y,datatypeid = c("PRCP")))
    print(nrow(weather_raw))
  }
  weather <- weather_raw |>
    fix_raw_weather()
  save(weather,file="data/weather.rdata")
  arrow::write_parquet(weather,"data/weather.parquet")
  write_csv(weather,"data/weather.csv")
}

c_to_f <- function(temp) {
  return(temp *9/5 + 32)
}

# function to convert millimeters to inches
mm_to_in <- function(len) {
  return(len*.039)
}


if (UPDATE){
  start_date <- max(weather$date) + 1
  if (start_date+365 < Sys.Date()) {
    #its been a while since we updated. take batches of years.
    years <- seq(year(start_date),year(Sys.Date()))
    weather_update_raw <- map_dfr(years,get_weather_year,datatypeid = datatypeids)
  } else {
  weather_update_raw <- get_weather(datatypeid = datatypeids,startdate = start_date)
  weather_update <- fix_raw_weather(weather_update_raw)
  weather <- bind_rows(weather,weather_update) %>% distinct()
  }
  save(weather,file="data/weather.rdata")
  arrow::write_parquet(weather,"data/weather.parquet")
  write_csv(weather,"data/weather.csv")

}

