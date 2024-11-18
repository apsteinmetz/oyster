# get weather data from NOAA

library(tidyverse)
library(purrr)

# devtools::install_github("ropensci/rnoaa")
library(rnoaa)

#assumes NOAA_KEY is in .renvir

# get just an update or full history?
UPDATE <- TRUE

years = 2011:2022 # not relevant with update=TRUE
datatypeids <- c("TMIN","TMAX","PRCP")

ncdc_datasets(stationid = "GHCND:USW00014732")

# default is Laguardia airport nyc
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
    limit = 1000,
    add_units = TRUE
  )
  return(weather$data)
}

get_weather_year <-  function(year=2020,
                              station = "USW00014732",
                              datatypeid = "TMAX") {
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


noaa_datatypes <- ncdc_datatypes(datasetid = "GHCND",
                                 stationid = "GHCND:USW00094728",
                                 limit = 200)$data %>% as_tibble()


if (!file.exists("data/weather_raw.rdata")){
  # grab vector of data types and years from NOAA weather data
  # API limits require bite-size grabs
  weather_raw <- map_dfr(datatypeids,
                         function(d)map_dfr(years,
                                            function(y) get_weather_year(y,datatypeid = d)))
  save(weather_raw,file="data/weather_raw.rdata")
} else load("data/weather_raw.rdata")


c_to_f <- function(temp) {
  return(temp *9/5 + 32)
}

mm_to_in <- function(len) {
  return(len*.039)
}

fix_raw_weather <- function(weather_raw) {
  weather_raw %>%
    transmute(date = as.Date(date),
              station = "LGA",
              datatype,
              value) %>%
    pivot_wider(names_from = "datatype",
                values_from = "value") %>%
    transmute(date,
              station,
              TEMP = c_to_f((TMIN + TMAX) / 20),
              PRCP = mm_to_in(PRCP / 100))
}

weather <- fix_raw_weather(weather_raw)

if (UPDATE){
  start_date <- max(weather$date) + 1
  weather_update_raw <- map_dfr(datatypeids,
                         function(d) get_weather(datatypeid = d,startdate = start_date))
  weather_update <- fix_raw_weather(weather_update_raw)
  weather <- bind_rows(weather,weather_update) %>% unique()
}

save(weather,file="data/weather.rdata")
# ------------------------------------------------------------------------
# get precip only for NYC DEP water data
years = 1986:2024
# precip_nyc <- tibble()
for (y in years){
  precip_nyc <- bind_rows(precip_nyc,get_weather_year(y,datatypeid = "PRCP"))
}
save(precip_nyc,file="data/precip_nyc.rdata")

