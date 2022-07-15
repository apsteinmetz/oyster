# get weather data from NOAA

library(tidyverse)
library(purrr)
library(rnoaa)

#assumes NOAA_KEY is in .renvir

years = 2011:2022
datatypeids <- c("TMIN","TMAX","PRCP")

ncdc_datasets(stationid = "GHCND:USW00014732")

# default is Laguardia airport nyc
get_weather <-  function(station = "USW00014732",
                         startdate = Sys.Date() - 365,
                         enddate = Sys.Date() - 1,
                         datatypid = "TMAX") {
  weather <- ncdc(
    datasetid = 'GHCND',
    stationid = paste0("GHCND:", station),
    datatypeid=datatypid,
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

test <-function(year=1900,datatypeid="bbb"){
  return(tibble(year=seq(year,year+10),dataid = datatypeid))
}

years = 2011:2022
datatypeids <- c("TMIN","TMAX","PRCP")

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

weather <- weather_raw %>%
  transmute(date = as.Date(date),
            station = "LGA",
            datatype,
            value) %>%
  pivot_wider(names_from = "datatype",
              values_from = "value") %>%
  transmute(date,station,
            TEMP = c_to_f((TMIN+TMAX)/20),
            PRCP=mm_to_in(PRCP/100))

save(weather,file="data/weather.rdata")
