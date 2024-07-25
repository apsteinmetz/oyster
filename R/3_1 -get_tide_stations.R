# get closest tide station
library(tidyverse)
# library(rnoaa)
library(rvest)
library(duckdb)
library(duckplyr)

methods_overwrite()
battery <- "8518750"
# uses javascript on the fly so save this page as html and read that
# https://tidesandcurrents.noaa.gov/tide_predictions.html?gid=1407#listing

tide_station_path <- "data/Tide_Stations_NY_Harbor.html"
tide_stations_raw <- rvest::read_html(tide_station_path)



tide_station_url <- "https://tidesandcurrents.noaa.gov/tide_predictions.html?gid=1407#listing"
# maybe try rselenium or v8
# not implemented. This is a javascript page
# Instead I did a manual copy paste of the table and saved it in an html file
# tide_stations_xml <- rvest::read_html(tide_station_url)
#
# js <- tide_stations_xml |>
#   html_elements("script") |>
#   html_text() |>
#   pluck(9)
#
# sb <- V8::new_context()
#
# sb$eval(js)


# extract table from tide station page
tide_stations <- tide_stations_raw %>%
  rvest::html_elements("table") %>%
  rvest::html_table() |>
  pluck(1) |>
  # make a separate column for names where Id is an empty string
  mutate(area = if_else(Id == "", Name, NA),.before = Name) |>
  # if area is empty, fill with previous area
  fill(area) |>
  # remove rows where Id is empty
  filter(Id != "") |>
  rename(id = Id, name = Name, latitude = Lat, longitude = Lon)


write_csv(tide_stations, "data/tide_stations_nyc.csv")

