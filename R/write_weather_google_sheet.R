# make weather spreadsheet
library(tidyverse)
library(duckplyr)
library(googlesheets4)
library(googledrive)

tide_data <- df_from_parquet("data/tides_noaa.parquet") |>
  select(-good_tide_station) |>
  as_tibble()

weather_data <- df_from_parquet("data/weather_ghcn.parquet") |> as_tibble()

## done once
# ss <- gs4_create("weather_data_nyc")

# save each data frame as a separate worksheet in a single google spreadsheet
ss <- drive_get("weather_data_nyc")
sheet_write(tide_data,ss,"tide_data")
sheet_write(weather_data,ss,"weather_data")

