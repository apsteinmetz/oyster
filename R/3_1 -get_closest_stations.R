# get closest tide, rain and temperature stations to each water quality station
# and add it to the metadata

library(tidyverse)
# library(rnoaa)
library(rvest)
library(duckdb)
library(duckplyr)
methods_overwrite()
source("r/func_get_nearest.R")

# read the list of tide stations ---------------------------------------------------
tide_stations <- duckplyr_df_from_csv("data/tide_stations_nyc.csv")

# read the list of weather stations ---------------------------------------------------
# get station metadata from GHCND
# ghcnd_station_raw <- ghcnd_stations()
# save ghcnd_station_raw as parquet
# write_parquet(ghcnd_station_raw, "data/ghcnd_station_raw.parquet")
# read station metadata
ghcnd_station_raw <- read_parquet("data/ghcnd_station_raw.parquet")
# just take tri-state metro area
# and stations with recent data
ghcnd_stations <- ghcnd_station_raw %>%
  filter(longitude > -74.3 & longitude < -73.5,
         latitude > 40.5 & latitude < 41) |>
  filter(last_year == year(Sys.Date()))

precip_stations <- ghcnd_stations %>%
  filter(element =="PRCP")

temperature_stations <- ghcnd_stations %>%
  filter(element =="TMAX")

# we have all the station ids, types and locations
# assign to metadata
wq_meta <- duckplyr_df_from_file("data/wq_meta.parquet","read_parquet")

# get closest tide, rain and temperature stations to each water quality station
# and add it to the metadata

stations_to_query  <- tide_stations
wq_meta_2 <- wq_meta %>%
  mutate(tide_station_noaa = get_nearest_id_to_wq_station(latitude, longitude))

stations_to_query  <- temperature_stations
wq_meta_2 <- wq_meta_2 %>%
  mutate(temperature_station_ghcn = get_nearest_id_to_wq_station(latitude, longitude))

stations_to_query  <- precip_stations
wq_meta_2 <- wq_meta_2 %>%
  mutate(precip_station_ghcn = get_nearest_id_to_wq_station(latitude, longitude))

wq_meta_station_key <- wq_meta_2 %>%
  select(site,site_id,latitude,longitude,tide_station_noaa,temperature_station_ghcn,precip_station_ghcn) |>
  distinct() |>
  left_join(transmute(tide_stations,
                      tide_station_noaa = id,
                      tide_name = name,
                      tide_latitude = latitude,
                      tide_ongitude=longitude),
            by = "tide_station_noaa") |>
  left_join(transmute(precip_stations,
                      precip_station_ghcn = id,
                      precip_name = name,
                      precip_latitude = latitude,
                      precip_longitude=longitude),
            by = "precip_station_ghcn") |>
  left_join(transmute(temperature_stations,
                      temperature_station_ghcn = id,
                      temperature_name = name,
                      temperature_latitude = latitude,
                      temperature_longitude=longitude),
            by = "temperature_station_ghcn")



# make a key table for the stations
tide_stations %>%
  transmute(tide_station_noaa = id,tide_name = name,tide_latitude = latitude,tide_ongitude=longitude) |>
  right_join(wq_meta_station_key,by = "tide_station_noaa")
