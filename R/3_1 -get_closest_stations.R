# get closest tide, rain and temperature stations to each water quality station
# and add it to the metadata

library(tidyverse)
# library(rnoaa)
library(rvest)
library(duckdb)
library(duckplyr)
library(arrow)
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
#ghcnd_stations <- ghcnd_station_raw %>%
#  filter(longitude > -74.3 & longitude < -73.5,
#         latitude > 40.5 & latitude < 41)
#write_parquet(ghcnd_stations, "data/ghcnd_stations_ny.parquet")
ghcnd_stations <- read_parquet("data/ghcnd_stations_ny.parquet")

# use only currently reporting stations
gghcnd_stations <- ghcnd_stations |>
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
  mutate(nearest = get_nearest_v(latitude, longitude,include_dist = TRUE,label = "tide_noaa")) |>
  unnest(nearest) |> unnest(nearest)

stations_to_query  <- temperature_stations
wq_meta_2 <- wq_meta_2 %>%
  mutate(nearest = get_nearest_v(latitude, longitude,include_dist = TRUE,label = "temperature_ghcn")) |>
  unnest(nearest) |> unnest(nearest)

stations_to_query  <- precip_stations
wq_meta_2 <- wq_meta_2 %>%
  mutate(nearest = get_nearest_v(latitude, longitude,include_dist = TRUE,label = "precip_ghcn")) |>
  unnest(nearest) |> unnest(nearest)

wq_meta_station_key <- wq_meta_2 %>%
  select(site,site_id,starts_with("tide"),starts_with("temperature"),starts_with("precip"))

# save to parquet
write_parquet(wq_meta_station_key,"data/wq_meta_station_key.parquet")
# write to a google sheet
library(googlesheets4)
googlesheets4::gs4_deauth()
googlesheets4::gs4_auth(scopes = "spreadsheets")
write_sheet(wq_meta_station_key,"wq_meta_station_key")

