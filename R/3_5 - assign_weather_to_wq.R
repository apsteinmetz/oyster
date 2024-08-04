# get tide data from noaa ----------------------------------
library(tidyverse)
library(lubridate)
library(rvest)
library(duckdb)
library(duckplyr)
library(arrow)
library(progressr)

# use duckdb for every tidyverse function
methods_overwrite()

weather <- duckplyr_df_from_parquet("data/weather_ghcn.parquet") %>%
  rename(precip_in = precipitation) %>%
  # to get 2-day total precip we don't take current day precip
  # because most wq samples are taken early in the day
  mutate(precip_in_48 = lag(precip_in,1) +
           lag(precip_in,2))

wq_weather_key <- df_from_parquet("data/wq_meta_station_key.parquet") %>%
  select(site_id,precip_ghcn_id,temperature_ghcn_id) %>%
  na.omit()

wq_data_times <- df_from_parquet("data/wq_data.parquet") %>%
  select(site,site_id,sample_time) %>%
  left_join(wq_weather_key, by = "site_id") %>%
  mutate(date = as.Date(sample_time)) %>%
  na.omit()

precip <- weather %>%
  transmute(date,
            precip_ghcn_id = ghcn_station_id,
            precip_in,
            precip_in_48) %>%
  right_join(wq_data_times, by = c("date","precip_ghcn_id"))

# if no data for that date and station take mean of
# precip on that date for all stations reporting
precip_adj <- precip %>%
  mutate(.by= "date",precip_in = ifelse(is.na(precip_in),
                                        mean(precip_in,na.rm = TRUE),
                                        precip_in)) %>%
  mutate(.by= "date",precip_in_48 = ifelse(is.na(precip_in_48),
                                        mean(precip_in_48,na.rm = TRUE),
                                        precip_in_48))
# if still no data, use Central Park
default_station = "USW00094728"
missing_precip <- precip_adj %>%
  filter(is.na(precip_in)) %>%
  mutate(precip_ghcn_id = default_station) %>%
  select(-precip_in) %>%
  left_join(transmute(weather,
                      date,
                      precip_ghcn_id = ghcn_station_id,
                      precip_in = precipitation),
                      by = c("date","precip_ghcn_id"))


temperature <- weather %>%
  transmute(date,
            temperature_ghcn_id = ghcn_station_id,
            temperature_f = temperature) %>%
  right_join(wq_data_times, by = c("date","temperature_ghcn_id")) %>%
  select(-sample_time,-precip_ghcn_id)

# if no temp on that date default to Central Park
default_station = "USW00094728"
missing_temps <- temperature %>%
  filter(is.na(temperature_f)) %>%
  mutate(temperature_ghcn_id = default_station) %>%
  select(-temperature_f) %>%
  left_join(transmute(weather,
                      date,
                      temperature_ghcn_id = ghcn_station_id,
                      temperature_f = temperature),
                      by = c("date","temperature_ghcn_id"))


temperature_adj <- temperature %>%
  filter(!is.na(temperature_f)) %>%
  bind_rows(missing_temps)

# write to parquet
arrow::write_parquet(precip,"data/wq_precip_data.parquet")
arrow::write_parquet(temperature_adj,"data/wq_temperature_data.parquet")
