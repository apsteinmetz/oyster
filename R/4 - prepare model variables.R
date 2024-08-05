# prepare model variables
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

wq_data <- df_from_parquet("data/wq_data.parquet")

wq_data_select <- wq_data %>%
  select(bacteria,quality,site_id,date,precip_t0,precip_t1,precip_48)

wq_meta <- df_from_parquet("data/wq_meta.parquet")
# load station data
precip_stations <- df_from_parquet("data/ghcnd_stations_ny.parquet")

wq_meta_select <- wq_meta %>%
   select(
     site_id,
     site,
     water_body,
     neighborhood_town,
     nys_dec_water_body_classification,
     nyc_dep_wrrf_or_sewershed
   )
wq_meta_station_key <- df_from_parquet("data/wq_meta_station_key.parquet")

station_conflicts <-
  wq_meta_station_key %>%
  select(site, site_id, tide_noaa_id, precip_ghcn_id) |>
  left_join(
    select(
      wq_meta,
      site_id,
      water_body,
      borough_state,
      harmonic_noaa_tide_stations,
      noaa_rainfall_stations_id
    ),
    by = c("site_id")
  ) |>
  as_tibble() |>
  mutate(
    tides_agree = tide_noaa_id == harmonic_noaa_tide_stations,
    precip_agree = precip_ghcn_id == noaa_rainfall_stations_id
  )
station_conflicts

tide_data <- df_from_parquet("data/wq_tide_data.parquet") %>%
  mutate(.before=1,date = as.Date(sample_time)) %>%
  select(-sample_time,-tide_time,-good_tide_station)

temperature_data <- df_from_parquet("data/wq_temperature_data.parquet") %>%
  select(date,site_id,temperature_f)

precip_data <- df_from_parquet("data/wq_precip_data.parquet") %>%
  select(date,site_id,precip_in,precip_in_48)

model_data <- wq_data %>%
  left_join(wq_meta,by="site_id") %>%
  left_join(tide_data,by=c("site_id","date")) %>%
  left_join(temperature_data,by=c("site_id","date")) %>%
  left_join(precip_data,by=c("site_id","date")) %>%
  # na.omit() %>%
  # change chr to factor
  mutate(across(where(is.character),as.factor)) %>%
  # change site back to character
  mutate(site = as.character(site)) |>
  rename(bop_precip_t0 = precip_t0,
         bop_precip_t1 = precip_t1,
         bop_precip_48 = precip_48,
         ghcn_precip_in = precip_in,
         ghcn_precip_in_48 = precip_in_48)
model_data |> as_tibble()

# save model_data
write_parquet(model_data,"data/model_data.parquet")

# a lot of disagreement on precipitation
# look at LGA alone
model_data %>%
  filter(site_id == "15") |>
  ggplot(aes(x=ghcn_precip_in,y=bop_precip_t0)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Scatter plot of BOP vs GHCN precipitation")



