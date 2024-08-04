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

wq_data <- df_from_parquet("data/wq_data.parquet") %>%
  select(bacteria,quality,site_id,date,precip_t0,precip_t1,precip_48)

wq_meta <- df_from_parquet("data/wq_meta.parquet") %>%
   select(
     site_id,
     site,
     water_body,
     neighborhood_town,
     nys_dec_water_body_classification,
     nyc_dep_wrrf_or_sewershed
   )

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
  mutate(site = as.character(site))
model_data

# save model_data
write_parquet(model_data,"data/model_data.parquet")

# a lot of disagreement on precipitation
model_data %>%
  ggplot(aes(x=precip_in_48,y=precip_48)) +
  geom_point() +
  labs(title="Scatter plot of precip_in vs precip_in_48",
       x="ghc Precipitation in 48 hours (in)",
       y="bop Precipitation in 48 hours (in)")



# scatter plot of precip_t1 vs precip_in
model_data %>%
  ggplot(aes(x=precip_t0,y=precip_in)) +
  geom_point() +
  labs(title="Scatter plot of precip_t1 vs precip_in",
       x="Precipitation BOP",
       y="Precipitation ghcn")
