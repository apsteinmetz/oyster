# wrangle BOP spreadsheets and save as parquet files
library(tidyverse)
library(googlesheets4)
library(lubridate)
library(rvest)
library(arrow)
library(duckplyr)

# use duckdb for every tidyverse function
methods_overwrite()

# Get Water Quality Data from BOP ----------------------------------

googlesheets4::gs4_deauth()
wq_url <-
  "https://docs.google.com/spreadsheets/d/1813b2nagaxZ80xRfyMZNNKySZOitro5Nt7W4E9WNQDA/edit?usp=sharing"

wq_data_meta <- gs4_get(wq_url)
# take 400 rows of metadata to accomodate future growth in testing stations (up to 400 <grin>)
wq_meta_raw <- read_sheet(wq_url,"Information",range = "A10:J400")
wq_data_raw <- read_sheet(wq_url,"Data")

# Clean Data ----------------------------------

wq_meta <- wq_meta_raw %>%
  janitor::clean_names() |>
  rename("site" = 2) |>
  # remove empty rows
  filter(!is.na(site)) |>
  # why this comes in as a list of 1 is beyond me and one value is NULL
  # this is some complicated dplyr-fu.
  mutate(district_council_number =  unlist(map(district_council_number,~ifelse(is.null(.x),NA,.x)))) |>
  # some longitudes are erroneously positive
  mutate(longitude = if_else(longitude >0,-longitude,longitude)) |>
  mutate(currently_testing = if_else(is.na(currently_testing),0,1)) %>%
  mutate(currently_testing = as.logical(currently_testing))

data_names <- c("site","date","year","month","high_tide","sample_time","bacteria",
                "precip_t0","precip_t1","precip_t2","precip_t3","precip_t4",
                "precip_t5","precip_t6","notes")


# get average sample time and use that for NA sample times
sample_time_avg <- wq_data_raw$`Sample Time` |>
  mean(na.rm = TRUE) |>
  as.POSIXct()


wq_data <- wq_data_raw |>
  set_names(data_names) |>
  mutate(date = as_date(date)) %>%
  # change NA sample times to average time. Good idea?
  mutate(sample_time = if_else(is.na(sample_time),sample_time_avg,sample_time)) |>
  mutate(sample_time = hms::as_hms(sample_time)) %>%
  mutate(high_tide = hms::as_hms(high_tide)) %>%
  # add date to sample time
  mutate(sample_time = ymd_hms(paste(date,sample_time),tz= "America/New_York")) |>
  mutate(high_tide = ymd_hms(paste(date,high_tide),tz= "America/New_York")) |>
  mutate(across(where(is.list), as.character)) %>%
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "<10", "0"))) %>%
  # > 24196 test limit?
  mutate(across(where(is.character), .fns = ~ str_replace(.x, ">", ""))) %>%
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "Trace", "0"))) %>%
  # get rid of snow inches next to precip as water
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "\\(.+\\)", ""))) %>%
  mutate(across(where(is.character), .fns = ~ na_if(.x, "N/A"))) %>%
  mutate(across(contains("precip"), as.numeric)) %>%
  mutate(precip_wk = rowSums(select(., starts_with("precip")), na.rm = TRUE),.after="bacteria") |>
  mutate(bacteria = as.numeric(bacteria)) %>%
  mutate(notes = replace_na(notes, "")) %>%
  # fix some typos
  mutate(site = str_replace(site, "Daylighted Section", "daylighted section")) %>%
  mutate(site = str_replace(site, "Govenors", "Governors")) %>%
  mutate(quality = cut(
    bacteria,
    breaks = c(-1, 0, 35, 104, 49999),
    labels = c("Not Detected", "Good", "Fair", "Unacceptable")
  )) %>%
  mutate(site = as.factor(site))




# Save Data ----------------------------------
arrow::write_parquet(wq_data,"data/wq_data.parquet")
arrow::write_parquet(wq_meta,"data/wq_meta.parquet")
