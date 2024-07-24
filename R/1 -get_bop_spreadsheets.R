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
# assumes row 10 is column names, column A is site ID which is duplicated in column D
wq_meta_raw <- read_sheet(wq_url,"Information",range = "B10:BA400")
wq_data_raw <- read_sheet(wq_url,"Data")


water_body_classifications <- read_csv("data/NYDEC_water_classifications.csv",col_types = "fcc")
water_body_classifications


# Clean Meta Station Data ----------------------------------

wq_meta <- wq_meta_raw %>%
  # remove empty columns by selecting only columns where names starts with a letter
  select(!matches("^\\.")) |>
  janitor::clean_names() |>
  rename("site" = 2) |>
  # remove empty rows
  filter(!is.na(site)) |>
  mutate(site_id = as.factor(site_id)) |>
  # why this comes in as a list of 1 is beyond me and one value is NULL
  # this is some complicated dplyr-fu.
  mutate(district_council_number =  as.factor(unlist(map(district_council_number,~ifelse(is.null(.x),NA,.x))))) |>
  # FIXED as of Aug 2024  some longitudes are erroneously positive
  mutate(longitude = if_else(longitude >0,-longitude,longitude)) |>
  mutate(currently_testing = as.logical(if_else(is.na(currently_testing),0,1))) |>
  rename_with(~"nyc_dep_wrrf_or_sewershed",starts_with("associated")) |>
  # make NA entries in character columns actual NA so there is only one kind of NA
  mutate(across(where(is.character),\(x) if_else(x == "NA",NA,x))) |>
  # character as factor except notes
  mutate(across(where(is.character),as.factor)) |>
  mutate(notes = as.character(notes))


# add all levels of water body classifications to the meta data
wq_meta$nys_dec_water_body_classification <-
  fct_expand(wq_meta$nys_dec_water_body_classification,levels(NYDEC_water_classifications$water_body_class))
# reorder factor levels
wq_meta$nys_dec_water_body_classification <-
  fct_relevel(wq_meta$nys_dec_water_body_classification,levels(NYDEC_water_classifications$water_body_class))

# get all levels of lab_analysis columns
labs <- wq_meta |> select(contains("lab_analysis")) |>
  pivot_longer(cols = everything(),names_to = "lab_analysis",values_to = "value") |>
  pull(value) |>
  unique()

wq_meta <- wq_meta |>
  mutate(across(contains("lab_analysis"),
                \(x) fct_expand(x,levels(labs),after=0)))

# convert all N/As in factor columns to a "missing" level
# This will let us keep rows with NA in the models but all missing will have the same value
# good or bad?
wq_meta <- wq_meta |>
  mutate(across(where(is.factor), ~ fct_na_value_to_level(.x,level = "missing")))

# Clean Individual Data --------------------------------------------------------


data_names <- c("site","site_id","date","year","month","high_tide","sample_time","bacteria",
                "precip_t0","precip_t1","precip_t2","precip_t3","precip_t4",
                "precip_t5","precip_t6","notes")



# get average sample time and use that for NA sample times
sample_time_avg <- wq_data_raw$`Sample Time` |>
  mean(na.rm = TRUE) |>
  as.POSIXct()

# function to extract day of week from date
day_of_week <- function(x) {
  x |>
    lubridate::wday(week_start = 7) |>
    factor(levels = 1:7,labels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
}

wq_data <- wq_data_raw |>
  set_names(data_names) |>
  mutate(date = as_date(date)) %>%
  # change NA sample times to average time. Good idea?
  mutate(sample_time = if_else(is.na(sample_time),sample_time_avg,sample_time)) |>
  mutate(sample_time = hms::as_hms(sample_time)) %>%
  mutate(sample_day = day_of_week(date),.before = bacteria) |>
  mutate(high_tide = hms::as_hms(high_tide)) %>%
  # add date to sample time
  mutate(sample_time = ymd_hms(paste(date,sample_time),tz= "America/New_York")) |>
  mutate(high_tide = ymd_hms(paste(date,high_tide),tz= "America/New_York")) |>
  mutate(across(where(is.list), as.character)) %>%
  # We chose to make "Trace" and "<10" into zero.
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "<10", "0"))) %>%
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "Trace", "0"))) %>%
  # > 24196 test limit?  This value is so far out of the range of the other values
  # that it might as well be infinity.
  mutate(across(where(is.character), .fns = ~ str_replace(.x, ">", ""))) %>%
  # get rid of snow inches next to precip as water
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "\\(.+\\)", ""))) %>%
  mutate(across(where(is.character), .fns = ~ na_if(.x, "N/A"))) %>%
  mutate(across(contains("precip"), as.numeric)) %>%
  # since observation time is typically in the morning don't include the current day's precip
  mutate(precip_48 = rowSums(select(., precip_t1,precip_t2), na.rm = TRUE),.after="bacteria") |>
  mutate(bacteria = as.numeric(bacteria)) %>%
  mutate(notes = replace_na(notes, "")) %>%
  # fix some typos
  mutate(site = str_replace(site, "Daylighted Section", "daylighted section")) %>%
  mutate(site = str_replace(site, "Govenors", "Governors")) %>%
  mutate(quality = as_factor(cut(
      bacteria,
      breaks = c(-1, 34, 104, Inf),
      labels = c("Safe","Caution", "Unsafe"))
      )
    ) %>%
  mutate(site = as.factor(site))




# Save Data ----------------------------------
arrow::write_parquet(wq_data,"data/wq_data.parquet")
arrow::write_parquet(wq_meta,"data/wq_meta.parquet")
