# get NYC Open Data Water quality data from NY DEP
library(tidyverse)
library(lubridate)
library(RSocrata)
library(duckdb)

# use duckdb for every tidyverse function
methods_overwrite()
wq_data_url <- "https://data.cityofnewyork.us/resource/5uug-f49n.json"

# get the data
wq_data_dep_raw <- read.socrata(wq_data_url) |>
  as_tibble()
# save file to parquet
arrow::write_parquet(wq_data_dep_raw,"data/wq_data_dep_raw.parquet")
wq_data_dep_raw <- arrow::read_parquet("data/wq_data_dep_raw.parquet")

wq_data_dep <- wq_data_dep_raw %>%
  # change NA sample times to noon
  mutate(sample_time = if_else(is.na(sample_time),"12:00",sample_time)) |>
  mutate(sample_time = if_else(is.na(sample_time),"12:00",sample_time)) |>
  #combine sample_date and sample_time and change AM/PM to 24 hour time
  mutate(sample_datetime = parse_date_time(paste(sample_date,sample_time),
                                       orders = c("%Y-%m-%d %I:%M %p",
                                                  "%Y-%m-%d %H:%M")),
                                       .before = sample_date) |>
# if sample_datetime is NA, use sample_date and sample_time set to noon
  mutate(sample_datetime = if_else(is.na(sample_datetime),
                                   parse_date_time(paste(sample_date,"12:00"),
                                                   orders = c("%Y-%m-%d %H:%M")),
                                   sample_datetime)) |>
# set time zone of sampe_datetime to America/New_York
mutate(sample_datetime = with_tz(sample_datetime,tz = "America/New_York"))

# get station metadata
stations_metadata_last <- wq_data_dep |>
  select(sampling_location,sample_datetime) |>
  distinct() |>
  group_by(sampling_location) |>
  slice_max(order_by = sample_datetime,n = 1) |>
  transmute(sampling_location, last_sample_date = sample_datetime)

stations_metadata_first <- wq_data_dep |>
  select(sampling_location,sample_datetime) |>
  distinct() |>
  group_by(sampling_location) |>
  slice_min(order_by = sample_datetime,n = 1) |>
  transmute(sampling_location, first_sample_date = sample_datetime)

stations_metadata_loc <- wq_data_dep |>
  drop_na(lat,long) |>
  select(sampling_location,sample_datetime,lat,long) |>
  group_by(sampling_location) |>
  slice_max(order_by = sample_datetime,n = 1) |>
  transmute(sampling_location, lat, long) |>
  # swap lat and long if lat is less  than long
  mutate(lat = if_else(lat < long, long, lat),
         long = if_else(lat < long, lat, long))

stations_metadata <- stations_metadata_last |>
  left_join(stations_metadata_first,by = "sampling_location") |>
  left_join(stations_metadata_loc,by = "sampling_location")

dep_station_names <- read_csv("data/dep_station_names.csv")
dep_station_names <- dep_station_names |>
  separate(Station,into = c("sampling_location","station_name"),sep = " ",extra = "merge")

stations_metadata <- stations_metadata |>
  left_join(dep_station_names,by = "sampling_location")


arrow::write_parquet(stations_metadata,"data/wq_meta_dep.parquet")

# count the number of NA entries in each column of wq_data_dep
missing_data <- wq_data_dep |>
  summarise_all(~sum(is.na(.))) |>
  gather() |>
  arrange(desc(value))

#  retain intersting columns with not too much missing data
# in most cases this means keeping sample labeled "top" but not "bottom"
wq_data_dep_subset <- wq_data_dep |>
  transmute(sampling_location,
            sample_datetime,
            current_direction = current_direction_current_direction,
            current_speed_knot,
            dissolved_oxygen_ml_l = as.numeric(winkler_method_top_dissolved_oxygen_mg_l),
            sample_temperature_c = as.numeric(top_sample_temperature_c),
            salinity_psu = as.numeric(top_salinity_psu),
            ph = as.numeric(top_ph),
            nutrient_nitrate_mg_l =  as.numeric(top_nitrate_nitrite_mg_l),
            nutrient_phosphorus_mg_l = as.numeric(total_phosphorus_mg_l),
            nutrient_ammonium_mg_l = as.numeric(top_ammonium_mg_l),
            enterococci_100ml = as.numeric(top_enterococci_bacteria_cells_100ml),
            fecal_coliform_100ml = as.numeric(top_fecal_coliform_bacteria_cells_100ml)) |>
    # we need at least coliform
  drop_na(fecal_coliform_100ml)

# impute missing values.
# first group by date and take the mean of each column on that date
# replace missing values with the mean of the column for that date
# IS THIS GOOD PRACTICE? I DON'T KNOW.
wq_data_dep_imputed <- wq_data_dep_subset |>
  mutate(date = as.Date(sample_datetime)) |>
  group_by(date) |>
  mutate(across(where(is.numeric), ~if_else(is.na(.), mean(., na.rm = TRUE), .)) ) |>
  ungroup() |>
  left_join(stations_metadata,by = "sampling_location")

arrow::write_parquet(wq_data_dep,"data/wq_data_dep.parquet")

dep_station_names <- dep_station_names |>
  separate(Station,into = c("station_id","station_name"),sep = " ",extra = "merge")


# function to convert compass degrees to one of 16 compass points
compass_point_16 <- function(degrees) {
  if (is.na(degrees)) {
    return(NA)
  }
  if (degrees < 11.25) {
    return("N")
  }
  if (degrees < 33.75) {
    return("NNE")
  }
  if (degrees < 56.25) {
    return("NE")
  }
  if (degrees < 78.75) {
    return("ENE")
  }
  if (degrees < 101.25) {
    return("E")
  }
  if (degrees < 123.75) {
    return("ESE")
  }
  if (degrees < 146.25) {
    return("SE")
  }
  if (degrees < 168.75) {
    return("SSE")
  }
  if (degrees < 191.25) {
    return("S")
  }
  if (degrees < 213.75) {
    return("SSW")
  }
  if (degrees < 236.25) {
    return("SW")
  }
  if (degrees < 258.75) {
    return("WSW")
  }
  if (degrees < 281.25) {
    return("W")
  }
  if (degrees < 303.75) {
    return("WNW")
  }
  if (degrees < 326.25) {
    return("NW")
  }
  if (degrees < 348.75) {
    return("NNW")
  }
  return("N")
}
