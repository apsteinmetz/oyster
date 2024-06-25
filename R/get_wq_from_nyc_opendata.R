# get NYC Open Data Water quality data
library(tidyverse)
library(lubridate)
library(RSocrata)
library(duckdb)

# use duckdb for every tidyverse function
methods_overwrite()
wq_data_url <- "https://data.cityofnewyork.us/resource/5uug-f49n.json"

# get the data
wq_data_nyc_raw <- read.socrata(wq_data_url) |>
  as_tibble()
# save file to parquet
arrow::write_parquet(wq_data_nyc_raw,"data/wq_data_nyc_raw.parquet")

wq_data_nyc <- wq_data_nyc_raw %>%
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

stations_metadata  <- wq_data_nyc |>
  select(sampling_location,sample_datetime,lat,long) |>
  remove_missing() |>
  group_by(sampling_location) |>
  slice_max(order_by = sample_datetime,n = 1) |>
  transmute(sampling_location,
            last_sample_date = sample_datetime,
            lat,
            long) |>
  # swap lat and long if lat is less  than long
  mutate(lat = if_else(lat < long, long, lat),
         long = if_else(lat < long, lat, long))

# count the number of NA entries in each column of wq_data_nyc
missing_data <- wq_data_nyc |>
  summarise_all(~sum(is.na(.))) |>
  gather() |>
  arrange(desc(value))

#  retain intersting columns with not too much missing data
# in most cases this means keeping sample "top" but not "bottom"
wq_data_nyc_subset <- wq_data_nyc |>
  transmute(sampling_location,
            sample_datetime,
            current_direction = current_direction_current_direction,
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

# how does fecal relate to enterococci?
wq_data_nyc_subset |>
  ggplot(aes(x = fecal_coliform_100ml, y = enterococci_100ml)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  # geom_smooth(method = "lm") +
  # annotate plot with r-squared value
  annotate("text", x = 1e2, y = 1e4,  color = "red",
           label = paste("R^2 = ",round(cor(wq_data_nyc_subset$fecal_coliform_100ml,
                                            wq_data_nyc_subset$enterococci_100ml,
                                            use = "complete.obs")^2,2))) +
  labs(title = "Fecal Coliform vs Enterococci",
       x = "Log Fecal Coliform (cells/100ml)",
       y = "Log Enterococci (cells/100ml)",
       caption = "Data from NYC Open Data")


# impute missing values.
# first group by date and take the mean of each column
# replace missing values with the mean of the column for that date
wq_data_nyc_imputed <- wq_data_nyc_subset |>
  mutate(date = as.Date(sample_datetime)) |>
  group_by(date) |>
  mutate(across(where(is.numeric), ~if_else(is.na(.), mean(., na.rm = TRUE), .)) ) |>
  ungroup()

arrow::write_parquet(wq_data_nyc,"data/wq_data_nyc.parquet")

