# get NYC Open Data Water quality data from NY DEP
library(tidyverse)
library(lubridate)
library(RSocrata)
library(arrow)

# Which Bacteria Should You Monitor?
#
# Which bacteria you test for depends on what you want to know. Do you want to
# know whether swimming in your stream poses a health risk? Do you want to know
# whether your stream is meeting state water quality standards?
#
# Studies conducted by EPA to determine the correlation between different
# bacterial indicators and the occurrence of digestive system illness at
# swimming beaches suggest that the best indicators of health risk from
# recreational water contact in fresh water are E. coli and enterococci. For
# salt water, enterococci are the best. Interestingly, fecal coliforms as a
# group were determined to be a poor indicator of the risk of digestive system
# illness. However, many states continue to use fecal coliforms as their primary
# health risk indicator.
#
# If your state is still using total or fecal coliforms as the indicator
# bacteria and you want to know whether the water meets state water quality
# standards, you should monitor fecal coliforms. However, if you want to know
# the health risk from recreational water contact, the results of EPA studies
# suggest that you should consider switching to the E. coli or enterococci
# method for testing fresh water. In any case, it is best to consult with the
# water quality division of your state's environmental agency, especially if you
# expect them to use your data.
#
# https://archive.epa.gov/water/archive/web/html/vms511.html

wq_data_url <- "https://data.cityofnewyork.us/resource/5uug-f49n.json"

# vector of 16 compass points
compass_points <- c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")

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

# function to parse current direction values to compass points
parse_current_direction <- Vectorize(function(direction){
  if(!is.na(as.numeric(direction))){
    return(compass_point_16(as.numeric(direction)))
  } else {
    return(direction)
  }
})

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

# get station metadata ---------------------------------------------------------
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


#  retain intersting columns with not too much missing data --------------------
# count the number of NA entries in each column of wq_data_dep
missing_data <- wq_data_dep |>
  summarise_all(~sum(is.na(.))) |>
  pivot_longer(cols = everything(),names_to = "variable",values_to = "num_missing") |>
  arrange(num_missing)
View(missing_data)

# in most cases this means keeping sample labeled "top" but not "bottom"
wq_data_dep_subset <- wq_data_dep |>
  # we need at least coliform
  drop_na(top_fecal_coliform_bacteria_cells_100ml) |>
  group_by(sampling_location,sample_datetime) |>
  # filter(year(sample_datetime) > year(Sys.Date()) - 10) |>
  transmute(sampling_location,
            sample_datetime,
            current_direction = current_direction_current_direction,
            current_speed_knot,
            oxygen_ml_l = max(as.numeric(winkler_method_top_dissolved_oxygen_mg_l),
                              as.numeric(winkler_method_bottom_dissolved_oxygen_mg_l),
                                        na.rm = TRUE),
            oxygen_ml_l = if_else(is.infinite(oxygen_ml_l),NA,oxygen_ml_l),
            sample_temperature_c = as.numeric(top_sample_temperature_c),
            salinity_psu = as.numeric(top_salinity_psu),
            ph = as.numeric(top_ph),
            nutrient_nitrate_mg_l =  as.numeric(top_nitrate_nitrite_mg_l),
            nutrient_phosphorus_mg_l = as.numeric(total_phosphorus_mg_l),
            nutrient_ammonium_mg_l = as.numeric(top_ammonium_mg_l),
            bacteria_enterococci_100ml = as.numeric(top_enterococci_bacteria_cells_100ml),
            bacteria_fecal_coliform_100ml = as.numeric(top_fecal_coliform_bacteria_cells_100ml))

# clean up current speed and direction.
# convert speed to numeric and direction to compass point
wq_data_dep_subset <- wq_data_dep_subset |>
  ungroup() |>
  # some are swapped
  mutate(current_speed_knot = if_else(current_speed_knot %in% compass_points,
                                      current_direction, current_speed_knot),
         current_direction = if_else(current_speed_knot %in% compass_points,
                                     current_speed_knot,current_direction)) |>
  # direction and speed are sometimes in the same column
  mutate(current_speed_knot = if_else(str_detect(current_direction,"-[0-9\\.]+"),
                                      str_extract(current_direction,"[0-9\\.]+"),
                                      current_speed_knot)) |>
  mutate(current_direction = str_extract(current_direction,"^[A-Za-z]+")) |>
  mutate(current_direction = toupper(parse_current_direction(current_direction))) |>
  # weak, ebb, slack and other characters all mean zero speed
  mutate(current_speed_knot = as.numeric(current_speed_knot)) |>
  mutate(current_speed_knot = if_else(is.na(current_speed_knot),0,
                                      current_speed_knot)) |>
  # change directions not in compass to "unknown"
  mutate(current_direction =
           if_else(
             !(current_direction %in% compass_points), "UNKN",
                  current_direction
           ))


arrow::write_parquet(wq_data_dep_subset,"data/wq_data_dep_subset.parquet")
# impute missing values.
# first group by date and take the mean of each column on that date
# replace missing values with the mean of the column for that date
# IS THIS GOOD PRACTICE? I DON'T KNOW.
wq_data_dep_imputed <- wq_data_dep_subset |>
  mutate(date = as.Date(sample_datetime)) |>
  mutate(.by = date,across(where(is.numeric), ~if_else(is.na(.), mean(., na.rm = TRUE), .)))

arrow::write_parquet(wq_data_dep_imputed,"data/wq_data_dep_imputed.parquet")
