# get closest tide station
library(tidyverse)
# library(rnoaa)
library(rvest)
library(duckdb)
library(duckplyr)

methods_overwrite()
battery <- "8518750"
# uses javascript on the fly so save this page as html and read that
# https://tidesandcurrents.noaa.gov/tide_predictions.html?gid=1407#listing

tide_station_path <- "data/Tide_Stations_NY_Harbor.html"
tide_stations_raw <- rvest::read_html(tide_station_path)



tide_station_url <- "https://tidesandcurrents.noaa.gov/tide_predictions.html?gid=1407#listing"
# maybe try rselenium or v8
# not implemented. This is a javascript page
# Instead I did a manual copy paste of the table and saved it in an html file
# tide_stations_xml <- rvest::read_html(tide_station_url)
#
# js <- tide_stations_xml |>
#   html_elements("script") |>
#   html_text() |>
#   pluck(9)
#
# sb <- V8::new_context()
#
# sb$eval(js)


# extract table from tide station page
tide_stations <- tide_stations_raw %>%
  rvest::html_elements("table") %>%
  rvest::html_table() |>
  pluck(1) |>
  # make a separate column for names where Id is an empty string
  mutate(area = if_else(Id == "", Name, NA),.before = Name) |>
  # if area is empty, fill with previous area
  fill(area) |>
  # remove rows where Id is empty
  filter(Id != "")


write_csv(tide_stations, "data/tide_stations_nyc.csv")
tide_stations <- duckplyr_df_from_csv("data/tide_stations_nyc.csv")

# function to get distance between two points of lat/lon -----------------------
# https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula

# pi constant
pi <- 3.14159265358979323846

deg2rad <- function(deg) {
  return (deg * (pi/180))
}

get_distance <- function(lat1,lon1,lat2,lon2) {
  # Haversine formula
  rad <- 6371 # Radius of the earth in km
  dLat <- deg2rad(lat2-lat1) # deg2rad below
  dLon <-  deg2rad(lon2-lon1);
  a =
    sin(dLat/2) * sin(dLat/2) +
    cos(deg2rad(lat1)) * cos(deg2rad(lat2)) *
    sin(dLon/2) * sin(dLon/2)

  c <- 2 * atan2(a**0.5, (1-a)**0.5)
  d <- rad * c # Distance in km
  return (d)
}

# example: get distance in km between two points
get_distance(40.7,-74,40.8,-74)

wq_meta <- duckplyr_df_from_parquet("data/wq_meta.parquet")

get_closest_tide_station_meta <- function(lat_q,lon_q) {
  # get distance between each station and the lat/lon
 if (is.na(lat_q) | is.na(lon_q)) {
   # if missing lat/lon set station location to Gravesend Bay
   lat_q <- 40.58534
   lon_q <- -73.99838
 }
  closest <- tide_stations %>%
    mutate(dist = get_distance(Lat,Lon,lat_q,lon_q)) |>
    # get the closest station
    filter(dist == min(dist)) |>
    # prepend "closest_" to column names
    rename_with(~paste0("closest_tide_",.x))
  return(as_tibble(as.list(closest)))
}

get_closest_tide_station <- Vectorize(function(lat_q,lon_q) {
  # get distance between each station and the lat/lon
  if (is.na(lat_q) | is.na(lon_q)) {
    # if missing lat/lon set station location to Gravesend Bay
    lat_q <- 40.58534
    lon_q <- -73.99838
  }
  closest_tide_station <- tide_stations %>%
    mutate(dist = get_distance(Lat,Lon,lat_q,lon_q)) |>
    # get the closest station
    filter(dist == min(dist)) |>
    pull(Id)
  return(closest_tide_station)
})

# add closest tide station data to wq_meta
wq_meta_2 <- wq_meta |>
  mutate(closest_tide_station = map2_dfr(latitude,longitude,get_closest_tide_station_meta)) |>
  unnest(cols = c(closest_tide_station)) |>
  as_duckplyr_df()


arrow::write_parquet(wq_meta_2, "data/wq_meta_2.parquet")

# add closest tide station to wq_data
wq_data_2 <- arrow::read_parquet("data/wq_data.parquet",as_data_frame = FALSE) |>
  left_join(wq_meta_2 %>% select(site,closest_tide_Id), by = "site") |>
  select(-high_tide)

#  choose battery where missing tide station
wq_data_2 <- wq_data_2 |>
  mutate(closest_tide_Id = if_else(is.na(closest_tide_Id), battery, closest_tide_Id))

arrow::write_parquet(wq_data_2,"data/wq_data_2.parquet")

