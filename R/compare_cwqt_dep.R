# compare locations of CWQTP and DEP sites
library(tidyverse)
library(here)
library(rvest)
library(duckplyr)
library(arrow)
library(leaflet)
library(htmltools)
library(sf)
library(gt)

# source helper functions
# get nearest station "id" from a data frame  of stations containing latitude
# and longitude columns

# pi constant
pi <- 3.14159265358979323846

deg2rad <- function(deg) {
  return (deg * (pi/180))
}

get_distance <- function(lat1,lon1,lat2,lon2) {
  # https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula
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

# set stations to query as a global variable so it doesn't get
# passed to the function each time e.g.:
#  stations_to_query <- ghcnd_stations_precip

get_nearest_id_to_wq_station <- function(lat_q,lon_q,id_col = site_id, include_dist = FALSE,
                                         label = c("dep")) {

  # get distance between each station and the lat,lon
  # assumes coordinate columns are names latitude and longitude
  closest_id <- stations_to_query %>%
    mutate(dist = get_distance(lat,long,lat_q,lon_q)) |>
    # get the closest station
    slice_min(dist,n=1,with_ties = FALSE)
  if (include_dist) {
    #return a tibble
    ret_val <- closest_id |> transmute(!!paste0(label,"_id") := site_id,
                                       !!paste0(label,"_dist_km") := dist) |>
      nest(data = everything())

  } else {
    #return a scalar
    ret_val <- closest_id$id
  }
  return(ret_val)
}

get_nearest_v <- Vectorize(get_nearest_id_to_wq_station)

# load data files
water_body_classifications <- read_csv(here("data/NYDEC_water_classifications.csv"),col_types = "fcc")
wq_meta_cwqt <- arrow::read_parquet(here("data/wq_meta.parquet"))
# wq_data_cwqt <- arrow::read_parquet(here("data/wq_data.parquet"))

wq_meta_dep <- arrow::read_parquet(here("data/wq_meta_dep.parquet"))

dep_stations  <- wq_meta_dep |>
  filter(year(last_sample_date) > 2022)

cwqt_stations <- wq_meta_cwqt |>
  filter(currently_testing)


station_pairs <- tibble()
for (cwqt in 1:nrow(cwqt_stations)) {
  for(dep in 1:nrow(dep_stations)) {
    # get the nearest dep station to the cwqt station
    dist <- get_distance(cwqt_stations$latitude[cwqt],
                           cwqt_stations$longitude[cwqt],
                           dep_stations$lat[dep],
                           dep_stations$long[dep])
    station_pairs <- bind_rows(station_pairs,
                               tibble(site_id = cwqt_stations$site_id[cwqt],
                                      sampling_location = dep_stations$sampling_location[dep],
                                      dist = dist))
  }
}
station_pairs <- station_pairs |>  slice_min(by = site_id,order_by = dist,n=1,with_ties = FALSE) |>
  arrange(desc(dist)) |>
  left_join(cwqt_stations,by = "site_id") |>
  left_join(wq_meta_dep,by = "sampling_location")

both_stations <-
  dep_stations |>
  ungroup() |>
  transmute(site_id = sampling_location,
         site = station_name,
         latitude = lat,
         longitude = long,
         source = "DEP") |>
  bind_rows(cwqt_stations |>
              ungroup() |>
              transmute(site_id, site,
                     latitude, longitude,
                     source = "CWQT")
            ) |>
  # remove rows with na
  filter(!is.na(site),longitude < -69)

map_labels <- glue::glue("<strong>{both_stations$site}</strong><br/>
                          Source </strong>{both_stations$source}") %>%
  lapply(htmltools::HTML)

both_stations %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = 4,
                   label = ~map_labels,
                   color = ~ifelse(source == "DEP","red","blue"))
