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

get_nearest_id_to_wq_station <- Vectorize(function(lat_q,lon_q) {

  # get distance between each station and the lat,lon
  # assumes coordinate columns are names latitude and longitude
  closest_id <- stations_to_query %>%
    mutate(dist = get_distance(latitude,longitude,lat_q,lon_q)) |>
    # get the closest station
    slice_min(dist,n=1,with_ties = FALSE) |>
    pull(id)
  return(closest_id)
})

