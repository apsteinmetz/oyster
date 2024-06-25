get_tide_position <- function(obs_time= as.POSIXct("2011-10-20 18:43:00"),station="8530645"){
  # find the closest tide time to the observation time
  # use global tides_noaa. Is it faster?
  tide_time <- tides_noaa |>
    filter(station_id == station) |>
    filter(difftime(obs_time,datetime,units="hours") < 6) |>
    slice_min(order_by=datetime,n=1)
  return(tibble(since_tide = obs_time-tide_time$datetime,
                last_tide_level = tide_time$tide_level,
                flood_or_ebb = if_else(tide_time$hi_lo == "H",-1,1)))
}

get_tide_time <- function(obs_time,station="8530645"){
  # find the closest tide time to the observation time
  # use global tides_noaa. Is it faster?
  tide_time <- tides_noaa |>
    filter(station_id == station) |>
    filter(difftime(obs_time,datetime,units="hours") < 6) |>
    slice_min(order_by=datetime,n=1)
  return(tide_time$datetime)
}

get_tide_position(wq_data_2$sample_time[1],station="8530645")

tides_noaa |>
  filter(station_id == station) |>
#  filter(difftime(obs_time,datetime,units="hours") < 6) |>
  slice_min(order_by=datetime,n=1)
