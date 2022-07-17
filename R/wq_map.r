# map water quality

library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)

load("data/wq_data.rdata")
load("data/wq_meta.rdata")
load("data/weather.rdata")

wq_data_summary <- wq_data %>%
  select(-contains("precip")) %>%
  group_by(site) %>%
  summarise(median_bacteria = median(bacteria))

wq_meta_select <-  wq_meta %>%
  filter(!is.na(currently_testing)) %>%
  filter(currently_testing == TRUE) %>%
  left_join(wq_data_summary,by="site") %>%
  mutate(median_quality = cut(
    median_bacteria,
    breaks = c(-1, 0, 35, 104, 49999),
    labels = c("NotDetected", "Good", "Fair", "Unacceptable")
  )) %>%
  filter(!is.na(median_quality))

# Create a palette that maps factor levels to colors
pal <- colorFactor(palette = c("grey","green","yellow","red"),
                   levels =  levels(wq_meta_select$median_quality))


# median water quality- entire history


leaflet::leaflet(wq_meta_select) %>%
  fitBounds(-74.270325,40.490826,-73.668823,40.960197) %>%
  addTiles() %>%
  addCircles(~longitude, ~latitude,
             radius = ~log(median_bacteria)*100,
             color = "black",
             weight = 2,
             fillColor = ~pal(median_quality),
             fillOpacity = .7,
             label = ~as.character(site))



# most recent water quality
all_dates = unique(wq_data$date)
obs_date = max(wq_data$date)
#obs_date = all_dates[140]

wq_data %>%
  filter(date == obs_date) %>%
  left_join(wq_meta) %>%
  group_by(site) %>%
  leaflet() %>%
  fitBounds(-74.1, 40.6, -73.8, 40.8) %>%
  addTiles() %>%
  addCircles(
    ~ longitude,
    ~ latitude,
    radius = ~ log(bacteria) * 100,
    color = "black",
    weight = 2,
    fillColor = ~ pal(quality),
    fillOpacity = .7,
    options = popupOptions(closeButton = FALSE),
    popup = ~paste(
      sep = "<br/>",
      paste("<b>",site,"</b>"),
      paste("Sample Time",date ,  sample_time),
      paste(str_remove(as.period(abs(high_tide - sample_time),hours)," 0S"),"From High Tide"),
      paste(scales::comma(bacteria), "Enterococci Colonies")
    )
  )

weather %>%
  filter(date > obs_date-8) %>%
  filter(date <  obs_date-1) %>%
  ggplot() +
  geom_line(aes(date,TEMP),fill = "yellow")


weather %>%
  filter(date > obs_date-8) %>%
  filter(date <  obs_date-1) %>%
  ggplot() +
  geom_col(aes(date,PRCP),fill="blue")

