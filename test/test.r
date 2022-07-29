library(htmltools)
library(leaflet)
library(dplyr)

df <- read.csv(textConnection(
  "Name,Rating,Lat,Long
Samurai Noodle,Good, 47.597131,-122.327298
Kukai Ramen,Fair,47.6154,-122.327157
Tsukushinbo,Great,47.59987,-122.326726"))

leaflet(df) %>%  addTiles() %>%
  addMarkers(~Long, ~Lat,
             label = ~ HTML(paste(Name,Rating,sep = "<br/>")))
