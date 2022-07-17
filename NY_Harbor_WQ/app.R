
library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)

print(getwd())

load("../data/wq_data.rdata")
load("../data/wq_meta.rdata")

# most recent water quality
all_dates = unique(wq_data$date)
obs_date = max(wq_data_select$date)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NYC Harbor Bacteria Levels"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("date",
                        "Dates:",
                        min = min(all_dates),
                        max = max(all_dates),
                        value = min(all_dates),
                        animate = TRUE,
                        step = 7)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("wqPlot")
        )
    )
)


closest_val <- function(vec,val){
  vec[which.min(abs(vec-val))]
}

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$wqPlot <- renderLeaflet({
        leaflet() %>%
        fitBounds(-74.1, 40.6, -73.8, 40.8) %>%
        addTiles()
    })

    observe({
      next_date <- closest_val(all_dates, input$date)
      filtered_data <- wq_data %>%
        filter(date == next_date) %>%
        left_join(wq_meta, by = "site") %>%
        filter(!is.na(latitude))

      leafletProxy("wqPlot", data = filtered_data) %>%
        clearShapes() %>%
        addCircles(
          ~ longitude,
          ~ latitude,
          radius = ~ log(bacteria) * 100,
          color = "black",
          weight = 2,
          fillColor = ~ pal(quality),
          fillOpacity = .7,
          options = popupOptions(closeButton = FALSE),
          popup = ~ paste(
            sep = "<br/>",
            paste("<b>", site, "</b>"),
            paste("Sample Time", date ,  sample_time),
            paste(str_remove(as.period(
              abs(high_tide - sample_time), hours
            ), " 0S"), "From High Tide"),
            paste(scales::comma(bacteria), "Enterococci Colonies")
          )
        )


    })
}

# Run the application
shinyApp(ui = ui, server = server)
