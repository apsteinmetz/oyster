

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(RcppRoll)

print(getwd())

load("../data/wq_data.rdata")
load("../data/wq_meta.rdata")
load("../data/weather.rdata")

all_dates = unique(wq_data$date)
weather <- weather %>%
  mutate(rain_7D = roll_sum(PRCP, 7, fill = NA, align = "right"))  %>%
  filter(date %in% all_dates) %>%
  mutate(temp_color = cut(TEMP, 5, labels = c(
    "blue", "lightblue", "green", "yellow", "red"
  ))) %>%
  mutate(rain_color = cut(rain_7D, 5, labels = colorRampPalette(c(
    "lightskyblue", "darkblue"
  ))(5)))

max_rain <- max(weather$rain_7D, na.rm = T)

# Define UI for application that draws a histogram
ui <- fluidPage(responsive = FALSE,
                fluidRow(column(
                  12,
                  # Application title
                  titlePanel("NYC Harbor Bacteria Levels"),
                  fluidRow(column(3, textOutput("next_date"))),
                  fluidRow(
                    column(
                      3,
                      sliderInput(
                        "date",
                        "Dates:",
                        min = min(all_dates),
                        max = max(all_dates),
                        value = min(all_dates),
                        animate = animationOptions(
                          interval = 100,
                          loop = FALSE,
                          playButton = NULL,
                          pauseButton = NULL
                        ),
                        step = 7
                      ),
                      fluidRow(column(4,
                                      plotOutput("tempPlot")),
                               column(4,
                                      plotOutput("rainPlot"))),
                      fluidRow(column(12, "Temp.(F) and 7-Day Rainfall (in)"))
                    ),
                    column(width = 9,
                           leafletOutput("wqPlot"))
                  )
                )))


closest_val <- function(vec, val) {
  vec[which.min(abs(vec - val))]
}

# Create a palette that maps factor levels to colors
pal <- colorFactor(
  palette = c("grey", "green", "yellow", "red"),
  levels =  levels(wq_data$quality)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  new_date <- reactive({
    closest_val(all_dates, input$date)
  })

  output$next_date <- renderText({
    format.Date(new_date())
  })

  new_weather <- reactive({
    weather %>%
      filter(date == closest_val(all_dates, input$date))
  })

  output$tempPlot <- renderPlot({
    weather_1d <- new_weather()
    weather_1d %>%
      ggplot(aes(date, TEMP)) + geom_col(fill = weather_1d$temp_color) +
      theme(legend.position = "none") +
      scale_y_continuous(limits = c(0, 100))
  })

  output$rainPlot <- renderPlot({
    weather_1d <- new_weather()
    weather_1d %>%
      ggplot(aes(date, rain_7D)) + geom_col(fill = weather_1d$rain_color) +
      theme(legend.position = "none") +
      scale_y_continuous(limits = c(0, max_rain))
  })

  output$wqPlot <- renderLeaflet({
    leaflet() %>%
      fitBounds(-74.1, 40.6,-73.8, 40.8) %>%
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
