# NY Water Quality over Time

library(shiny)
library(shinyWidgets)
library(bslib)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(RcppRoll)

load("www/wq_data.rdata")
load("www/wq_meta.rdata")
load("www/weather.rdata")

all_dates = unique(wq_data$date)
weather <- weather %>%
  mutate(rain_7D = roll_sum(PRCP, 7, fill = NA, align = "right"))  %>%
  filter(date %in% all_dates) %>%
  mutate(temp_color = cut(TEMP, 5,
                          # labels =  c("blue", "lightblue", "green", "yellow", "red"
                          labels = colorRampPalette(c("darkblue", "yellow","tomato"))(5)
                          )) %>%
  mutate(rain_color = cut(rain_7D, 5, labels = colorRampPalette(c(
    "lightskyblue", "darkblue"
  ))(5)))

max_rain <- max(weather$rain_7D, na.rm = T)

closest_val <- function(vec, val) {
  vec[which.min(abs(vec - val))]
}

# Create a palette that maps factor levels to colors
pal <- colorFactor(
  palette = c("grey", "green", "yellow", "red"),
  levels =  levels(wq_data$quality)
)

# Define UI
oyster_theme <- bs_theme(
       bg = "#0b3d91", fg = "white", primary = "#FCC780",
       base_font = font_google("Roboto Condensed"),
       code_font = font_google("Roboto Condensed")
  )

ui <- fluidPage(theme = oyster_theme,
                  # Application title
                  fluidRow(column(3,
                                  img(src = 'bop_logo.png', align = "left"),
                                  "Demo Only. Not sanctioned by BOP"),
                  column(9,
                         h3("NYC Harbor Enterococci Levels"))),
                  fluidRow(
                    column(
                      3,
                      br(),
                      sliderInput(
                        "date",
                        HTML("<b>Dates</b>"),
                        min = min(all_dates),
                        max = max(all_dates),
                        value = min(all_dates),
                        step = 1,
                        animate = animationOptions(
                          interval = 1000,
                          loop = FALSE,
                          playButton = NULL,
                          pauseButton = NULL
                        )
                      ),
                      HTML("<b>Observation Date</b>"),
                      textOutput("next_date"),
                      HTML("<b>Weather Gauges</b>"),
                      fluidRow(column(4,
                                      plotOutput("tempPlot")),
                               column(4,
                                      plotOutput("rainPlot"))),
                      fluidRow(column(12, "Temp.(F) and 7-Day Rainfall (in)")),
                      HTML("<br>"),
                      fluidRow(column(12, "Source: Citizens Water Quality Testing Program, NOAA")),
                      fluidRow(column(12, tags$a(href="https://nycwatertrail.org/water-quality/","Water Quality Data Source"))),
                      HTML("<br>"),
                      fluidRow(column(12, HTML("Built by Art Steinmetz using Shiny from RStudio")))
                    ),
                    column(width = 9,
                           leafletOutput("wqPlot", height = "90vh"))
                  )
                )


# Define server logic
server <- function(input, output, session) {
  observe({
    req(input$date)
    updateSliderInput(session, "date",
                      value = min(all_dates[which(input$date <= all_dates)]))  })

  new_date <- reactive({
    min(all_dates[which(input$date <= all_dates)])
  })

  output$next_date <- renderText({
    #   format.Date(new_date())
       format.Date(input$date)

   })

  output$tempPlot <- renderPlot({
    weather_1d <- weather %>%
      filter(date == input$date)
    weather_1d %>%
      ggplot(aes(date, TEMP)) + geom_col(fill = weather_1d$temp_color) +
      theme(legend.position = "none") +
      theme(axis.text.x = element_blank()) +
      labs(x = "") +
      scale_y_continuous(limits = c(0, 100))
  })

  output$rainPlot <- renderPlot({
    weather_1d <- weather %>%
      filter(date == input$date)
    weather_1d %>%
      ggplot(aes(date, rain_7D)) + geom_col(fill = weather_1d$rain_color) +
      theme(legend.position = "none") +
      theme(axis.text.x = element_blank()) +
      labs(x = "") +
      scale_y_continuous(limits = c(0, max_rain))
  })

  output$wqPlot <- renderLeaflet({
    leaflet() %>%
      fitBounds(-74.1, 40.6,-73.8, 40.8) %>%
      addTiles()
  })

  observe({
    filtered_data <- wq_data %>%
      filter(date == new_date()) %>%
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
          paste(scales::comma(round(bacteria)), "Enterococci Colonies")
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
