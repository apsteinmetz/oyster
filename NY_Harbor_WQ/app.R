# NY Water Quality over Time

library(shiny)
library(shinyWidgets)
library(bslib)
library(tidyverse)
library(lubridate)
library(leaflet)
library(RcppRoll)
library(googlesheets4)
library(rnoaa)



# Get Data ----------------------------------
UPDATE = FALSE
if (UPDATE){
  # Omit need for key since sheets are public
  gs4_deauth()
  wq_url <-
    "https://docs.google.com/spreadsheets/d/1813b2nagaxZ80xRfyMZNNKySZOitro5Nt7W4E9WNQDA/edit?usp=sharing"

  wq_meta_raw <- read_sheet(wq_url,"Information",range = "A10:J400")
  wq_data_raw <- read_sheet(wq_url,"Data")

  # Wrangle Data ----------------------------------
  wq_meta <- wq_meta_raw %>%
    rename_with(~str_replace_all(.x," ","_")) %>%
    rename_with(~str_replace_all(.x,"/","_")) %>%
    rename("site" = 2) %>%
    filter(!is.na(site)) %>%
    mutate(Currently_Testing = if_else(is.na(Currently_Testing),0,1)) %>%
    mutate(Currently_Testing = as.logical(Currently_Testing)) %>%
    rename_with(tolower) %>%
    mutate(site = as.factor(site))


  data_names <- c("site","date","year","month","high_tide","sample_time","bacteria",
                  "precip_t-0","precip_t-1","precip_t-2","precip_t-3","precip_t-4",
                  "precip_t-5","precip_t-6","notes")

  wq_data <- wq_data_raw
  names(wq_data) <- data_names
  wq_data <- wq_data %>%
    mutate(date = as_date(date)) %>%
    mutate(sample_time = hms::as_hms(sample_time)) %>%
    mutate(high_tide = hms::as_hms(high_tide)) %>%
    mutate(across(where(is.list), as.character)) %>%
    mutate(across(where(is.character), .fns = ~ str_replace(.x, "<10", "0"))) %>%
    # > 24196 test limit?
    mutate(across(where(is.character), .fns = ~ str_replace(.x, ">", ""))) %>%
    mutate(across(where(is.character), .fns = ~ str_replace(.x, "Trace", "0"))) %>%
    # get rid of snow inches next to precip as water
    mutate(across(where(is.character), .fns = ~ str_replace(.x, "\\(.+\\)", ""))) %>%
    mutate(across(where(is.character), .fns = ~ na_if(.x, "N/A"))) %>%
    mutate(across(contains("precip"), as.numeric)) %>%
    mutate(bacteria = as.numeric(bacteria)) %>%
    mutate(notes = replace_na(notes, "")) %>%
    # fix some typos
    mutate(site = str_replace(site, "Daylighted Section", "daylighted section")) %>%
    mutate(site = str_replace(site, "Govenors", "Governors")) %>%
    mutate(quality = cut(
      bacteria,
      breaks = c(-1, 0, 35, 104, 49999),
      labels = c("Not Detected", "Good", "Fair", "Unacceptable")
    )) %>%
    mutate(site = as.factor(site))

  save(wq_data,file="www/wq_data.rdata")
  save(wq_meta,file="www/wq_meta.rdata")

}

load("www/wq_data.rdata")
load("www/wq_meta.rdata")
load("www/weather.rdata")

# Get latest weather
datatypeids <- c("TMIN","TMAX","PRCP")

# ncdc_datasets(stationid = "GHCND:USW00014732")

# default is Laguardia airport nyc
get_weather <-  function(station = "USW00014732",
                         startdate = Sys.Date() - 365,
                         enddate = Sys.Date() - 1,
                         datatypeid = "TMAX") {
  weather <- ncdc(
    datasetid = 'GHCND',
    stationid = paste0("GHCND:", station),
    datatypeid=datatypeid,
    startdate = startdate,
    enddate = enddate,
    limit = 1000,
    add_units = TRUE
  )
  return(weather$data)
}
c_to_f <- function(temp) {
  return(temp *9/5 + 32)
}

mm_to_in <- function(len) {
  return(len*.039)
}

fix_raw_weather <- function(weather_raw) {
  weather_raw %>%
    transmute(date = as.Date(date),
              station = "LGA",
              datatype,
              value) %>%
    pivot_wider(names_from = "datatype",
                values_from = "value") %>%
    transmute(date,
              station,
              TEMP = c_to_f((TMIN + TMAX) / 20),
              PRCP = mm_to_in(PRCP / 100))
}

if (UPDATE){
  start_date <- max(weather$date) + 1
  weather_update_raw <- map_dfr(datatypeids,
                                function(d) get_weather(datatypeid = d,startdate = start_date))
  if (nrow(weather_update_raw)>0){
    weather_update <- fix_raw_weather(weather_update_raw)
    weather <- bind_rows(weather,weather_update) %>% unique()
  }
}

save(weather,file="www/weather.rdata")

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
                      fluidRow(column(12, HTML("<i>Source: Citizens Water Quality Testing Program, NOAA</i>"))),
                      fluidRow(column(12, HTML("<i>https://www.nycwatertrail.org/water_quality.html</i>"))),
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
