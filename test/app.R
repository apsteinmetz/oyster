library(shiny)

set.seed(2022)
date_vec <- as.Date("2022-01-01") + cumsum(round(runif(10, 1, 20)))

ui <- fluidPage(
  sliderInput(
    "date",
    "Date",
    min = min(date_vec),
    max = max(date_vec),
    value = min(date_vec),
    animate = TRUE
  ),
  mainPanel(
    textOutput("this_date")
  )
)

server <- function(input, output, session) {

  observe({
    req(input$date)
    updateSliderInput(session, "date",
                      value = min(date_vec[which(input$date <= date_vec)]))
  })

  output$this_date <- renderText({
    paste("Slider Date:", format.Date(input$date))
  })
}

shinyApp(ui = ui, server = server)
