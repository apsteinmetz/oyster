library(shiny)

date_vec <- as.Date("2022-01-01") +
  cumsum(round(runif(10, 1, 20)))

ui <- fluidPage(uiOutput("jumpySlider"),
                mainPanel(textOutput("this_date"),
                          textOutput("desired_date")))

server <- function(input, output) {
  nearest_date <- reactive({
      date_vec[which.min(abs(as.numeric(input$date) - as.numeric(date_vec)))]
  })

  output$jumpySlider <- renderUI({
    sliderInput(
      "date",
      "Date",
      min = min(date_vec),
      max = max(date_vec),
      # value = min(date_vec),
      # PROBLEM LINE
      value = ifelse(length(nearest_date()) > 0,nearest_date(),min(date_vec)),
      animate = animationOptions(interval = 100)
    )
  })

  output$this_date <- renderText({
    paste("Slider Date:", format.Date(input$date))
  })
  output$desired_date <- renderText({
    paste("Desired Date:", format.Date(nearest_date()))
  })
}
shinyApp(ui = ui, server = server)
