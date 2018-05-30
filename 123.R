library(shiny)
library(rhandsontable)

did_recalc <- FALSE

ui <- fluidPage(
  rHandsontableOutput('table'),
  textOutput('result')
)

server <- function(input,output,session)({
  values <- reactiveValues(data=as.data.frame(runif(2)))
  
  observe({
    values$data <- as.data.frame(runif(2))
  })
  
  observe({
    if(!is.null(input$table))
      values$data <- hot_to_r(input$tables)
  })
  
  
  output$table <- renderRHandsontable({
    rhandsontable(values$data)
  })
  
  
  output$result <- renderText({ 
    sum(values$data)
  })
}) 

shinyApp(ui = ui, server = server)
