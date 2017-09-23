
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  observeEvent(input$execute, {
    analysis = input$expAnalysis
    model = input$model
    output$prueba <- renderText({paste("Ejecutando", "analisis", analysis, "con", model, "\n")})
  })

})
