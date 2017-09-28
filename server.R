
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  observeEvent(input$execute, {
    req(input$file)
    df <- read.csv(input$file$datapath,
                   header = TRUE,
                   sep = ",")
    
    analysis = input$expAnalysis
    model = input$model
    if("graphic" %in% input$expAnalysis){
      output$graphic <- renderPlot({ plot(df[,1]) })
    }
    
    if("histogram" %in% input$expAnalysis){
      output$histogram <- renderPlot({ hist(df[,1]) })
    }
    
    if("pacf" %in% input$expAnalysis){
      output$pacf <- renderPlot({ pacf(df[,1]) })
    }
    
    if("acf" %in% input$expAnalysis){
      output$acf <- renderPlot({ acf(df[,1]) })
    }
  })
  
})
