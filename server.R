
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
    
    data_to_analyze = df[,1]
    
    analysis = input$expAnalysis
    model = input$model
    if("graphic" %in% analysis){
      output$graphic <- renderPlot({ plot(data_to_analyze) })
    }
    
    if("histogram" %in% analysis){
      output$histogram <- renderPlot({ hist(data_to_analyze) })
    }
    
    if("pacf" %in% analysis){
      output$pacf <- renderPlot({ pacf(data_to_analyze) })
    }
    
    if("acf" %in% analysis){
      output$acf <- renderPlot({ acf(data_to_analyze) })
    }
    
    switch(model, 
           none={
             
           }, 
           lineal={
             output$modelName <- renderText({ "Regresion Lineal" })
             
             y <- data_to_analyze
             y <- ts(y,frequency = 4,start = c(2005,1)) 
             t <- seq(1:length(y))                      
             m <- lm(formula = y ~ t)
             
             output$modelPlot <- renderPlot({
               options(repr.plot.width=6, repr.plot.height=6)
               plot(t, y, type = "o", lwd = 2)
               lines(m$fitted.values, col = "red", lwd = 2)
               legend( "topleft",                              
                       c("real","pronostico"),                 
                       lwd = c(2, 2),                          
                       col = c('black','red'),                 
                       bty = "n")                              
               grid()
             })
             
             output$modelSummary <- renderPrint({ summary(m) })
             
             output$modelExtraGraphs <- renderPlot({
               par(mfrow=c(2,2))
               options(repr.plot.width=10, repr.plot.height=6)
               r = m$residuals
               plot(t, r, type='b', ylab='', main="Residuales", col="red")
               abline(h=0,lty=2)               
               plot(density(r), xlab='x', main= 'Densidad residuales', col="red")
               qqnorm(r)               
               qqline(r, col=2)         
               acf(r, ci.type="ma", 60)
             })
             
           }, 
           squared={
             output$modelName <- renderText({ "Regresion Cuadratica" })
             
             y <- data_to_analyze
             t  <- seq(1:length(y))   
             tt <- t*t                
             m  <- lm(y ~ t + tt)     
             
             output$modelPlot <- renderPlot({
               options(repr.plot.width=8, repr.plot.height=6)
               plot(t, y, type="o", lwd=2)
               lines(m$fitted.values, col = "red", lwd = 2)
               legend( "topleft", c("real","pronostico"),
                       lwd = c(2, 2), col = c('black','red'), 
                       bty = "n") 
               grid()
             })
             
             output$modelSummary <- renderPrint({ summary(m) })
             
             output$modelExtraGraphs <- renderPlot({
               par(mfrow=c(2,2))
               options(repr.plot.width=10, repr.plot.height=6)
               r = m$residuals
               plot(t, r, type='b', ylab='', main="Residuales", col="red")
               abline(h=0,lty=2)               
               plot(density(r), xlab='x', main= 'Densidad residuales', col="red")
               qqnorm(r)               
               qqline(r, col=2)         
               acf(r, ci.type="ma", 60)
             })
           }, 
           cubic={
             
           }
    )
    
  })
  
})
