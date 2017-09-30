
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(forecast)

shinyServer(function(input, output) {
  
  observeEvent(input$execute, {
    #req(input$file)
    #df <- read.csv(input$file$datapath,
    #               header = TRUE,
    #               sep = ",")
    df <- read.csv("~/Desktop/Workbook2.csv")
    
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
             output$modelName <- renderText({ "Ningun modelo ha sido seleccionado" })
           }, 
           lineal={
             output$modelName <- renderText({ "Regresion Lineal" })
             
             y <- data_to_analyze
             y <- ts(y,frequency = 4,start = c(2005,1)) 
             t <- seq(1:length(y)) 
             
             df2 = data.frame(y, t)
             trainNumber = nrow(df2)*0.8
             
             dataTrain = df2[1:trainNumber,]
             dataTest = df2[trainNumber:nrow(df2),]
             
             # Forecast
             fit = lm(y ~ t, data = dataTrain)
             preds <- predict(fit, newdata=dataTest, interval="confidence")
            
             output$modelPlot <- renderPlot({
               plot(t, y, type="o", lwd=1)
               polygon(c(rev(dataTest$t), dataTest$t), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(79/255, 148/255, 207/255, 0.6), border = NA)
               lines(fit$fitted.values, col = "red", lwd = 2)
               lines(dataTest$t,preds[,"fit"],lty=3)
               legend( "topleft",                              
                       c("real","pronostico"),                 
                       lwd = c(2, 2),                          
                       col = c('black','red'),                 
                       bty = "n")                              
               grid()
             })
             
             output$modelSummary <- renderPrint({ summary(fit) })
             
             output$modelExtraGraphs <- renderPlot({
               par(mfrow=c(2,2))
               options(repr.plot.width=10, repr.plot.height=6)
               r = fit$residuals
               plot(dataTrain$t, r, type='b', ylab='', main="Residuales", col="red")
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
             
             # Forecast
             newt <- seq(min(t), max(t), length.out=100)
             preds <- predict(m, newdata=data.frame(x=newt), interval="confidence")
             
             output$modelPlot <- renderPlot({
               options(repr.plot.width=8, repr.plot.height=6)
               plot(t, y, type="o", lwd=2)
               polygon(c(rev(newt), newt), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(79/255, 148/255, 207/255, 0.6), border = NA)
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
             output$modelName <- renderText({ "Regresion Cubica" })
             
             y <- data_to_analyze
             t  <- seq(1:length(y))
             ttt <- t*t*t
             m  <- lm(y ~ t + ttt)
             
             # Forecast
             newt <- seq(min(t), max(t), length.out=100)
             preds <- predict(m, newdata=data.frame(x=newt), interval="confidence")
             
             output$modelPlot <- renderPlot({
               options(repr.plot.width=8, repr.plot.height=6)
               plot(t, y, type="o", lwd=2)
               polygon(c(rev(newt), newt), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(79/255, 148/255, 207/255, 0.6), border = NA)
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
           }
    )
    
  })
  
})
