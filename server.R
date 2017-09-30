
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(fBasics)

shinyServer(function(input, output) {
  
  observeEvent(input$execute, {
    req(input$file)
    df <- read.csv(input$file$datapath,
                   header = TRUE,
                   sep = ",")
    
    data_to_analyze = na.omit(df[,input$column])
    analysis = input$expAnalysis
    model = input$model
    startYear = input$startYear
    startPeriod = input$startPeriod
    frecuency = input$frecuency
    
    if("graphic" %in% analysis){
      output$graphic <- renderPlot({ plot(data_to_analyze, main="Grafico", ylab=names(df)[1], xlab="t") })
    }
    
    if("histogram" %in% analysis){
      output$histogram <- renderPlot({ hist(data_to_analyze, main="Histograma", ylab=names(df)[1], xlab="t") })
    }
    
    if("pacf" %in% analysis){
      output$pacf <- renderPlot({ pacf(data_to_analyze, main="Pacf", ylab=names(df)[1], xlab="t") })
    }
    
    if("acf" %in% analysis){
      output$acf <- renderPlot({ acf(data_to_analyze, main="Acf", ylab=names(df)[1], xlab="t") })
    }
    
    switch(model, 
           none={
             output$modelName <- renderText({ "Ningun modelo ha sido seleccionado" })
           }, 
           lineal={
             output$modelName <- renderText({ "Regresion Lineal" })
             
             y <- data_to_analyze
             y <- ts(y,frequency = frecuency, start = c(startYear, startPeriod))
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
             y <- ts(y,frequency = frecuency, start = c(startYear, startPeriod))
             t <- seq(1:length(y))
             tt <- t*t 
             
             df2 = data.frame(y, t, tt)
             trainNumber = nrow(df2)*0.8
             
             dataTrain = df2[1:trainNumber,]
             dataTest = df2[trainNumber:nrow(df2),]
             
             # Forecast
             fit = lm(y ~ t + tt, data = dataTrain)
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
               plot(dataTrain$tt, r, type='b', ylab='', main="Residuales", col="red")
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
             y <- ts(y,frequency = frecuency, start = c(startYear, startPeriod))
             t <- seq(1:length(y))
             ttt <- t*t*t
             
             df2 = data.frame(y, t, ttt)
             trainNumber = nrow(df2)*0.8
             
             dataTrain = df2[1:trainNumber,]
             dataTest = df2[trainNumber:nrow(df2),]
             
             # Forecast
             fit = lm(y ~ t + ttt, data = dataTrain)
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
               plot(dataTrain$tt, r, type='b', ylab='', main="Residuales", col="red")
               abline(h=0,lty=2)               
               plot(density(r), xlab='x', main= 'Densidad residuales', col="red")
               qqnorm(r)               
               qqline(r, col=2)         
               acf(r, ci.type="ma", 60)
             })
           },
           holt={
             output$modelName <- renderText({ "HoltWinters" })
             
             y <- data_to_analyze
             
             y <- ts(y,frequency = frecuency, start = c(startYear, startPeriod))
             hw <- HoltWinters(y)
             forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)

             output$modelPlot <- renderPlot({
               plot(hw, forecast)                          
               grid()
             })
             
             output$modelSummary <- renderPrint({ summary(hw) })
             
             output$modelExtraGraphs <- renderPlot({

             })
           }
    )
    
    output$descriptiveSummary <- renderPrint({ basicStats(data_to_analyze) })
    
  })
  
})
