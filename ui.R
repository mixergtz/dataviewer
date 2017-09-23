
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  br(),
  fluidRow(
    column(3,
           h4("Menu"),
           wellPanel(
             fileInput("file", "Archivo", multiple = FALSE,
                       accept = c("text/csv", "text/comma-separated-values,text/plain",
                                  ".csv")),
             tags$hr(),
             checkboxGroupInput("expAnalysis", label = h5("Analisis Exploratorio"), 
                                choices = list("Grafico" = "graphic", "Histograma" = "histogram", "Acf" = "acf", "Pacf" = "pacf"),
                                selected = 1),
             h5("Modelos de Regresión"),
             radioButtons("model", label = h6("Tendencia"),
                          choices = c("Ninguna" = "none",
                                      "Lineal" = "lineal",
                                      "Cuadratica" = "squared",
                                      "Cubica" = "cubic"),
                          selected = "none"),
             radioButtons("model", label = h6("Estacional"),
                          choices = c("Ninguna" = "none",
                                      "Dummy" = "dummy"),
                          selected = "none"),
             actionButton("execute", "Ejecutar")
           )
    ),
    column(7,
           h5("Graficas"),
           verbatimTextOutput("prueba")
    ),
    column(2, 
           h5("Resultados")
    )
  )
))
