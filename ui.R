
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
             fileInput("file", "Archivo de serie de tiempo (csv)", multiple = FALSE,
                       accept = c("text/csv", "text/comma-separated-values,text/plain",
                                  ".csv")),
             numericInput("startYear", "Año de inicio:", 2017, min = 1000, max = 3000),
             numericInput("startPeriod", "Periodo de inicio:", 1, min = 1, max = 40),
             numericInput("frecuency", "Frecuencia:", 4, min = 1, max = 100),
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
    column(9,
           tabsetPanel(
             tabPanel(h5("Estadísticos Datos"),
                      verbatimTextOutput("descriptiveSummary")
                      ),
             tabPanel(h5("Resultados Exploratorios"), 
                      plotOutput("graphic"),
                      plotOutput("histogram"),
                      plotOutput("pacf"),
                      plotOutput("acf")
                      ),
             tabPanel(h5("Resultados Modelo"),
                      h4(textOutput("modelName")),
                      plotOutput("modelPlot"),
                      verbatimTextOutput("modelSummary"),
                      plotOutput("modelExtraGraphs")
                      )
           )
    )
  )
))
