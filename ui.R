#Funciones fuera de Shiny App
{library(RCurl)
library(plyr)
library(urltools)
library(XML)
library(leaflet)
library(htmlwidgets)
library(sp)
library(stringr)
library(shiny)
library(tableHTML)
library(shinyjs)
library(shinydashboard)}
#Client Side

ui= fluidPage(
  
  sidebarLayout(
    sidebarPanel(
  
  textInput(inputId="Date", label="Date", value=Sys.time()+86400),
  
  fluidRow(
    box(width = 10, title="Origin point:",
        splitLayout(
          numericInput(inputId="OriginLat", label="Latitude", value=-33.4516906, min=-90, max=90),
          numericInput(inputId="OriginLon", label="Longitude", value=-70.66742690000001, min=-180, max=180)
        ),
        helpText(a("Find Latitude and Longitude values for specific locations here!", target="_blank", href="https://www.gps-coordinates.net/"))
        
    )
  ),
  
  numericInput(inputId="Puntos", label="Points", value=10, min=10, max=500),

  fluidRow(
    box(width = 10, title="Optional: Grid size", footer="Dont change this, ideally",
        splitLayout(
          numericInput(inputId="PointMatrixLat", label="Latitude", value=0.5154258, min=0.1, max=1),
          numericInput(inputId="PointMatrixLon", label="Longitude", value=0.8047485, min=0.1, max=1)
        )
    )
  ),
  
  textInput(inputId="Key", label="Google API Key", value=""),
  helpText(a("Get your Google Maps API Key here!", target="_blank", href="https://developers.google.com/maps/documentation/javascript/get-api-key?hl=EN"))
    ),
  mainPanel(
  titlePanel("Instructions:"),
  uiOutput(outputId="temp"),
  
  div(actionButton("time", "Run"), style="float:left"),
  actionButton("Grid","Plot Grid"),
  leafletOutput("gridmap"),
  leafletOutput("mymap")
  ))
  
  )

#Ui elije Fecha, Origen, Nº de Puntos y Clave.

