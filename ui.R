#Funciones fuera de Shiny App
{
library(RCurl)
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
library(shinydashboard)
  }
#Client Side

ui= fluidPage(
  
  sidebarLayout(
    sidebarPanel(
  
  textInput(inputId="Date", label="Date", value=Sys.time()+86400),
  
  fluidRow(
    box(width = 10, title="Origin point:",
        splitLayout(
          numericInput(inputId="OriginLat", label="Latitude", value=-33.4516906, min=-90, max=90, step=0.5),
          numericInput(inputId="OriginLon", label="Longitude", value=-70.66742690000001, min=-180, max=180, step=0.5)
        ),
        helpText(a("Find Latitude and Longitude values for specific locations here!", target="_blank", href="https://www.gps-coordinates.net/"))
        
    )
  ),
  
  

  fluidRow(
    box(width = 10, title="Optional: Grid size", footer="Dont change this, ideally",
        splitLayout(
          numericInput(inputId="PointMatrixLat", label="Latitude", value=0.5, min=0.1, max=1),
          numericInput(inputId="PointMatrixLon", label="Longitude", value=0.8, min=0.1, max=1)
        )
    )
  ),
  numericInput(inputId="Puntos", label="Points", value=100, min=100, max=500),
  
  textInput(inputId="Key", label="Google API Key"),
  helpText(a("Get your Google Maps API Key here!", target="_blank", href="https://developers.google.com/maps/documentation/javascript/get-api-key?hl=EN"))
    ),
  mainPanel(
  titlePanel("Instructions:"),
  uiOutput(outputId="temp"),
  
  div(actionButton("time", "Run"), style="float:left"),
  actionButton("Grid","Plot Grid"),
  leafletOutput("gridmap")
  ))
  )

