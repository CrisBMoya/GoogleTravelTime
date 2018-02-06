#Funciones fuera de Shiny App
{
  library(RCurl)
  library(XML)
  library(leaflet)
  library(stringr)
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(mapview)
  library(tidyverse)
}
#Client Side

ui= fluidPage(
  
  sidebarLayout(
    sidebarPanel(
  
  #Input date in future. System day + 1 day is used to prevent a past time
  textInput(inputId="Date", label="Date", value=Sys.time()),
  textInput(inputId="TimeZone", label="Time Zone in UTC", value=-1),
  textOutput("DEBUG"),
  
  fluidRow(
    box(width = 10, title="Origin point:",
        splitLayout(
          numericInput(inputId="centralPointLat", label="Latitude", 
                       value=-33.4516906, min=-90, max=90, step=0.5),
          numericInput(inputId="centralPointLon", label="Longitude", 
                       value=-70.66742690000001, min=-180, max=180, step=0.5)
        ),
        helpText(a("Find Latitude and Longitude values for specific locations here!", 
                   target="_blank", href="https://www.gps-coordinates.net/"))
        
    )
  ),
  
  

  fluidRow(
    box(width = 10, title="Optional: Grid size", footer="Dont change this, ideally",
        splitLayout(
          numericInput(inputId="LatitudeAxisLength", label="Latitude", value=0.5, min=0.1, max=1),
          numericInput(inputId="LongitudeAxisLength", label="Longitude", value=0.8, min=0.1, max=1)
        )
    )
  ),
  numericInput(inputId="Puntos", label="Points", value=100, min=100, max=500),
  
  textInput(inputId="Key", label="Google API Key"),
  helpText(a("Get your Google Maps API Key here!", target="_blank", 
             href="https://developers.google.com/maps/documentation/javascript/get-api-key?hl=EN"))
    ),
  mainPanel(
  titlePanel("Instructions:"),
  uiOutput(outputId="temp"),
  
  div(actionButton("time", "Run"), style="float:left"),
  actionButton("Grid","Plot Grid"),
  leafletOutput("gridmap"),
  downloadButton("download")
  ))
  )

