#Funciones fuera de Shiny App
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
#Client Side
ui= fluidPage(
  
  
  sidebarLayout(
    sidebarPanel(
  textInput(inputId="Date", label="Date", value="2017-11-20 19:00"),
  textInput(inputId="Origin", label="Origin", value=paste(c(-33.4516906,-70.66742690000001), collapse=",")),
  numericInput(inputId="Puntos", label="Puntos", value=10, min=10, max=500),
  textInput(inputId="Key", label="Google API Key", value=""),
  helpText(a("Get your Google Maps API Key here!", target="_blank", href="https://developers.google.com/maps/documentation/javascript/get-api-key?hl=EN"))
    ),
  mainPanel(
  titlePanel("Instructions:"),
  textOutput(outputId="Progress2"),
  uiOutput(outputId="temp"),
  
  div(actionButton("time", "Run"), style="float:left"),
  conditionalPanel(condition= "output.Progress2", 
                   div(actionButton("plot", "Plot"), style="float:left"))
 
  )),
  leafletOutput("mymap")
  )

#Ui elije Fecha, Origen, Nº de Puntos y Clave.

