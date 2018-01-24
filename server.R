#Server Side
#Functions outside Shiny
#Libraries

{
library(RCurl)
library(XML)
library(leaflet)
library(stringr)
library(shiny)
library(shinyjs)
library(shinydashboard)
  }

#Functions
{#Decode Function
  decodeLine <- function(encoded){
    require(bitops)
    
    vlen <- nchar(encoded)
    vindex <- 0
    varray <- NULL
    vlat <- 0
    vlng <- 0
    
    while(vindex < vlen){
      vb <- NULL
      vshift <- 0
      vresult <- 0
      repeat{
        if(vindex + 1 <= vlen){
          vindex <- vindex + 1
          vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63  
        }
        
        vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
        vshift <- vshift + 5
        if(vb < 32) break
      }
      
      dlat <- ifelse(
        bitAnd(vresult, 1)
        , -(bitShiftR(vresult, 1)+1)
        , bitShiftR(vresult, 1)
      )
      vlat <- vlat + dlat
      
      vshift <- 0
      vresult <- 0
      repeat{
        if(vindex + 1 <= vlen) {
          vindex <- vindex+1
          vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63        
        }
        
        vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
        vshift <- vshift + 5
        if(vb < 32) break
      }
      
      dlng <- ifelse(
        bitAnd(vresult, 1)
        , -(bitShiftR(vresult, 1)+1)
        , bitShiftR(vresult, 1)
      )
      vlng <- vlng + dlng
      
      varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
    }
    coords <- data.frame(varray)
    names(coords) <- c("lat", "lon")
    coords
  }
  
  #API Function
  APIFunction=function(start,end,time,mode,key,debug=FALSE){
    
    #Base code
    urlBase="https://maps.googleapis.com/maps/api/directions/xml?"
    
    #User defined
    startPoint=as.character(paste(start, collapse = ","))
    endPoint=as.character(paste(end, collapse=","))
    mode="driving"
    traffic="best_guess"
    time=time
    key=key
    
    #Creating the whole request url
    wholeUrl=paste(urlBase,"origin=",startPoint,
                   "&destination=",endPoint,
                   "&mode=",mode,
                   "&traffic_model=",traffic,
                   "&departure_time=",time,
                   "&key=",key, sep="")
    
    if(debug==TRUE){
      return(wholeUrl)
    }else 
      Query1=try(RCurl::getURL(wholeUrl, .mapUnicode = FALSE))
    Query2=grepl(pattern="error", x=Query1, ignore.case=TRUE)
    Query3=try(xpathSApply((htmlParse(Query1)),
                           "//directionsresponse/status", xmlValue))
    
    if(Query2==TRUE){
      errorFun=0
      names(errorFun)="Error with getURL"
      return(c(errorFun, Query2))
    }else if(Query3 != "OK"){
      errorFun=1
      names(errorFun)="Error with Google Response"
      return(c(errorFun, Query3))
    }else{
      tempLine=decodeLine((xpathSApply((htmlParse(Query1)),
                                       "//overview_polyline", xmlValue)))
      time=as.numeric(xpathSApply((htmlParse(Query1)),
                                  "//duration_in_traffic/value", xmlValue))/60
      distance=max(as.numeric(xpathSApply((htmlParse(Query1)),
                                          "//distance/value", xmlValue))/1000)
      return(c(time, distance, tempLine, Query1))
    }}
}

#Beggining of shiny
server=function(input, output){

#Instructions
output$temp=renderUI(HTML("<ul>
<li>Enter a date and time, it must be a future date, otherwise it wont work!.</li>
<li>You can change the Origin Point changing the Latitude and Longitude, the mapp will follow.</li>
<li>Choose how many points you want to calculate. 
This value are used to create a grid of points surrounding the Start/Origin point.</li>
<li>Enter your API Key from Google Maps API. Remember! this Key has daily limits to use it for free! 
So don't waste it!</li>
</ul>"))

#Calculatin matrix of points
output$gridmap=renderLeaflet({
  p1=leaflet() %>%  addTiles() %>% addProviderTiles(providers$OpenStreetMap) %>%
    setView(lat=input$centralPointLat,
            lng=input$centralPointLon, zoom=9)
  p1
  })

########
##SET TIME
StartingDayReac=reactive({as.numeric(as.POSIXct(as.character(input$Date), tz="UTC"))})
TimeZoneReac=reactive({as.numeric(input$TimeZone)})
DayTimeReac=reactive({StartingDayReac()+86400-(TimeZoneReac()*3600)})
##SET ORIGIN
centralPointReac=reactive({c(input$centralPointLat,input$centralPointLon)})
##NUMERO DE PUNTOS    
NPointsGridReac=reactive({input$Puntos})
NPointsReac=reactive({round(sqrt(NPointsGridReac()), digits=0)})

#Custom Matrix
#Choosing size of Point Gird relative to the central Point. 
#X=Longitude, Horizontal; Y=Latitude, Vertical
LongitudeAxisLengthReac=reactive({input$LongitudeAxisLength})
LatitudeAxisLengthReac=reactive({input$LatitudeAxisLength})


#Extend Grid from central point to the outer parts. MIND THE ORDER OF FACTORS
OriginYAxisExtendReac=reactive({c((LatitudeAxisLengthReac()/2)+as.numeric(centralPointReac()[1]),as.numeric(centralPointReac()[2])-(LongitudeAxisLengthReac()/2))})
OriginXAxisExtendReac=reactive({c(as.numeric(centralPointReac()[1])-(LatitudeAxisLengthReac()/2),(LongitudeAxisLengthReac()/2) + as.numeric(centralPointReac()[2]))})

#Generating the sequence of points, the actual Grid of points.

##Sequential steps from the center to the Southest Point (Longitude, lowest point)
SeqAlongLatitudeReac=reactive({seq(OriginXAxisExtendReac()[1],OriginYAxisExtendReac()[1], 
                     by=(OriginXAxisExtendReac()[1]-OriginYAxisExtendReac()[1])*-1/NPointsReac())[1:NPointsReac()]})
SeqAlongLongitudeReac=reactive({seq(OriginXAxisExtendReac()[2],OriginYAxisExtendReac()[2], 
                      by=(OriginXAxisExtendReac()[2]-OriginYAxisExtendReac()[2])*-1/NPointsReac())[2:(NPointsReac()+1)]})

CoordinateMatrixReac=reactive({
CoordinateMatrix=apply(expand.grid(SeqAlongLatitudeReac(), SeqAlongLongitudeReac()), 1, paste, collapse=",")
CoordinateMatrix=as.data.frame(CoordinateMatrix)
CoordinateMatrix=data.frame(str_split_fixed(CoordinateMatrix$CoordinateMatrix, ",", 2))
CoordinateMatrix=data.frame(lat=as.numeric(as.character(CoordinateMatrix$X1)), 
                            lng=as.numeric(as.character(CoordinateMatrix$X2)))
})



#Plotting a rectangle and every point to be calculated by the API
observeEvent(input$Grid, {
  p1=leafletProxy("gridmap") %>% clearShapes() %>% clearMarkers() %>% removeLayersControl() %>% 
    clearMarkers() %>% clearPopups() %>% clearControls() %>%  addTiles() %>% addProviderTiles(providers$OpenStreetMap) %>%
    setView(lat=as.numeric(centralPointReac()[1]),
            lng=as.numeric(centralPointReac()[2]),
            zoom=9)
  p1=addCircles(p1, lat=CoordinateMatrixReac()$lat,
                        lng=CoordinateMatrixReac()$lng, 
                        popup=paste(1:nrow(CoordinateMatrixReac())))
  p1=addMarkers(p1, lat=centralPointReac()[1],
                        lng=centralPointReac()[2]) 
})

#Running API
APIResults=reactive({

  resFuture <- list()
  withProgress(message="Calculating Time...", value=0, {
    for(routes in 1:nrow(CoordinateMatrixReac())){
      incProgress(1/nrow(CoordinateMatrixReac()), detail=paste("Sample Nº", routes))
      resFuture[[routes]]=APIFunction(start=c(centralPointReac()[1],centralPointReac()[2]), 
                                end=CoordinateMatrixReac()[routes,], 
                                time=DayTimeReac(), key=input$Key)
    }

    })
  names(resFuture)=paste("Route",1:nrow(CoordinateMatrixReac()), sep="")
  resFuture
  })

#Filtering Errors
DayListReac=reactive({lapply(APIResults(), function(x) x[length(x)==5])})

#Renaming all data
DayListv2Reac=reactive({lapply(DayListReac(), function(x){names(x)=c("time","distance","lat","lon","xml"); return(x)})})

TimeStoreReac=reactive({unlist(lapply(DayListv2Reac(), function(x) x$time))})
Breaks=4
TimeRangeReac=reactive({(max(TimeStoreReac())-min(TimeStoreReac()))/Breaks})


BreaksListReac=reactive({
BreaksList=list()
for(i in 1:Breaks){
  if(i==1){
    BreaksList[[i]]=c(min(TimeStoreReac()),min(TimeStoreReac())+TimeRangeReac()*i)
  }else if(i==Breaks){
    BreaksList[[i]]=c(min(TimeStoreReac())+TimeRangeReac()*(i-1),max(TimeStoreReac()))
  }else{
    BreaksList[[i]]=c(min(TimeStoreReac())+TimeRangeReac()*(i-1),min(TimeStoreReac())+TimeRangeReac()*i)
  }}
names(BreaksList)=paste0("Break_", 1:Breaks)

BreaksList
})
BreakLegendReac=reactive({unique(unlist(BreaksListReac()))[1:(length(unique(unlist(BreaksListReac())))-1)]})

timeRanking=function(x){
  for(i in 1:Breaks){
    if(x > BreaksListReac()[[i]][1] & x < BreaksListReac()[[i]][2]){
      return(i)
    }
  }}

RanksTimeReac=reactive({lapply(DayListv2Reac(), function(x) timeRanking(x$time))})

#You can change the Pallete. Check RColorBrewer
colorFun=colorNumeric(palette="BuPu", c(1:(Breaks*2)))
#colorFun=colorNumeric(c("#00ffff","#008080","#3987C0","#08306B"), c(1:4))
ColorReac=reactive({lapply(RanksTimeReac(), function(x) colorFun(x*2))})

#Adding color to every route
routeNumberReac=reactive({length(DayListv2Reac())})

DayListv4Reac=reactive({
  DayListv3=DayListv2Reac()
for(routes in 1:routeNumberReac()){
  DayListv3[[routes]]$Color=ColorReac()[[routes]]
}
  DayListv3
})

#output$DEBUG=renderPrint({DayListv4Reac()[[1]]})

#PLOT
#ALTERNATIVE MAPPING
observeEvent(input$time, {
 
p1=leafletProxy("gridmap") %>% clearShapes() %>% clearMarkers() %>%
  clearMarkers() %>% clearPopups() %>% clearControls() %>%
  addTiles() %>% addLayersControl(overlayGroups="Route Polylines",
                                  options= layersControlOptions(collapsed=FALSE)) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  hideGroup("Route Polylines")

for(routes in 1:routeNumberReac()){
  #Add route lines
  p1=addPolylines(p1,
                  lat=DayListv4Reac()[[routes]]$lat,
                  lng=DayListv4Reac()[[routes]]$lon,
                  group="Route Polylines",
                  color=DayListv4Reac()[[routes]]$Color,
                  popup = paste(round(DayListv4Reac()[[routes]]$time, digits=2), "minutes;",
                                round(DayListv4Reac()[[routes]]$distance, digits=2),"km"),
                  highlight=highlightOptions(weight=5,
                                             color="#ffff00",
                                             dashArray="",
                                             fillOpacity=0.7,
                                             bringToFront=TRUE)
  )
  
  #Points
  p1=addCircles(p1,
                lat=DayListv4Reac()[[routes]]$lat[length(DayListv4Reac()[[routes]]$lat)],
                lng=DayListv4Reac()[[routes]]$lon[length(DayListv4Reac()[[routes]]$lon)],
                color="black", fillColor="black",
                group="Route Polylines", radius=50
  )
}

#Add info marker         
p1=addMarkers(p1, group="Route Polylines", lat=as.numeric(centralPointReac()[1]), 
              lng=as.numeric(centralPointReac()[2]),
              popup = paste("Central point is HERE", sep=""))


p1= p1 %>% addLegend("bottomright", colors=colorFun(c(1:Breaks*2)),
                     labels=paste0(">", round(BreakLegendReac(), digits=2),"min"),
                     title="Travel Time Quantile")
p1
})
 }


