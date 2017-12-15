#Server Side
#Functions outside Shiny
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
library(mapview)
  }
#Decode Function
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
APIFunction=function(start,end,time,mode,key){
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
  
  result=RCurl::getURL(wholeUrl, .mapUnicode = FALSE)
  result=htmlParse(result)
  
  {if(!xpathSApply(result, "//directionsresponse/status", xmlValue) == "OK"){
    errorFun=1
    names(errorFun)="time"
    return(errorFun)
  }else
    
    tempLine=decodeLine((xpathSApply(result, "//overview_polyline", xmlValue)))
    time=as.numeric(xpathSApply(result, "//duration_in_traffic/value", xmlValue))/60
    time=c(rep(time, dim(tempLine)[1]))
    time=list(time)
    names(time)="time"
    return(c(time,tempLine))
  }
}


server=function(input, output){


output$temp=renderUI(HTML("<ul>
<li>Enter a date and time, it must be a future date, otherwise it wont work!.</li>
<li>You can change the Origin Point changing the Latitude and Longitude, the mapp will follow.</li>
<li>Choose how many points you want to calculate. 
This value are used to create a grid of points surrounding the Start/Origin point.</li>
<li>Enter your API Key from Google Maps API. Remember! this Key has daily limits to use it for free! 
So don't waste it!</li>
</ul>"))

#Calculatin matrix of points
centralPointv1=reactive({paste(c(input$OriginLat,input$OriginLon), collapse=",")})
latDistv1=reactive({input$PointMatrixLat/2})
lonDistv1=reactive({input$PointMatrixLon/2})

OriginNorthWestv1=reactive({c(latDistv1() + as.numeric(strsplit(centralPointv1(),",")[[1]][1]), as.numeric(strsplit(centralPointv1(),",")[[1]][2]) - lonDistv1())})
OriginSouthEastv1=reactive({c((as.numeric(strsplit(centralPointv1(),",")[[1]][1])) - latDistv1(), lonDistv1() + as.numeric(strsplit(centralPointv1(),",")[[1]][2]))})
CoordMatrix=reactive({c(OriginNorthWestv1(),OriginSouthEastv1())})

output$debug=renderText({OriginNorthWestv1()})

output$gridmap=renderLeaflet({
  p1=leaflet() %>%  addTiles() %>% addProviderTiles(providers$OpenStreetMap) %>%
    setView(lat=input$OriginLat,
            lng=input$OriginLon, zoom=9)
  p1
  })

########
#Sequence from central Y to Southest Point, in 5 steps.
Stepsv2=reactive({round(sqrt(input$Puntos), digits=0)}) #Usar 20 es ideal
centralSouthYv2=reactive({seq(OriginSouthEastv1()[1],OriginNorthWestv1()[1], 
                            by=(OriginSouthEastv1()[1]-OriginNorthWestv1()[1])*-1/Stepsv2())[1:Stepsv2()]})

#Sequence from central X to Westest Point, in 5 steps.
centralWestXv2=reactive({seq(OriginSouthEastv1()[2],OriginNorthWestv1()[2], 
                 by=(OriginSouthEastv1()[2]-OriginNorthWestv1()[2])*-1/Stepsv2())[2:(Stepsv2()+1)]})

#Q3 for Quadrant 3 in cartesian system.
GridCoords=reactive({
coordinatesQ3v2=apply(expand.grid(centralSouthYv2(), centralWestXv2()), 1, paste, collapse=",")
coordinatesQ3v2=as.data.frame(coordinatesQ3v2)
coordinatesQ3v2=data.frame(str_split_fixed(coordinatesQ3v2$coordinatesQ3v2, ",", 2))
coordinatesQ3v2=data.frame(lat=as.numeric(as.character(coordinatesQ3v2$X1)), lng=as.numeric(as.character(coordinatesQ3v2$X2)))
coordinatesQ3v2
})

#######
observeEvent(input$Grid, {
  p1=leafletProxy("gridmap") %>% clearShapes() %>% clearMarkers() %>% removeLayersControl() %>% 
    clearMarkers() %>% clearPopups() %>% clearControls() %>%  addTiles() %>% addProviderTiles(providers$OpenStreetMap) %>%
    setView(lat=as.numeric(strsplit(centralPointv1(),",")[[1]][1]),
            lng=as.numeric(strsplit(centralPointv1(),",")[[1]][2]),
            zoom=9)
  
  p1=addRectangles(p1, lat1=CoordMatrix()[1], lng1=CoordMatrix()[2],
                   lat2=CoordMatrix()[3], lng2=CoordMatrix()[4],
                   fillColor="transparent", color="blue")
  p1=addCircles(p1, lat=GridCoords()$lat, 
                lng=GridCoords()$lng, color="black")
  p1
})


#OUT OF OBSERVE

timeVar=reactive({as.numeric(as.POSIXct(input$Date))})

resultsV1=reactive({
  resultsList <- list()
  withProgress(message="Calculating Time...", value=0, {
    for (i in 1:nrow(GridCoords())){
      incProgress(1/nrow(GridCoords()), detail=paste("Sample Nº", i))
      resultsList[[i]]=as.data.frame(APIFunction(start=c(input$OriginLat,input$OriginLon), end=GridCoords()[i,], time=timeVar(), key=input$Key))
    }
  })
  names(resultsList)=paste("Route",1:length(resultsList), sep="")
  resultsList=resultsList[sapply(resultsList, function(x) x[1,1]<=90 & x[1,1] >= 20)]
  resultsList
  })

colorRang=reactive({
  colorRang=sapply(resultsV1(), function(x) x[1,1])
  colorRang=cut(colorRang, breaks=quantile(colorRang,seq(0,1, length.out=7)), include.lowest=T, labels=F)
  colorRang 
  })

results=reactive({
  resultsV2=mapply(cbind, resultsV1(), "colorRang"=colorRang(), SIMPLIFY=FALSE)
  pal=colorQuantile(c("green","red"), 1:max(unique(colorRang())))
  colorPallete=pal(sapply(resultsV2, function(x) x$colorRang[1]))
  resultsV2=mapply(cbind, resultsV2, "colorPallete"=colorPallete, SIMPLIFY=FALSE)
  resultsV2
  })

quantileInfo=reactive({
quantileInfo=as.data.frame(cbind(sapply(results(), function(x) x[1,1]),sapply(results(), function(x) x[1,4])))
quantileInfo=sapply(split(quantileInfo$V1, quantileInfo$V2), mean)
quantileInfo
})

pal2=reactive({
pal2=colorQuantile(c("green","red"), quantileInfo())
})

groupName=reactive({
  groupName=paste("Quantile nº", sort(unique(colorRang())), sep="")
})

observeEvent(input$time,{
  p1=leafletProxy("gridmap") %>% clearShapes() %>% clearMarkers() %>% clearGroup(c(groupName(),"Info")) %>% 
    clearMarkers() %>% clearPopups() %>% clearControls() %>%
    addTiles() %>% addLayersControl(baseGroups = c(groupName(),"Info")) %>% 
    addProviderTiles(providers$OpenStreetMap)
  for(i in 1:length(results())){
    #Add route lines
    p1=addPolylines(p1, lat=results()[[i]]$lat,
                    lng=results()[[i]]$lon, color = paste(results()[[i]]$colorPallete[1]),
                    group=groupName()[(results()[[i]]$colorRang)[1]]) 
    #Add markers         
    p1=addMarkers(p1,label=paste(round(results()[[i]]$time[1], digits=2),"min"),
                  lat=results()[[i]]$lat[nrow(results()[[i]])],
                  lng=results()[[i]]$lon[nrow(results()[[i]])],
                  group=groupName()[(results()[[i]]$colorRang)[1]],
                  popup=paste("Route nº", results()[[i]]$route[1]))
    
    
  }
  
  #Add info marker         
  p1=addMarkers(p1, group="Info", lat=input$OriginLat,
                lng=input$OriginLon,
                popup = paste("Nº of Points calculated: ", nrow(GridCoords()),"; ",
                              "Nº of Points after filter: ", length(results()),"; ",
                              "Date and time used: ", paste(input$Date, sep=""),"; ",
                              "Central point is HERE.", sep=""))
  
  p1= p1 %>% addLegend("bottomright", colors=pal2()(quantileInfo()),
                       labels=round(quantileInfo(), digits = 2),
                       title="Average Travel Time")
  
  for(i in sort(unique(colorRang()))){
    p1=addAwesomeMarkers(p1, group=groupName()[i],
                         lat=input$OriginLat, 
                         lng=input$OriginLon,
                         icon = awesomeIcons(
                           library = 'glyphicon',
                           icon = "download",
                           markerColor = "black"))}
  p1
  
})
}


