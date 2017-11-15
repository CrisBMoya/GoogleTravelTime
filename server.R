#Server Side
#Funciones fuera de Shiny App
#A change
#Another Change

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
<li>Enter a date, it must be a future date, otherwise it wont work!.</li>
<li>Enter a Start/Origin Point in the form of 'Latitute,Longitude'.</li>
<li>Choose how many points you want to calculate. 
This value are used to create a grid of points surrounding the Start/Origin point.</li>
<li>Enter your API Key from Google Maps API. Remember! this Key has daily limits to use it for free! 
So don't waste it!</li>
</ul>"))

TravelTimeFUN=eventReactive(input$time,{

#Central Point
centralPoint=input$Origin

#Full Grid from central point
DestinationNorthWest=c(-33.22490308626395,-71.04721069335938)

#Origin in South-East
OriginSouthEast=c(-33.74032885072381,-70.24246215820312)

#Sequence from central Y to Southest Point, in 5 steps.
Steps=round(sqrt(input$Puntos), digits=0) #Usar 20 es ideal
centralSouthY=seq(OriginSouthEast[1],DestinationNorthWest[1], by=(OriginSouthEast[1]-DestinationNorthWest[1])*-1/Steps)[1:Steps]

#Sequence from central X to Westest Point, in 5 steps.
centralWestX=seq(OriginSouthEast[2],DestinationNorthWest[2], by=(OriginSouthEast[2]-DestinationNorthWest[2])*-1/Steps)[2:(Steps+1)]

#Q3 for Quadrant 3 in cartesian system.
coordinatesQ3=apply(expand.grid(centralSouthY, centralWestX), 1, paste, collapse=",")
coordinatesQ3=as.data.frame(coordinatesQ3)
coordinatesQ3=data.frame(str_split_fixed(coordinatesQ3$coordinatesQ3, ",", 2))
coordinatesQ3=data.frame(lat=as.numeric(as.character(coordinatesQ3$X1)), lng=as.numeric(as.character(coordinatesQ3$X2)))

#Iterating Time of travel function over number of coordinates generated
results <- list()
timeVar=as.numeric(as.POSIXct(input$Date))

withProgress(message="Calculating Time", value=0, {
 for (i in 1:nrow(coordinatesQ3)){
   incProgress(1/nrow(coordinatesQ3), detail=paste("Sample Nº", i))
   results[[i]]=as.data.frame(APIFunction(start=centralPoint, end=coordinatesQ3[i,], time=timeVar, key=input$Key))
  }
})

names(results)=paste("Route",1:length(results), sep="")

#Only values under certain time
before=length(results)
results=results[sapply(results, function(x) x[1,1]<=90 & x[1,1] >= 20)]
colorRang=sapply(results, function(x) x[1,1])
colorRang=cut(colorRang, breaks=quantile(colorRang,seq(0,1, length.out=7)), include.lowest=T, labels=F)
results=mapply(cbind, results, "colorRang"=colorRang, SIMPLIFY=FALSE)
after=length(results)

#Formatting palletes, colors and groups for further plotting.
pal=colorQuantile(c("green","red"), 1:max(unique(colorRang)))
colorPallete=pal(sapply(results, function(x) x$colorRang[1]))
results=mapply(cbind, results, "colorPallete"=colorPallete, SIMPLIFY=FALSE)

quantileInfo=as.data.frame(cbind(sapply(results, function(x) x[1,1]),sapply(results, function(x) x[1,4])))
quantileInfo=sapply(split(quantileInfo$V1, quantileInfo$V2), mean)

pal2=colorQuantile(c("green","red"), quantileInfo)
groupName=paste("Quantile nº", sort(unique(colorRang)), sep="")



list(groupName=groupName, 
     results=results, 
     coordinatesQ3=coordinatesQ3, 
     pal2=pal2, 
     quantileInfo=quantileInfo,
     colorRang=colorRang,
     debugList=" ")
      
      })
    
    

groupName=reactive({TravelTimeFUN()$groupName})
results=reactive({TravelTimeFUN()$results})
coordinatesQ3=reactive({TravelTimeFUN()$coordinatesQ3})
pal2=reactive({TravelTimeFUN()$pal2})
quantileInfo=reactive({TravelTimeFUN()$quantileInfo})
colorRang=reactive({TravelTimeFUN()$colorRang})
debugList=reactive({TravelTimeFUN()$debugList})

output$Progress2=renderText({debugList()})


plotTemp=eventReactive(input$plot, {
  p1=leaflet() %>% addTiles() %>% addLayersControl(baseGroups = c(groupName(),"Info")) %>% 
    addProviderTiles(providers$OpenStreetMap)
  for(i in 1:length(results())){
    #Add route liens
    p1=addPolylines(p1, lat=results()[[i]]$lat,
                    lng=results()[[i]]$lon, color = paste(results()[[i]]$colorPallete[1]),
                    group=groupName()[(results()[[i]]$colorRang)[1]]) 
    #Add markers         
    p1=addMarkers(p1,label=paste(round(results()[[i]]$time[1], digits=2),"min"),
                  lat=results()[[i]]$lat[nrow(results()[[i]])],
                  lng=results()[[i]]$lon[nrow(results()[[i]])],
                  group=groupName()[(results()[[i]]$colorRang)[1]],
                  popup=paste("Route nº", results()[[i]]$route[1]))
    
    #cat("\r", paste("Plot Nº", i))
  }
  
  #Add info marker         
  p1=addMarkers(p1, group="Info", lat=as.numeric(strsplit(input$Origin,",")[[1]][1]),
                lng=as.numeric(strsplit(input$Origin,",")[[1]][2]),
                popup = paste("Nº of Points calculated: ", nrow(coordinatesQ3()),"; ",
                              "Nº of Points after filter: ", length(results()),"; ",
                              "Date and time used: ", paste(input$Date, sep=""),"; ",
                              "Central point is HERE.", sep=""))
  
  p1= p1 %>% addLegend("bottomright", colors=pal2()(quantileInfo()),
                       labels=round(quantileInfo(), digits = 2),
                       title="Minuntos de viaje promedio")
  
  for(i in sort(unique(colorRang()))){
    p1=addAwesomeMarkers(p1, group=groupName()[i],
                         lat=as.numeric(strsplit(input$Origin,",")[[1]][1]), 
                         lng=as.numeric(strsplit(input$Origin,",")[[1]][2]), icon = awesomeIcons(
                           library = 'glyphicon',
                           icon = "download",
                           markerColor = "black"))}
p1

})

output$mymap=renderLeaflet({plotTemp()})

}


