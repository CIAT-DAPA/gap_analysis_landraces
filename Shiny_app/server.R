#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#library(shiny)
#library(shinydashboard)
#library(leaflet)
#library(dplyr)
#library(curl)

suppressMessages(if(!require(shiny)){install.packages("shiny");library(shiny)}else{library(shiny)})
suppressMessages(if(!require(shinydashboard)){install.packages("shinydashboard");library(shinydashboard)}else{library(shinydashboard)})
suppressMessages(if(!require(leaflet)){install.packages("leaflet");library(leaflet)}else{library(leaflet)})
suppressMessages(if(!require(curl)){install.packages("curl");library(curl)}else{library(curl)})
# Define server logic required to draw a histogram
function(input, output,session) {
  RV<-reactiveValues(Clicks=list())
  points<-reactive({ survey[which(survey$Country %in% input$check_1),c(6,7)]   })
  
# points2<-eventReactive(input$submit,{  coor2 <- subset(survey,survey$selec=="Caribbean");coor3<-coor2[which(coor$Country==input$check_1),c(6,7)] ;return(coor3)  })
  

 

  
output$mymap <- renderLeaflet({
  leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLabels,
                     options = providerTileOptions(noWrap = TRUE) 
                     
    ) %>% addPolygons(data=countries,color = "#444444", weight = 1, smoothFactor = 0.5,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                 opacity = 1.0, fillOpacity = 0.5,
                 fillColor = colorQuantile("Blues", domain=NULL), label = ~as.character(countries@data$NAME),layerId = ~countries@data$NAME,
                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                     bringToFront = TRUE)) 
  
  
  
  #%>% addCircles(data = points())
  
})

ver<-eventReactive(input$mymap_shape_click,{ click2 <- input$mymap_shape_click

RV$Clicks<-c(RV$Clicks,click2$id)

feature<-countries@plotOrder
name<-countries@data$NAME

fname<-as.data.frame(cbind(feature,as.character(name)) )


poly<-fname[which(fname[,2] %in% RV$Clicks),1]

selected<- countries[which(countries@plotOrder%in%poly),]  #list(one=countries[countries@data$NAME=="Oman",],two=countries[countries@data$NAME=="Colombia",]) 
print(RV$Clicks)
return(list(selec=selected,clicks=unlist(RV$Clicks)))
})





countries[countries@plotOrder==24,]@data
countries@data$NAME
observe({  proxy <- leafletProxy("mymap",data=ver()[[1]])
proxy %>%
 
  addPolygons(
    color = "#444444", weight = 1, smoothFactor = 0.5,
    opacity = 1.0, fillOpacity = 0.5,
    fillColor ="yellow",label=~ver()[[2]] )})







#poner el marcador donde el mouse haga click
observeEvent(input$mymap_click, {
  click<-input$mymap_click
  cor<- unlist(click)[1:2]
  
  proxy <- leafletProxy("mymap")
  proxy %>%  
    addCircleMarkers(cor[2],cor[1])
  
  #text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
  #print(cor)
  
})



#poner puntos en los paises donde se han seleccionado en la lista de los chekbox
observe({
  
  
  leafletProxy("mymap", data =countries) %>%
    clearMarkers()  %>%
    addCircleMarkers(data = points())
})




#hacer seguimiento a que esty haciendo
output$text <- renderText({ 
 # coor <- subset(survey,survey$selec=="Caribbean");coor[which(coor$Country==input$check_1),c(6,7)]
#input$check_1


unlist(ver()[[2]])
  
  })


}



