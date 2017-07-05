#SHINY APP FOR ASK EXPERTS THE PRESENCE OR AUSENCE OF LANDRACER CROPS IN THE WORLD
#Andres Camilo Mendez
#
#
# This is the server logic of a Shiny web application
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#import packages
suppressMessages(if(!require(shiny)){install.packages("shiny");library(shiny)}else{library(shiny)})
suppressMessages(if(!require(shinydashboard)){install.packages("shinydashboard");library(shinydashboard)}else{library(shinydashboard)})
suppressMessages(if(!require(leaflet)){install.packages("leaflet");library(leaflet)}else{library(leaflet)})
suppressMessages(if(!require(curl)){install.packages("curl");library(curl)}else{library(curl)})


# Define server logic required to draw map whit leaflet options


function(input, output,session) {
  RV<-reactiveValues(Clicks=list())  #Store clicks events in a list
  
  
  #create a markes in the map when checkbox is selected
  points<-reactive({ survey[which(survey$Country %in% input$check_1),c(6,7)]   }) 
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, #map type or map theme. -default($Stame.TonerLite)
                       options = providerTileOptions(noWrap = TRUE) 
                       
      )%>%  addCircleMarkers(data = points())
    
  })
  
  
  # put points in the map when countries checkbox is selected 
  observe({
    
    
    leafletProxy("mymap", data =survey) %>%
      clearMarkers()  %>%
      addCircleMarkers(data = points())
  })
  
  
 
 
  
  #SECOND PROPOSAL
  
  #create a emptu leaflet map
  output$mymap2 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, #map type or map theme. -default($Stame.TonerLite)
                       options = providerTileOptions(noWrap = TRUE) 
                       
      )%>%  clearMarkers()
    
  })
  
  
   #put markes where mouse is clicked
  #leaflet proxy Upgrade the map 
  observeEvent(input$mymap2_click, {
    click<-input$mymap2_click
    cor<- unlist(click)[1:2]
    
    proxy <- leafletProxy("mymap2") 
    proxy %>%  
      addCircleMarkers(cor[2],cor[1])
    
  })
  

  #THIRD PROPOSAL
 
#build the leaflet map and add it ploygons 
  
  output$mymap3 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLabels, #map type or map theme. -default($Stame.TonerLite)
                       options = providerTileOptions(noWrap = TRUE) 
                       
      ) %>% addPolygons(data=countries,color = "#444444", weight = 1, smoothFactor = 0.5,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                        opacity = 1.0, fillOpacity = 0.5,
                        fillColor = colorQuantile("Blues", domain=NULL), label = ~as.character(countries@data$NAME),layerId = ~countries@data$NAME,
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE)) 
    
    
    
    
    
  })
  
  #event that save the mouse clicks on the polygons of map 
  ver <-eventReactive(input$mymap3_shape_click,{ click2 <- input$mymap3_shape_click
  
  RV$Clicks<-c(RV$Clicks,click2$id)
  
  feature<-countries@plotOrder
  name<-countries@data$NAME
  
  fname<-as.data.frame(cbind(feature,as.character(name)) )
  
  
  poly<-fname[which(fname[,2] %in% RV$Clicks),1]
  
  selected<- countries[which(countries@plotOrder%in%poly),]  #list(one=countries[countries@data$NAME=="Oman",],two=countries[countries@data$NAME=="Colombia",]) 
  print(RV$Clicks)
  return(list(selec=selected,clicks=unlist(RV$Clicks)))
  })
  
  
  #actualize map when a ploygon is clicked  and Highlight them
  observe({  proxy <- leafletProxy("mymap3",data=ver()[[1]])
  proxy %>%
    
    addPolygons(
      color = "#444444", weight = 1, smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = 0.5,
      fillColor ="yellow",label=~ver()[[2]] )})
  


#hacer seguimiento a que esty haciendo
output$text <- renderText({ 
 # coor <- subset(survey,survey$selec=="Caribbean");coor[which(coor$Country==input$check_1),c(6,7)]
#input$check_1


unlist(ver()[[2]])
  
  })


}



