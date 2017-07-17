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
suppressMessages(if(!require(devtools)){install.packages("devtools");library(devtools)}else{library(devtools)})
suppressMessages(if(!require(readr)){install.packages("readr");library(readr)}else{library(readr)})
suppressMessages(if(!require(rmapshaper)){install.packages("rmapshaper");library(rmapshaper)}else{library(rmapshaper)})
suppressMessages( if (!require(geojsonio)) { install.packages("geojsonio");library(geojsonio)}else{library(geojsonio)})
suppressMessages( if (!require(rleafmap)) { install.packages("rleafmap");library(rleafmap)}else{library(rleafmap)})
suppressMessages( if (!require(leaflet.extras)) { devtools::install_github('bhaskarvk/leaflet.extras',force=TRUE);library(leaflet.extras)}else{library(leaflet.extras)})
suppressMessages(if(!require(sf)){install.packages("sf");library(sf)}else{library(sf)})
suppressMessages(if(!require(digest)){install.packages("digest");library(digest)}else{library(digest)})
suppressMessages(if(!require(rdrop2)){install.packages("rdrop2");library(rdrop2)}else{library(rdrop2)})

# Define server logic required to draw map whit leaflet options
outputDir <- getwd()

saveData <- function(data) {
  
  # Create a unique file name
  fileName <- sprintf("%s_%s.shp", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system

 st_write(  x = (data),dsn="fileName")
}








function(input, output,session) {
  RV<-reactiveValues(Clicks=list())  #Store clicks events in a list
  poly<-reactiveValues(poligonos=list())
  
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
                       
      ) %>% addPolygons(data=countries2,color = "#444444", weight = 1, smoothFactor = 0.5,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                        opacity = 1.0, fillOpacity = 0.5,
                        fillColor = colorQuantile("Blues", domain=NULL), label = ~as.character(countries2@data$NAME),layerId = ~countries2@data$NAME,
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE)) 
    
    
    
    
    
  })
  
  #event that save the mouse clicks on the polygons of map 
  ver <-eventReactive(input$mymap3_shape_click,{ click2 <- input$mymap3_shape_click
  
  RV$Clicks<-c(RV$Clicks,click2$id)
  
  feature<-countries2@plotOrder
  name<-countries2@data$NAME
  
  fname<-as.data.frame(cbind(feature,as.character(name)) )
  
  
  poly<-fname[which(fname[,2] %in% RV$Clicks),1]
  
  selected<- countries2[which(countries2@plotOrder%in%poly),]  #list(one=countries[countries@data$NAME=="Oman",],two=countries[countries@data$NAME=="Colombia",]) 
  print(RV$Clicks)
  return(list(selec=selected,clicks=unlist(RV$Clicks)))
  })
  
  
  #actualize map when a ploygon is clicked  and Highlight them
  observe({  proxy <- leafletProxy("mymap3",data=ver()[[1]])
  proxy %>%
    
    addPolygons(
      color = "#444444", weight = 1, smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = 0.5,
      fillColor ="yellow",label=~ver()[[2]] )
  
  })
  
#FOUR PROPOSAL  
  
  #devtools::install_github('rstudio/leaflet',force=TRUE)
  #devtools::install_github('bhaskarvk/leaflet.extras',force=TRUE)
  #library(leaflet)
  #library(leaflet.extras)
  
  
  output$mymap4 <- renderLeaflet({
 
     leaflet("mymap4") %>%
      addProviderTiles(providers$Stamen.TonerLite, #map type or map theme. -default($Stame.TonerLite)
                       options = providerTileOptions(noWrap = TRUE) 
                       
      )%>% addDrawToolbar(
        targetGroup='draw',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
      addLayersControl(overlayGroups = c('draw'), options =
                         layersControlOptions(collapsed=FALSE)) %>%
      addStyleEditor() 
    
   
    
    
  })  
  
 polygons<- eventReactive(input$mymap4_draw_all_features, {
   
   features<-input$mymap4_draw_all_features
   poly$poligonos<-c(poly$poligonos,features)
     
   return(poly$poligonos)
  
  })
  
  
 eventReactive(input$save_poly,{
   stopApp(polygons())
   
 })  
 
 
 #save<-eventReactive(input$save_poly,{ geojson_write(mapita, file = "C:/Users/acmendez/Downloads/hola.txt") })
  
####FIVE PROPOSAL----------------------------------------
 #Create the lefletmap object and add drawToolbar to edit maps
output$mymap5<-renderLeaflet({
 leaflet("mymap5") %>%
   addProviderTiles(providers$Stamen.TonerLite, #map type or map theme. -default($Stame.TonerLite)
                    options = providerTileOptions(noWrap = TRUE) 
                    
   )%>% addDrawToolbar(
     targetGroup= NULL,
     polylineOptions = drawPolylineOptions(repeatMode = TRUE),
     polygonOptions = drawPolygonOptions(repeatMode = TRUE),
     circleOptions = FALSE,
     rectangleOptions = drawRectangleOptions(repeatMode = TRUE),
     markerOptions = FALSE,
     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  
 
})

 #create a reactives list to store drawn objects in environmental obrserver (critical step)
featurelist <- reactiveValues(
  drawn = list(),
  edited_all = list(),
  deleted_all = list(),
  finished = list()
)


recorder <- list()
#set the names of objects where polygons coordinates are stored 

EVT_DRAW <- "mymap5_draw_new_feature"
EVT_EDIT <- "mymap5_draw_edited_features"
EVT_DELETE <- "mymap5_draw_deleted_features"


#call the objects whit the input[[]] function in a observer environment
#this function store the drawn polygons in the reactives lists
observeEvent(input[[EVT_DRAW]], {
  featurelist$drawn <- c(featurelist$drawn, list(input[[EVT_DRAW]]))
  featurelist$finished <- c(featurelist$finished, list(input[[EVT_DRAW]]))
 
})
#this function store edited polygons
observeEvent(input[[EVT_EDIT]], {
  edited <- input[[EVT_EDIT]]
  # find the edited features and update drawn
  # start by getting the leaflet ids to do the match
  ids <- unlist(lapply(featurelist$finished, function(x){x$properties$`_leaflet_id`}))
  # now modify drawn to match edited
  lapply(edited$features, function(x) {
    loc <- match(x$properties$`_leaflet_id`, ids)
    if(length(loc) > 0) {
      featurelist$finished[loc] <<- list(x)
    }
  })

  featurelist$edited_all <- c(featurelist$edited_all, list(edited))
  
})

#this function estore the objects deleted
observeEvent(input[[EVT_DELETE]], {
  deleted <- input[[EVT_DELETE]]
  # find the deleted features and update finished
  # start by getting the leaflet ids to do the match
  ids <- unlist(lapply(featurelist$finished, function(x){x$properties$`_leaflet_id`}))
  # now modify finished to match edited
  lapply(deleted$features, function(x) {
    loc <- match(x$properties$`_leaflet_id`, ids)
    if(length(loc) > 0) {
      featurelist$finished[loc] <<- NULL
    }
  })
  
  featurelist$deleted_all <- c(featurelist$deleted_all, list(deleted))
  
})

###FUNCTION THAT ALLOW CONVERT THE DRAWN OBJECTS TO SPATIALOBJECTS
combine_list_of_sf <- function(sf_list) {
  if(length(sf_list) == 0) {return(NULL)}
  props <- dplyr::bind_rows(
    lapply(
      sf_list,
      function(x) {
        dplyr::select_(
          as.data.frame(x, stringsAsFactors=FALSE),
          paste0("-",attr(x, "sf_column", exact=TRUE))
        )
      }
    )
  )
  
  sf::st_sf(
    props,
    geometry <- sf::st_sfc(
      unlist(lapply(sf_list, function(x) sf::st_geometry(x)), recursive=FALSE)
    ),
    crs = sf::st_crs(4326)
  )
}

st_as_sf.geo_list = function(x, ...) {
  if(x$type != "Feature") {
    stop("should be of type 'Feature'", call.=FALSE)
  }
  
  x <- fix_geojson_coords(x)
  
  #props <- do.call(
  #  data.frame,
  #  modifyList(
  #    Filter(Negate(is.null), x$properties),
  #    list(stringsAsFactors=FALSE)
  #  )
  #)
  
  geom_sf <- st_as_sfc.geo_list(x)
  # if props are empty then we need to handle differently
  #if(nrow(props) == 0 ) {
  #  return(sf::st_sf(feature=geom_sf, crs = sf::st_crs(4326)))
  #} else {
  #  return(sf::st_sf(props, feature=geom_sf, crs = sf::st_crs(4326)))
  #}
}

fix_geojson_coords <- function(ft) {
  
  if(ft$geometry$type == "Point") {
    ft$geometry$coordinates <- unlist(ft$geometry$coordinates)
  }
  
  if(ft$geometry$type == "LineString") {
    ft$geometry$coordinates <- matrix(
      unlist(ft$geometry$coordinates),
      ncol = 2,
      byrow = TRUE
    )
  }
  
  if(!(ft$geometry$type %in% c("Point", "LineString"))) {
    ft$geometry$coordinates <- list(
      matrix(
        unlist(ft$geometry$coordinates),
        ncol = 2,
        byrow = TRUE
      )
    )
  }
  
  ft
}
st_as_sfc.geo_list = function(x, ...) {
  sf::read_sf(
    jsonlite::toJSON(x, auto_unbox=TRUE, force=TRUE)
  )
}

#####3 END FUNCTIONS THAT ALLOW CONVERT TO SPATIAL OBJECTS

#when the buttom "done" is clicked this function
#Create a list containing the objectes drawn, edited and delted  from the leaflet.drawn map

returnlist <- reactive({
  
  workinglist <- list(
    drawn = featurelist$drawn,
    edited = featurelist$edited_all,
    deleted = featurelist$deleted_all,
    finished = featurelist$finished
  )

  # convert the lists to simple features files througth functions created above
  
  workinglist <- lapply(
    workinglist,
    function(action) {
      # ignore empty action types to prevent error
      #   handle in the helper functions?
      if(length(action) == 0) { return() }
      
      # FeatureCollection requires special treatment
      #  and we need to extract features
      features <- Reduce(
        function(left,right) {
          if(right$type == "FeatureCollection") {
            right <- lapply(right$features, identity)
          } else {
            right <- list(right)
          }
          c(left,right)
        },
        action,
        init = NULL
      )
      
      
      
      combine_list_of_sf(
        lapply(features, st_as_sf.geo_list)
      )
    }
  )
  
  
  return(workinglist)
  
})

countriesf <- rgdal::readOGR(dsn = "C:/Users/acmendez/Documents/GitHub/gap_analysis_landraces/Shiny_app", layer='changos')
plot(countriesf)

observeEvent(input$done,{
  
  question<-"first_quest_files"
  
  if(!dir.exists(paste(getwd(),"/",question,sep=""))){dir.create(paste(getwd(),"/",question,sep=""))}
 
   date<-unclass(as.POSIXlt(Sys.time()))
  fileName <- paste("drawn_polygons_quest_1", ".",date$year,"-",date$mon+1,"-",date$mday,"-",date$hour,"_",date$min,".shp",sep="",collapse = "")

 
 filePath <- file.path(paste(getwd(),"/",question,sep=""), fileName)
 
  st_write(st_as_sf(countries),filePath) 
  

  })
  
drop_create(path = "foobar")
drop_dir()
 drop_create()
 
 
 drop_auth()

 drop_acc() %>% 
   select(uid, display_name, email_verified, quota_info.quota)

 token <- drop_auth()
 saveRDS(token, "droptoken.rds")  
 token <- readRDS("droptoken.rds")
 # Then pass the token to each drop_ function
 drop_acc(dtoken = token)
 
 fds<-data.frame(1:200,rnorm(200,0,1),rbinom(200,34,0.5))
 drop_create("carpeta_prueba")
 fileshare<-file.path(tempdir(), "prueba_upload.csv")
 write.csv(fds,file = fileshare,dec=",",sep="\t")
 drop_upload(fileshare, dest= "carpeta_prueba" )
 

 
  

 output$plots<-renderPlot({
   
  
 # print(is(returnlist()$finished))
   plot(returnlist()$finished,col="red")
        
        })
  

 #hacer seguimiento a que esty haciendo
output$text <- renderDataTable({ 

  #input$done
  #loadData()
  is(returnlist()$finished)
#input$mymap4_drawnItems_created
#as.data.frame(ver()[[1]])
#as.data.frame(unlist(polygons()))
 # print(unlist(polygons()$features[[1]]$geometry$coordinates))
  })


}





