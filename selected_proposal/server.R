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
suppressMessages(if(!require(zip)){install.packages("zip");library(zip)}else{library(zip)})
suppressMessages(if(!require(rsconnect)){install.packages("rsconnect");library(rsconnect)}else{library(rsconnect)})
suppressMessages(if(!require(shinyBS)){inastall.packages("shinyBS");library(shinyBS)}else{library(shinyBS)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} else {library(dplyr)})


# Define server logic required to draw map whit leaflet option


####importantisisismo

#token <- drop_auth()
#saveRDS(token, "droptoken.rds")

countries2<-readOGR(dsn="world_shape_simplified",layer="all_countries_simplified")




token <- readRDS("droptoken.rds")

saveData_1<-function(data,nombre,n.quest,n.clicks,crop,texto){
  
  if(length(data)!= 0){
 
  if(n.clicks>=2){
   if(drop_exists(paste("survey","_",nombre,"_",crop,"/",n.quest,sep=""),dtoken=token)){ drop_delete(paste("survey","_",nombre,"_",crop,"/",n.quest,sep=""),dtoken=token)}

  }
  
  token <- readRDS("droptoken.rds")
  
  if(!drop_exists(paste("survey","_",nombre,"_",crop,sep=""),dtoken=token)){drop_create(paste("survey","_",nombre,"_",crop,sep=""),dtoken = token)}
  
  if(!drop_exists(paste("survey","_",nombre,"_",crop,"/",n.quest,sep=""),dtoken=token)){drop_create(paste("survey","_",nombre,"_",crop,"/",n.quest,sep=""))}
  
  create<-file.path(tempdir(),paste("shapefiles","_",nombre,"_",crop,sep=""))
 
 if(!dir.exists(create)){ dir.create(create,showWarnings = TRUE)} 
  
  
  fileName <- paste("dp",n.quest,"_",crop,"_",nombre,".shp",sep="",collapse = "")
  
  nameText<-paste("dp",n.quest,"_",crop,"_",nombre,"_comments",".txt",sep="",collapse = "")
  
  
  
   filePath <- file.path(create,fileName)
  
   filecon<-file(file.path(create,nameText))
   writeLines(texto,filecon)
  close(filecon)
  
  
  
  st_write((data),filePath)
  
  
  files<-list.files(create)
  
  drop_acc(dtoken = token)    
  for(i in 1:length(files)){
    
    drop_upload(file.path(create,files[i]), dest = paste("survey","_",nombre,"_",crop,"/",n.quest,sep=""),dtoken=token) 
    
    
  }
 
  unlink(create, recursive=TRUE)
     
   
  }else{print("sorry bro")}
  
 

}



savePlots<-function(data,nombre,countries=countries2,col="red",n.quest,crop){

  
  create<-file.path(tempdir(),paste("plots","_",nombre,"_",crop,sep=""))
  if(!dir.exists(create)){dir.create(create,showWarnings = TRUE)}
  fileName<-paste("plot_",nombre,"_",crop,"_",gsub(" ","",n.quest),".png",sep="")
  
  png(file.path(create,fileName))
  plot(countries)
 plot(data,col=col,add=TRUE)
  dev.off()
  token <- readRDS("droptoken.rds")
  drop_acc(dtoken = token)
  files<-list.files(create)

  for(i in 1:length(files)){
    drop_upload(file.path(create,files[i]),dest= paste("survey","_",nombre,"_",crop,sep=""),dtoken=token)
    
  }
  unlink(create, recursive=TRUE)
}




function(input, output,session) {
  
  
  
  

 
  
  observeEvent(input$keep,{
   
     if(input$crop!="-- Please select crop --" && nchar(input$nombre)!=0 ){
    updateTabsetPanel(session, "tabset1",
                      selected = "1. Cultivars")
    
    }else{showModal(modalDialog(
      title = "Oops something went wrong:",
      h3("May be you forgot to select a crop variety or  write a name"),footer = modalButton("OK"),easyClose = TRUE
    ))}
    
    
    
    
  })
  
  
####FIVE PROPOSAL----------------------------------------
 #Create the lefletmap object and add drawToolbar to edit maps
output$mymap5<-renderLeaflet({
 leaflet("mymap5") %>%
   addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stame.TonerLite)
                    options = providerTileOptions(noWrap = TRUE) 
                    
   )%>% addDrawToolbar(
     targetGroup= NULL,
     polylineOptions = FALSE,
     polygonOptions = drawPolygonOptions(repeatMode = TRUE),
     circleOptions = FALSE,
     rectangleOptions = drawRectangleOptions(repeatMode=TRUE),
     markerOptions = FALSE,
     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>% setView(10,10,zoom=2)
 
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









observeEvent(input$done,{
 

 
 
  saveData_1(returnlist()$finished,gsub(" ", "", input$nombre),gsub(" ", "",input$tabset1),(input$done), gsub(" ", "",input$crop),input$txt1)
  
 
   if(length(returnlist()$finished)!=0){
  updateButton(session, "done",label = "Stored",style = "success")
     updateTabsetPanel(session, "tabset1",
                       selected = "2. Landraces")
     
   }
  
 
})


output$plot1<-renderPlot({
  
  if(length(returnlist()$finished)!=0){
  plot(countries2)
  plot(returnlist()$finished,col="blue",add=TRUE)
  }else{return(NULL)}
  
})


# Then pass the token to each drop_ function



  

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


######SEGUNDA PREGUNTA###########


output$mymap6<-renderLeaflet({
  leaflet("mymap6") %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stame.TonerLite)
                     options = providerTileOptions(noWrap = TRUE) 
                     
    )%>% addDrawToolbar(
      targetGroup= NULL,
      polylineOptions = FALSE,
      
      polygonOptions = drawPolygonOptions(repeatMode = TRUE,shapeOptions = drawShapeOptions(color = "red", weight = 1, opacity = 1,
                                                                                           fill = TRUE, fillColor = "red", fillOpacity = 0.4)),
      circleOptions = FALSE,
      rectangleOptions = drawRectangleOptions(repeatMode=TRUE,shapeOptions = drawShapeOptions(color = "red", weight = 1, opacity = 1,
                                                                                           fill = TRUE, fillColor = "red", fillOpacity = 0.4)),
      markerOptions = FALSE,
      editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>% setView(10,10,zoom=2)
  
})

featurelist2 <- reactiveValues(
  drawn = list(),
  edited_all = list(),
  deleted_all = list(),
  finished = list()
)


recorder2 <- list()

EVT_DRAW2 <- "mymap6_draw_new_feature"
EVT_EDIT2 <- "mymap6_draw_edited_features"
EVT_DELETE2 <- "mymap6_draw_deleted_features"


#call the objects whit the input[[]] function in a observer environment
#this function store the drawn polygons in the reactives lists
observeEvent(input[[EVT_DRAW2]], {
  featurelist2$drawn <- c(featurelist2$drawn, list(input[[EVT_DRAW2]]))
  featurelist2$finished <- c(featurelist2$finished, list(input[[EVT_DRAW2]]))
  
})
#this function store edited polygons
observeEvent(input[[EVT_EDIT2]], {
  edited <- input[[EVT_EDIT2]]
  # find the edited features and update drawn
  # start by getting the leaflet ids to do the match
  ids <- unlist(lapply(featurelist2$finished, function(x){x$properties$`_leaflet_id`}))
  # now modify drawn to match edited
  lapply(edited$features, function(x) {
    loc <- match(x$properties$`_leaflet_id`, ids)
    if(length(loc) > 0) {
      featurelist2$finished[loc] <<- list(x)
    }
  })
  
  featurelist2$edited_all <- c(featurelist2$edited_all, list(edited))
  
})

#this function estore the objects deleted
observeEvent(input[[EVT_DELETE2]], {
  deleted <- input[[EVT_DELETE2]]
  # find the deleted features and update finished
  # start by getting the leaflet ids to do the match
  ids <- unlist(lapply(featurelist2$finished, function(x){x$properties$`_leaflet_id`}))
  # now modify finished to match edited
  lapply(deleted$features, function(x) {
    loc <- match(x$properties$`_leaflet_id`, ids)
    if(length(loc) > 0) {
      featurelist2$finished[loc] <<- NULL
    }
  })
  
  featurelist2$deleted_all <- c(featurelist2$deleted_all, list(deleted))
  
})



returnlist2 <- reactive({
  
  workinglist <- list(
    drawn = featurelist2$drawn,
    edited = featurelist2$edited_all,
    deleted = featurelist2$deleted_all,
    finished = featurelist2$finished
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






observeEvent(input$done2,{
  
  
  saveData_1(returnlist2()$finished,gsub(" ", "", input$nombre),gsub(" ", "", input$tabset1),(input$done2), gsub(" ", "",input$crop),input$txt2)
  
  if(length(returnlist2()$finished)!=0){
    updateButton(session, "done2",label = "Stored",style = "success")}
  updateTabsetPanel(session, "tabset1",
                    selected = "3. Collecting")

})

output$plot2<-renderPlot({
  if(length(returnlist2()$finished)!=0){
    plot(countries2)
    plot(returnlist2()$finished,col="red",add=TRUE)
  }else{return(NULL)}
  
})

####-----TERCERA PREGUNTA ---------####
##-----------------------------------##
###                                 ###
##                                   ## 
#-------------------------------------#

output$mymap7<-renderLeaflet({
 
  if(length(returnlist2()$finished)!=0){
  
   leaflet("mymap7") %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stamen.TonerLite)
                     options = providerTileOptions(noWrap = TRUE) 
                     
    )%>% addPolygons(data=returnlist2()$finished,color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                            opacity = 0.5, fillOpacity = 0.1,
                                                            fillColor = colorQuantile("Blues", domain=NULL))    %>% 
    
  
    
    addDrawToolbar(
      targetGroup= NULL,
      polylineOptions = FALSE,
      
      polygonOptions = drawPolygonOptions(repeatMode = TRUE,shapeOptions = drawShapeOptions(color = "orange", weight = 1, opacity = 1,
                                                                                            fill = TRUE, fillColor = "orange", fillOpacity = 0.4)),
      circleOptions = FALSE,
      rectangleOptions = drawRectangleOptions(repeatMode=TRUE,shapeOptions = drawShapeOptions(color = "orange", weight = 1, opacity = 1,
                                                                                              fill = TRUE, fillColor = "orange", fillOpacity = 0.4)),
      markerOptions = FALSE,
      editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>% setView(10,10,zoom=2)
 
  }else{leaflet("mymap7") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stamen.TonerLite)
                       options = providerTileOptions(noWrap = TRUE) 
                       
      )%>% 
      addDrawToolbar(
        targetGroup= NULL,
        polylineOptions = FALSE,
        
        polygonOptions = drawPolygonOptions(repeatMode = TRUE,shapeOptions = drawShapeOptions(color = "orange", weight = 1, opacity = 1,
                                                                                              fill = TRUE, fillColor = "orange", fillOpacity = 0.5)),
        circleOptions = FALSE,
        rectangleOptions = drawRectangleOptions(repeatMode=TRUE,shapeOptions = drawShapeOptions(color = "orange", weight = 1, opacity = 1,
                                                                                                fill = TRUE, fillColor = "orange", fillOpacity = 0.5)),
        markerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>% setView(10,10,zoom=2)} 
  })


featurelist3 <- reactiveValues(
  drawn = list(),
  edited_all = list(),
  deleted_all = list(),
  finished = list()
)


recorder3 <- list()

EVT_DRAW3 <- "mymap7_draw_new_feature"
EVT_EDIT3 <- "mymap7_draw_edited_features"
EVT_DELETE3 <- "mymap7_draw_deleted_features"


#call the objects whit the input[[]] function in a observer environment
#this function store the drawn polygons in the reactives lists
observeEvent(input[[EVT_DRAW3]], {
  featurelist3$drawn <- c(featurelist3$drawn, list(input[[EVT_DRAW3]]))
  featurelist3$finished <- c(featurelist3$finished, list(input[[EVT_DRAW3]]))
  
})
#this function store edited polygons
observeEvent(input[[EVT_EDIT3]], {
  edited <- input[[EVT_EDIT3]]
  # find the edited features and update drawn
  # start by getting the leaflet ids to do the match
  ids <- unlist(lapply(featurelist3$finished, function(x){x$properties$`_leaflet_id`}))
  # now modify drawn to match edited
  lapply(edited$features, function(x) {
    loc <- match(x$properties$`_leaflet_id`, ids)
    if(length(loc) > 0) {
      featurelist3$finished[loc] <<- list(x)
    }
  })
  
  featurelist3$edited_all <- c(featurelist3$edited_all, list(edited))
  
})

#this function estore the objects deleted
observeEvent(input[[EVT_DELETE3]], {
  deleted <- input[[EVT_DELETE3]]
  # find the deleted features and update finished
  # start by getting the leaflet ids to do the match
  ids <- unlist(lapply(featurelist3$finished, function(x){x$properties$`_leaflet_id`}))
  # now modify finished to match edited
  lapply(deleted$features, function(x) {
    loc <- match(x$properties$`_leaflet_id`, ids)
    if(length(loc) > 0) {
      featurelist3$finished[loc] <<- NULL
    }
  })
  
  featurelist3$deleted_all <- c(featurelist3$deleted_all, list(deleted))
  
})



returnlist3 <- reactive({
  
  workinglist <- list(
    drawn = featurelist3$drawn,
    edited = featurelist3$edited_all,
    deleted = featurelist3$deleted_all,
    finished = featurelist3$finished
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






observeEvent(input$done3,{
  
  
  saveData_1(returnlist3()$finished,gsub(" ", "", input$nombre),gsub(" ", "", input$tabset1),(input$done3), gsub(" ", "",input$crop),input$txt3)
  if(length(returnlist3()$finished)!=0){
    updateButton(session, "done3",label = "Stored",style = "success")
    updateTabsetPanel(session, "tabset1",
                      selected = "4. Already conserved")
    
    }
  
  
})

output$plot3<-renderPlot({
  

    if(length(returnlist3()$finished)!=0){
      plot(countries2)
      plot(returnlist3()$finished,col="green",add=TRUE)
    }else{return(NULL)}
    

  
  
})

 #####-------PREGUNTA 4-----------######
 ####--------##########-----------######
 ###_________#########____________######

output$mymap8<-renderLeaflet({

  if(length(returnlist2()$finished)!=0){
    
    leaflet("mymap8") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stamen.TonerLite)
                       options = providerTileOptions(noWrap = TRUE) 
                       
      )%>% addPolygons(data=returnlist2()$finished,color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                        opacity = 0.5, fillOpacity = 0.1,
                        fillColor = colorQuantile("Blues", domain=NULL))    %>% 
      
      addDrawToolbar(
        targetGroup= NULL,
        polylineOptions = FALSE,
        
        polygonOptions = drawPolygonOptions(repeatMode = TRUE,shapeOptions = drawShapeOptions(color = "green", weight = 1, opacity = 1,
                                                                                              fill = TRUE, fillColor = "green", fillOpacity = 0.4)),
        circleOptions = FALSE,
        rectangleOptions = drawRectangleOptions(repeatMode=TRUE,shapeOptions = drawShapeOptions(color = "green", weight = 1, opacity = 1,
                                                                                                fill = TRUE, fillColor = "green", fillOpacity = 0.4)),
        markerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>% setView(10,10,zoom=2)
    
  
  
}else{
  
  
    leaflet("mymap8") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stamen.TonerLite)
                       options = providerTileOptions(noWrap = TRUE) 
                       
      )%>% addDrawToolbar(
        targetGroup= NULL,
        polylineOptions = FALSE,
        
        polygonOptions = drawPolygonOptions(repeatMode = TRUE,shapeOptions = drawShapeOptions(color = "green", weight = 1, opacity = 1,
                                                                                              fill = TRUE, fillColor = "green", fillOpacity = 0.4)),
        circleOptions = FALSE,
        rectangleOptions = drawRectangleOptions(repeatMode=TRUE,shapeOptions = drawShapeOptions(color = "green", weight = 1, opacity = 1,
                                                                                                fill = TRUE, fillColor = "green", fillOpacity = 0.4)),
        markerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>% setView(10,10,zoom=2)
    

  
  
  
}

})







featurelist4 <- reactiveValues(
  drawn = list(),
  edited_all = list(),
  deleted_all = list(),
  finished = list()
)


recorder4 <- list()

EVT_DRAW4 <- "mymap8_draw_new_feature"
EVT_EDIT4 <- "mymap8_draw_edited_features"
EVT_DELETE4 <- "mymap8_draw_deleted_features"


#call the objects whit the input[[]] function in a observer environment
#this function store the drawn polygons in the reactives lists
observeEvent(input[[EVT_DRAW4]], {
  featurelist4$drawn <- c(featurelist4$drawn, list(input[[EVT_DRAW4]]))
  featurelist4$finished <- c(featurelist4$finished, list(input[[EVT_DRAW4]]))
  
})
#this function store edited polygons
observeEvent(input[[EVT_EDIT4]], {
  edited <- input[[EVT_EDIT4]]
  # find the edited features and update drawn
  # start by getting the leaflet ids to do the match
  ids <- unlist(lapply(featurelist3$finished, function(x){x$properties$`_leaflet_id`}))
  # now modify drawn to match edited
  lapply(edited$features, function(x) {
    loc <- match(x$properties$`_leaflet_id`, ids)
    if(length(loc) > 0) {
      featurelist4$finished[loc] <<- list(x)
    }
  })
  
  featurelist4$edited_all <- c(featurelist4$edited_all, list(edited))
  
})

#this function estore the objects deleted
observeEvent(input[[EVT_DELETE4]], {
  deleted <- input[[EVT_DELETE4]]
  # find the deleted features and update finished
  # start by getting the leaflet ids to do the match
  ids <- unlist(lapply(featurelist4$finished, function(x){x$properties$`_leaflet_id`}))
  # now modify finished to match edited
  lapply(deleted$features, function(x) {
    loc <- match(x$properties$`_leaflet_id`, ids)
    if(length(loc) > 0) {
      featurelist3$finished[loc] <<- NULL
    }
  })
  
  featurelist4$deleted_all <- c(featurelist4$deleted_all, list(deleted))
  
})



returnlist4 <- reactive({
  
  workinglist <- list(
    drawn = featurelist4$drawn,
    edited = featurelist4$edited_all,
    deleted = featurelist4$deleted_all,
    finished = featurelist4$finished
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






observeEvent(input$done4,{
  
  
  saveData_1(returnlist4()$finished,gsub(" ", "", input$nombre),gsub(" ", "", input$tabset1),(input$done4), gsub(" ", "",input$crop),input$txt4)
  if(length(returnlist4()$finished)!=0){
    updateButton(session, "done4",label = "Stored",style = "success")
    updateTabsetPanel(session, "tabset1",
                      selected = "Results")
    }
  
  
})

observeEvent(input$close,{
  
  
  if(length(returnlist()$finished)!=0) savePlots(returnlist()$finished,gsub(" ","",input$nombre),countries2,col="blue",n.quest=1,gsub(" ", "",input$crop))
  if(length(returnlist2()$finished)!=0)savePlots(returnlist2()$finished,gsub(" ","",input$nombre),countries2,col="red",n.quest=2,gsub(" ", "",input$crop))
  if(length(returnlist3()$finished)!=0) savePlots(returnlist3()$finished,gsub(" ","",input$nombre),countries2,col="green",n.quest=3,gsub(" ", "",input$crop))
  if(length(returnlist4()$finished)!=0)savePlots(returnlist4()$finished,gsub(" ","",input$nombre),countries2,col="orange",n.quest=4,gsub(" ", "",input$crop))
  
  
  
  
  Sys.sleep(2)
 showModal(modalDialog(
   title = "Important message:",
   h3("Thank you very much for your time!"),h3("Your work has already been saved, now you can close the app."),footer = NULL,easyClose = TRUE
 ))
 Sys.sleep(4)
js$closeWindow()
session$close()

})

output$plot4<-renderPlot({
  
  if(length(returnlist4()$finished)!=0){
  plot(countries2)
  plot(returnlist4()$finished,col="orange",add=TRUE)
  }else(return(NULL))
  
  
})






}






