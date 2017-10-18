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
suppressMessages(if(!require(dplyr)){install.packages("dplyr");library(dplyr)}else{library(dplyr)})
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
suppressMessages(if(!require(rdrop2)){devtools::install_github('karthik/rdrop2');library(rdrop2)}else{library(rdrop2)})
suppressMessages(if(!require(zip)){install.packages("zip");library(zip)}else{library(zip)})
suppressMessages(if(!require(rsconnect)){install.packages("rsconnect");library(rsconnect)}else{library(rsconnect)})
suppressMessages(if(!require(shinyBS)){install.packages("shinyBS");library(shinyBS)}else{library(shinyBS)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} else {library(dplyr)})
suppressMessages(if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)})
#suppressMessages(if(!require(plotly)){install.packages('plotly'); library(plotly)} else {library(plotly)})


# Define server logic required to draw map whit leaflet option


####importantisisismo
#options("httr_oauth_cache" = TRUE)
#token <- drop_auth()
#saveRDS(token, "droptoken.rds")

#setwd("C:/Users/acmendez/Google Drive/CIAT/gap_analysis_landraces/selected_proposal")

harvest<-readOGR(dsn="harvest_area_shapefile_simplify",layer="harvest_area")
presence<-readRDS("presence_data/genepool_predictions.RDS")
presence<-presence[[1]]
#harvest2<-ms_simplify(harvest, keep = 0.2)
#leaflet("apita")%>% addTiles()%>% addPolygons(data=harvest2)
#writeOGR(harvest2,dsn="C:/Users/acmendez/Google Drive/CIAT/gap_analysis_landraces/selected_proposal/harvest_area_shapefile_simplify",layer="harvest_area",driver ="ESRI Shapefile" )
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
  
  
  
  sf::st_write((data),filePath)
  
  
  files<-list.files(create)
  
  drop_acc(dtoken = token)    
  for(i in 1:length(files)){
    
    drop_upload(file.path(create,files[i]), path = paste("survey","_",nombre,"_",crop,"/",n.quest,sep=""),dtoken=token) 
    
    
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
  
  
  
  
  observeEvent(input$intro,{
    
    newtab<-switch(input$menu,"intro"="Andean")
    updateTabItems(session,"menu",newtab)
    # updateTabsetPanel(session, "tabset1",
    #                   selected = "Crops and expert name")
    
  })
  
  
  
  observeEvent(input$menu,{
    
    
    
    if(input$menu=="Andean"){
      
      source("source/server_scripts/server_andean.R",local=TRUE)
  
      
          }
   
    
 
    
    if(input$menu=="Mesoamerican"){
      
      source("source/server_scripts/server_meso.R",local=TRUE)
    }
    
    })
      
      

 
  
  



}






