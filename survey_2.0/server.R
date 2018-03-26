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
suppressMessages(if(!require(yaml)){install.packages('yaml'); library(yaml)} else {library(yaml)})
suppressMessages(if(!require(shiny)){install.packages("shiny");library(shiny)}else{library(shiny)})
suppressMessages(if(!require(shinydashboard)){install.packages("shinydashboard");library(shinydashboard)}else{library(shinydashboard)})
suppressMessages(if(!require(leaflet)){install.packages("leaflet");library(leaflet)}else{library(leaflet)})
suppressMessages(if(!require(curl)){install.packages("curl");library(curl)}else{library(curl)})
suppressMessages(if(!require(devtools)){install.packages("devtools");library(devtools)}else{library(devtools)})
#suppressMessages(if(!require(readr)){install.packages("readr");library(readr)}else{library(readr)})
#suppressMessages(if(!require(rmapshaper)){install.packages("rmapshaper");library(rmapshaper)}else{library(rmapshaper)})
suppressMessages( if (!require(geojsonio)) { install.packages("geojsonio");library(geojsonio)}else{library(geojsonio)})
#suppressMessages( if (!require(rleafmap)) { install.packages("rleafmap");library(rleafmap)}else{library(rleafmap)})
#suppressMessages( if (!require(leaflet.extras)) { devtools::install_github('bhaskarvk/leaflet.extras',force=TRUE);library(leaflet.extras)}else{library(leaflet.extras)})
suppressMessages(if(!require(sf)){install.packages("sf");library(sf)}else{library(sf)})
#suppressMessages(if(!require(digest)){install.packages("digest");library(digest)}else{library(digest)})
suppressMessages(if(!require(rdrop2)){install.packages("rdrop2");library(rdrop2)}else{library(rdrop2)})
#suppressMessages(if(!require(zip)){install.packages("zip");library(zip)}else{library(zip)})
suppressMessages(if(!require(rsconnect)){install.packages("rsconnect");library(rsconnect)}else{library(rsconnect)})
suppressMessages(if(!require(shinyBS)){install.packages("shinyBS");library(shinyBS)}else{library(shinyBS)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} else {library(dplyr)})
#suppressMessages(if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)})
#suppressMessages(if(!require(plotly)){install.packages('plotly'); library(plotly)} else {library(plotly)})
suppressMessages(if(!require(jsonlite)){install.packages('jsonlite'); library(jsonlite)} else {library(jsonlite)})
suppressMessages(if(!require(sp)){install.packages('sp'); library(sp)} else {library(sp)})

# Define server logic required to draw map whit leaflet option


####importantisisismo

#token <- drop_auth()
#saveRDS(token, "droptoken.rds")

#setwd("C:/Users/acmendez/Google Drive/CIAT/gap_analysis_landraces/selected_proposal")

#harvest<-readOGR(dsn="harvest_area_shapefile_simplify",layer="harvest_area")
#presence<-read.csv("presence_data/presence_beans_cleaned.csv")
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
  
  
  
  writeOGR( data , dsn = filePath , layer = nombre , driver = "ESRI Shapefile" )
  
  
  files<-list.files(create)
  
  drop_acc(dtoken = token)    
  for(i in 1:length(files)){
    
    drop_upload(file =  file.path(create,files[i]), path = paste("survey","_",nombre,"_",crop,"/",n.quest,sep=""),dtoken=token) 
    
    
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


###BEGIN SERVER FUNCTION

function(input, output,session) {
  
  
  observeEvent(input$introd,{
    
      #print(input$menu)
    newtab <- switch(input$menu,"introm" = "Andean")
    
    updateTabItems(session,"menu",newtab)
    # updateTabsetPanel(session, "tabset1",
    #                   selected = "Crops and expert name")
    
  })
  
  
  observeEvent(input$keep,{
    #input$crop!="-- Please select crop --" && 
   
    
     if(nchar(input$nombre)!=0 ){
     
  
    updateTabsetPanel(session, "tabset1",
                      selected = "Landrace")
    }
     else{showModal(modalDialog(
       title = "Oops something went wrong:",
       h3("May be you forgot to select a crop variety or  write a name"),footer = modalButton("OK"),easyClose = TRUE
     ))  }
     
    
    
    
  })
  
  
 
  
  
  observeEvent(input$mydata,{ 
   
    
  raw_pol <-   fromJSON(input$mydata)
  
  print(raw_pol)
 
  #polys <- vector(mode='list', length=length(raw_pol))
  
  polys <- lapply(raw_pol, function(x){
    
    pol <- Polygons( list(Polygon( x[,c("lng", "lat")]) ),  ID = as.character( parent.frame()$i[] )  )
    return(pol)
  })
  
  SP <- SpatialPolygons(polys)
  SP <- SpatialPolygonsDataFrame(SP, data = data.frame(ID= 1:length(SP)) )
  
  raster::crs(SP) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
  
  saveData_1(SP,gsub(" ", "", input$nombre),gsub(" ", "",input$tabset1),(input$done), gsub(" ", "",input$crop),input$txt1)
    
    
    })
  
  

  
  
  observeEvent(input$mydata2,{ 
    
    
    raw_pol <-   fromJSON(input$mydata2)
    
   # print(raw_pol)
    
    
    
    polys <- lapply(raw_pol, function(x){
      
      pol <- Polygons( list(Polygon( x[,c("lng", "lat")]) ),  ID = as.character( parent.frame()$i[] )  )
      return(pol)
    })
    
    SP <- SpatialPolygons(polys)
    SP <- SpatialPolygonsDataFrame(SP, data = data.frame(ID= 1:length(SP)) )
    
    raster::crs(SP) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
    
    saveData_1(SP,gsub(" ", "", input$nombre),gsub(" ", "",input$tabset1),(input$done), gsub(" ", "",input$crop),input$txt2)
    
    
  })
  
  


observeEvent(input$nextA,{
  
    updateTabsetPanel(session, "tabset1",
                      selected = "CGIAR gaps")
  
})

observeEvent(input$closeA,{

  Sys.sleep(2)
    showModal(modalDialog(
      title = "Important message:",
      h3("Thank you very much for your time!"),h3("Your work has already been saved, now you can close the app."),footer = NULL,easyClose = TRUE
    ))
    Sys.sleep(3)

    session$close()
  
})


# Then pass the token to each drop_ function








}






