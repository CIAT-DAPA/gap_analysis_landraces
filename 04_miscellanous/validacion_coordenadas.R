suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(maptools)){install.packages('maptools'); library(maptools)} else {library(maptools)})
suppressMessages(if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} else {library(dplyr)})
suppressMessages(if(!require(tidyr)){install.packages('tidyr'); library(tidyr)} else {library(tidyr)})
suppressMessages(if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)})
suppressMessages(if(!require(jsonlite)){install.packages('jsonlite'); library(jsonlite)} else {library(jsonlite)})
suppressMessages(if(!require(foreach)){install.packages('foreach'); library(foreach)} else {library(foreach)})
#suppressMessages(if(!require(doMC)){install.packages('doMC'); library(doMC)} else {library(doMC)})
suppressMessages(if(!require(XML)){install.packages('XML'); library(XML)} else {library(XML)})
suppressMessages(if(!require(plspm)){install.packages('plspm'); library(plspm)} else {library(plspm)})
suppressMessages(if(!require(reshape)){install.packages('reshape'); library(reshape)} else {library(reshape)})
suppressMessages(if(!require(ncdf4)){install.packages('ncdf4'); library(ncdf4)} else {library(ncdf4)})
suppressMessages(if(!require(compiler)){install.packages('compiler'); library(compiler)} else {library(compiler)})
suppressMessages(if(!require(scales)){install.packages("scales");library(scales)}else{library(scales)})
suppressMessages(if(!require(readxl)){install.packages("readxl"):library(readxl)}else{library(readxl)})
suppressMessages(if(!require(rgeos)){install.packages("rgeos"):library(rgeos)}else{library(rgeos)})
suppressMessages(if(!require(rmapshaper)){install.packages("rmapshaper"):library(rmapshaper)}else{library(rmapshaper)})
suppressMessages(if(!require(dismo)){install.packages("dismo"):library(dismo)}else{library(dismo)})
suppressMessages(if(!require(leaflet)){install.packages("leaflet"):library(leaflet)}else{library(leaflet)})


if(Sys.info()[1]=="Windows"){setwd("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data")}
if(Sys.info()[1]=="Linux"){setwd("/mnt/workspace_cluster_9/gap_analysis_landraces/Input_data")}


#import wold_shape
countries <- rgdal::readOGR(dsn = "./world_shape", layer='all_countries')
#Simplify shapefile make them more lightweight
countries2<-ms_simplify(countries, keep = 0.1)

#Load  the genesys databases presence of bean crops landraces 

geo<-read.csv("presence_data/presence_beans/geo.csv",header = T)
core<-read.csv("presence_data/presence_beans/core.csv",header=T)
#name<-read.csv("genesys-accessions-filtered/names.csv",header=T)

#####--------------------------------------------------------------------###########
#####------ DEPURAR LA BASE DE DATOS DE PRESENCIA DE LOS CULTIVOS --------############
#####-------------------------------------------------------------------############

presence_genesys<-merge(geo,core,by="genesysId")

#mirar si hay valores perdidos (NA) y eliminarlos
sum(is.na(presence_genesys$latitude))
sum(is.na(presence_genesys$longitude))
#en este caso no hay valores perdidos

names(presence_genesys)

nrow(presence_genesys)



#3acaule = gbif("solanum", "acaule*", geo=FALSE)


#quitar las coordenadas (0,0), (0,lat),(long,0)
presence<-subset(presence_genesys, latitude!=0 & longitude!=0 )


#mirar si las coordenadas (0,lat),(long,0) se pueden quitar tambien. (no hubo ni una,i.e se quitaron todas (0,0),(0,lat),(lon,0)) caso contrario acomodar la condicion de arriba como (latitude!=0 | longitude!=0)
dat<-subset(presence_genesys,longitude==0 & latitude!=0)[,c("longitude","latitude", "uncertainty","orgCty")]
coordinates(dat)<- ~ longitude + latitude
proj4string(dat) <- proj4string(countries)
gf<-sp::over(dat,countries)
dat$orgCty

cbind(gf,dat$orgCty,iguales=as.character(gf$ISO3)==as.character(dat$orgCty))


#quitar las coordenadas que caen dentro del oceano

r <- raster::stack("presence_data/world_body_waters_2-5.asc")  #raster con oceanos
crs(r)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


ex <- raster::extract(x=r,y=presence[,c("longitude","latitude")])

cv <- na.omit(ex)
sum(cv)
error <- (1 -sum(cv)/nrow(presence))*100;cat(error,"%")

coor.in.ocean<- cbind(ex, presence)
presence.cleaned<- coor.in.ocean[complete.cases(coor.in.ocean$world_body_waters_2.5),]

#quitar coordenadas repetidas

nrow(presence.cleaned)
dups<-duplicated(presence.cleaned[,c("longitude","latitude")])

 

presence.cleaned<-presence.cleaned[!dups,] #hay mas de 20000 registros repetidos

#guardar base de datos depurada

write.csv(presence.cleaned,file="presence_data/presence_beans/presence_beans_cleaned.csv")




nrow(presence.cleaned[!dups,])

#hacer seguimiento a la depuración de la base de datos
map<-leaflet() %>%addProviderTiles(providers$Stamen.TonerLite, #map type or map theme. -default($Stame.TonerLite)
                                   options = providerTileOptions(noWrap = TRUE)) %>% addCircleMarkers(lng=presence.cleaned$longitude,lat=presence.cleaned$latitude)

map


files <- list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), pattern='grd', full.names=TRUE )

mask<-raster(files[1])
plot(mask)

set.seed(1963)
bg <- randomPoints(mask, 500 )

plot(!is.na(mask), legend=FALSE)
points(bg, cex=0.5)

###comment
