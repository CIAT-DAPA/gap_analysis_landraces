#SUM OF HARVESTED AREA IN BEANS CROPS (Landraces)
#By Andres camilo Mendez and Harold Achicanoy


suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(maptools)){install.packages('maptools'); library(maptools)} else {library(maptools)})
suppressMessages(if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} else {library(dplyr)})
suppressMessages(if(!require(tidyr)){install.packages('tidyr'); library(tidyr)} else {library(tidyr)})
suppressMessages(if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)})
suppressMessages(if(!require(jsonlite)){install.packages('jsonlite'); library(jsonlite)} else {library(jsonlite)})
suppressMessages(if(!require(foreach)){install.packages('foreach'); library(foreach)} else {library(foreach)})
suppressMessages(if(!require(doMC)){install.packages('doMC'); library(doMC)} else {library(doMC)})
suppressMessages(if(!require(XML)){install.packages('XML'); library(XML)} else {library(XML)})
suppressMessages(if(!require(plspm)){install.packages('plspm'); library(plspm)} else {library(plspm)})
suppressMessages(if(!require(reshape)){install.packages('reshape'); library(reshape)} else {library(reshape)})
suppressMessages(if(!require(ncdf4)){install.packages('ncdf4'); library(ncdf4)} else {library(ncdf4)})
suppressMessages(if(!require(compiler)){install.packages('compiler'); library(compiler)} else {library(compiler)})
suppressMessages(if(!require(scales)){install.packages("scales");library(scales)}else{library(scales)})
suppressMessages(if(!require(readxl)){install.packages("readxl"):library(readxl)}else{library(readxl)})
suppressMessages(if(!require(rgeos)){install.packages("rgeos"):library(rgeos)}else{library(rgeos)})
suppressMessages(if(!require(rmapshaper)){install.packages("rmapshaper"):library(rmapshaper)}else{library(rmapshaper)})

if(Sys.info()[1]=="Windows"){setwd("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data")}
if(Sys.info()[1]=="Linux"){setwd("/mnt/workspace_cluster_9/gap_analysis_landraces/Input_data")}


#import shape files
ogrInfo("./world_shape", "all_countries")
countries <- rgdal::readOGR(dsn = "./world_shape", layer='all_countries')
#Simplify shapefile make them more lightweight
countries2<-ms_simplify(countries, keep = 0.1)

st_write(st_as_sf(countries2),"//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/world_shape_simplified/all_countries_simplified.shp")

system.time( plot(countries2) )
countries$NAME <- iconv(countries$NAME, from = "UTF-8", to = "latin1")

env <- environment() 
  ls(env)
  unlist(lapply(ls(env), function(x) {
    object.size(get(x, envir = env, inherits = FALSE))
  }))
object.size(get(ls()[1]))

#import raster from Mapspam  harvested area data, filtered by landraces
access <- raster::raster('./Mapspam_raster/spam2005v2r0_harvested-area_bean_total.nc')


#Import a second raster from EarthStat  harvested area
access2 <- raster('./bean_HarvAreaYield2000_NetCDF/bean_AreaYieldProduction.nc',level=5)


#import  world country's cooridanates from .xlm file
coordenadas<- read_excel("./coordinates_countries_world/coordenadas_paises_mundo.xlsx", sheet = "Sheet1")

#set in a vector  the names of all countries
countryList <- countries@data$NAME

#discard antarctica's country 
countryList <- countryList[which(countryList!="Antarctica")]

#calculate sum of harvested area
calc_suma<-function(rObject ,i){

country <- countries[countries@data$NAME== countryList[i],]
country_data <- raster::crop(rObject, extent(country)) # cut raster by conutries shape's

country_data <- raster::mask(x = country_data, mask = country) #create a new raster from the cropped shape  

values <- country_data[!is.na(country_data[])]

country_data <- data.frame(ISO3 = countries@data$ISO3[which(countries@data$NAME == countryList[i])], Country = countryList[i], Suma = sum(na.omit(values))) 

cat(paste("Country: ", countryList[i], ": done " , "suma ",sum(na.omit(values)), " \n" ,sep = ""))
rm(values, country)

return(country_data)
}

calc_suma <- cmpfun(calc_suma) #convert in a compilable function


is.compile <- function(func){
  # this function lets us know if a function has been byte-coded or not
  #If you have a better idea for how to do this - please let me know...
  if(class(func) != "function") stop("You need to enter a function")
  last_2_lines <- tail(capture.output(func),2)
  any(grepl("bytecode:", last_2_lines)) # returns TRUE if it finds the text "bytecode:" in any of the last two lines of the function's print
}
is.compile(calc_suma)

#calculate the sum of harvested area in beans crops for MAPSPAM data.nc

f<-list()
for(i in 1:length(countryList)){f[[i]]<-calc_suma(rObject=access,i=i)}

df <- do.call(rbind, f)
rm(f)
df2<-data.frame(ISO3=countries@data$ISO3,subregion=countries@data$SUBREGION)

areaT_cultivada<-merge(df,df2,by="ISO3")
rm(df);rm(df2)

#calculate the sum of harvested area in beans crops for EARTHSTAT data.nc

f<-list()
for(i in 1:length(countryList)){f[[i]]<-calc_suma(rObject=access2,i=i)}
f<-do.call(rbind,f)
f<-f[,-which(names(f)=="Country")]

#combine the sums of harvested area for both information sources (mapspam and eartstat)

areaT_cultivada<-merge(areaT_cultivada,f,by="ISO3")
rm(f)
names(areaT_cultivada)[c(3,5)]<-c("sum_mapspam","sum_earthstat")

#calculate the difference between both sums of harvested area, and plot histogram of that difference

delta<- abs(areaT_cultivada$sum_mapspam- areaT_cultivada$sum_earthstat)
delta<-delta[which(delta!=0)]
hist(delta,main="Difference between area's sums",xlab="Diferrence")
summary(delta)

View(areaT_cultivada)

##########----------TO BUILD EXPERT'S SURVEY IN  R SHINY----------------------##################

### select the regions of central america, caribbean, south america, eastern africa and south africa
cod_reg<-c(29,13,5,14,18)
cond<-paste("subregion","==",cod_reg ,"|",sep="",collapse = " ")
cond<-substr(cond,1,nchar(cond)-1);cond
cond<-parse(text=cond)
regiones<- areaT_cultivada %>% mutate(.,selec=ifelse(eval(cond),1,0))

reg_subset<-regiones %>% filter(selec==1)
names(reg_subset)


#build the data set with countries selected
coordenadas<-countries@data[,c(5,10,11)]
names(coordenadas)[1]<- "Country"

survey<-merge(reg_subset,coordenadas,by="Country")

#add the names of regions to data set
survey[which(survey$subregion==14), 5]<-"Eastern Africa"
survey[which(survey$subregion==18), 5]<-"South Africa"
survey[which(survey$subregion==29), 5]<-"Caribbean"
survey[which(survey$subregion==13), 5]<-"Central America"
survey[which(survey$subregion==5), 5]<-"South America"

save(survey,file = "survey.RData")

##### DEPLOY SHINY APP ####3

rsconnect::setAccountInfo(name='andres159ciat',
                          token='B3CE4384B0DD472723F849715F715622',
                          secret='m9uT8tEICjyRtuQFverAQFauis9onxcKGlq3gQMk')

rsconnect::deployApp('C:/Users/acmendez/Documents/GitHub/gap_analysis_landraces/selected_proposal')



