if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)}
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
if(!require(compiler)){install.packages('compiler'); library(compiler)} else {library(compiler)}
if(!require(scales)){install.packages("scales");library(scales)}else{library(scales)}
if(!require(readxl)){install.packages("readxl"):library(readxl)}else{library(readxl)}

if(Sys.info()[1]=="Windows"){setwd("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data")}
if(Sys.info()[1]=="Linux"){setwd("/mnt/workspace_cluster_9/gap_analysis_landraces/Input_data")}


#import shape files
ogrInfo("./world_shape", "all_countries")
countries <- rgdal::readOGR(dsn = "./world_shape", layer='all_countries')

countries$NAME <- iconv(countries$NAME, from = "UTF-8", to = "latin1")


#import raster
access<-raster('./Raster/spam2005v2r0_harvested-area_bean_total.nc')

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

cat(paste("Country: ", countryList[i], "done " , "suma ",sum(na.omit(values)), " \n" ,sep = ""))
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

f<-list()
for(i in 1:length(countryList)){f[[i]]<-calc_suma(rObject=access,i=i)}

df <- do.call(rbind, f)
rm(f)
df2<-data.frame(ISO3=countries@data$ISO3,subregion=countries@data$SUBREGION)

areaT_cultivada<-merge(df,df2,by="ISO3")

### select the regions of central america, caribbean, south america, eastern africa and south africa
cod_reg<-c(29,13,5,14,18)
cond<-paste("subregion","==",cod_reg ,"|",sep="",collapse = " ")
cond<-substr(cond,1,nchar(cond)-1);cond
cond<-parse(text=cond)
regiones<- areaT_cultivada %>% mutate(.,selec=ifelse(eval(cond),1,0))

reg_subset<-regiones %>% filter(selec==1)
names(reg_subset)
names(coordenadas)[4]<-"Country"


#build the data set with countries selected
survey<-merge(reg_subset,coordenadas,by="Country")

#add the names of regions to data set
survey[which(survey$subregion==14), 5]<-"Eastern Africa"
survey[which(survey$subregion==18), 5]<-"South Africa"
survey[which(survey$subregion==29), 5]<-"Caribbean"
survey[which(survey$subregion==13), 5]<-"Central America"
survey[which(survey$subregion==5), 5]<-"South America"

survey




