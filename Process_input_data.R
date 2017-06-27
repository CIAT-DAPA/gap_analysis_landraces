# Processing data sources for sustainable food systems project
# Implemented by: H. Achicanoy, P. Alvarez & L. Lamotte
# CIAT, 2017
#andres mendez

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)
OSys <- Sys.info(); OSys <- OSys[names(OSys)=="sysname"]
if(OSys == "Linux"){
  wk_dir <- "/mnt/workspace_cluster_9/Sustainable_Food_System/Input_data/"; setwd(wk_dir); rm(wk_dir)
} else {
  if(OSys == "Windows"){
    wk_dir <- "//dapadfs/workspace_cluster_9/Sustainable_Food_System/Input_data"; setwd(wk_dir); rm(wk_dir)
  }
}; rm(OSys)

# Load packages
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
suppressMessages(library(compiler))

# Worldwide shapefile
countries <- rgdal::readOGR(dsn = "./world_shape", "all_countries")
countries$COUNTRY <- iconv(countries$COUNTRY, from = "UTF-8", to = "latin1")

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
### DIMENSION: HUMAN INTERVENTIONS                                                                            ###
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

# 1. Accessibility per country
if(!file.exists('./world_cityAccess/countries_access.txt')){
  
  # Accessibility to cities
  tmpRaster <- paste(getwd(), "/tmpRaster", sep = "")
  if(!dir.exists(tmpRaster)){dir.create(tmpRaster)}
  rasterOptions(tmpdir = tmpRaster)
  access <- raster('./world_cityAccess/acc_50k')
  
  # List of countries (excluding Antarctica)
  countryList <- countries@data$COUNTRY
  countryList <- countryList[which(countryList!="Antarctica")]
  
  # Function to calculate median per country
  calc_median <- function(rObject, i){
    
    country <- countries[countries@data$COUNTRY==countryList[i],]
    country_data <- raster::crop(rObject, extent(country))
    country_data <- raster::mask(x = country_data, mask = country)
    values <- country_data[!is.na(country_data[])]
    country_data <- data.frame(ISO3 = countries@data$ISO3[which(countries@data$COUNTRY == countryList[i])], Country = countryList[i], Median = median(values, na.rm = T)); rm(values, country)
    cat(paste("Country: ", countryList[i], " done\n", sep = ""))
    return(country_data)
    
  }; calc_median <- cmpfun(calc_median)
  
  # Parellize the process using 8 cores
  registerDoMC(8)
  countries_access <- foreach(i = 1:length(countryList)) %dopar% {
    tryCatch(expr={
      calc_median(rObject = access, i = i)
    },
    error=function(e){
      cat("Cutting process failed:", countryList[i], "\n")
      return("Done\n")
    })
  }; removeTmpFiles(h = 0)
  
  countries_access <- do.call(rbind, countries_access)
  saveRDS(object = countries_access, file = './world_cityAccess/countries_access.RDS')
  removeTmpFiles(h = 0)
} else {
  countries_access <- read.table(file = './world_cityAccess/countries_access.txt', sep = ",", header = T)
  names(countries_access)[ncol(countries_access)] <- "Access_median"
}

# 2. Global Human Footprint (1995-2004) per country
# From: http://sedac.ciesin.columbia.edu/data/set/wildareas-v2-human-footprint-geographic/data-download
if(!file.exists('./world_humanFootprint/countries_foodprint.txt')){
  
  # Human footprint raster
  tmpRaster <- paste(getwd(), "/tmpRaster", sep = "")
  if(!dir.exists(tmpRaster)){dir.create(tmpRaster)}
  rasterOptions(tmpdir = tmpRaster)
  hfootprint <- raster('./world_humanFootprint/hf_v2geo')
  
  # List of countries (excluding Antarctica)
  countryList <- countries@data$COUNTRY
  countryList <- countryList[which(countryList!="Antarctica")]
  
  # Function to calculate median per country
  calc_median <- function(rObject, i){
    
    country <- countries[countries@data$COUNTRY==countryList[i],]
    country_data <- raster::crop(rObject, extent(country))
    country_data <- raster::mask(x = country_data, mask = country)
    values <- country_data[!is.na(country_data[])]
    data.frame(ISO3 = countries@data$ISO3[which(countries@data$COUNTRY == countryList[i])], Country = countryList[i], Median = median(values, na.rm = T)); rm(values, country)
    cat(paste("Country: ", countryList[i], " done\n", sep = ""))
    return(country_data)
    
  }; calc_median <- cmpfun(calc_median)
  
  # Parellize the process using 8 cores
  registerDoMC(8)
  countries_footprint <- foreach(i = 1:length(countryList)) %dopar% {
    tryCatch(expr={
      calc_median(rObject = hfootprint, i = i)
    },
    error=function(e){
      cat("Cutting process failed:", countryList[i], "\n")
      return("Done\n")
    })
  }; removeTmpFiles(h = 0)
  countries_footprint <- do.call(rbind, countries_footprint)
  saveRDS(object = countries_footprint, file = './world_humanFootprint/countries_foodprint.RDS')
  removeTmpFiles(h = 0)
} else {
  countries_footprint <- read.table(file = './world_humanFootprint/countries_foodprint.txt', sep = ",", header = T)
  names(countries_footprint)[ncol(countries_footprint)] <- "Footprint_median"
}

h_interventions <- dplyr::inner_join(x = countries_access, y = countries_footprint, by = "ISO3")
h_interventions <- h_interventions %>% dplyr::select(ISO3, Access_median, Footprint_median); rm(countries_access, countries_footprint)

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
### DIMENSION: FOOD SECURITY                                                                                  ###
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

# 1. Global Hunger Index Data 2016
# From: https://github.com/IFPRI/Global-Hunger-Index/tree/master/data
ghi <- jsonlite::fromJSON("./world_globalHungerIndex2016/country-details.json"); rows <- ghi$data$name_de; ISO3 <- ghi$data$id
ghi <- ghi$data$score; ghi$ISO3 <- ISO3; rownames(ghi) <- rows; rm(rows, ISO3)
for(i in 1:(ncol(ghi)-1)){
  ghi[,i] <- gsub(pattern = "-", replacement = NA, x = ghi[,i]) # Omit empty data
  ghi[,i] <- gsub(pattern = "<5", replacement = NA, x = ghi[,i]) # Omit values under detection limit
  ghi[,i] <- as.numeric(ghi[,i])
}; rm(i)
ghi <- ghi[which(!is.na(ghi$ISO3)),]
ghi <- ghi[,c("ISO3", "year2000")]; names(ghi)[2] <- "GHI_2000"

# 2. Global Subnational Prevalence of Child Malnutrition (1990 - 2002)
# From: http://sedac.ciesin.columbia.edu/data/set/povmap-global-subnational-prevalence-child-malnutrition/data-download

chmalnutrition <- read.csv("./world_childMalnutrition/hunger_data.csv") # These data come from cities calculations

# Obtain countries according to ISO3 codes
aux <- countries@data %>% dplyr::select(ISO3, COUNTRY)
chmalnutrition <- chmalnutrition %>% dplyr::left_join(x = chmalnutrition, y = aux, by = c("ISO3V10" = "ISO3")); rm(aux)

# Define properly NAs
chmalnutrition$UW[which(chmalnutrition$UW == -999)] <- NA
chmalnutrition$PCTU5[which(chmalnutrition$PCTU5 == -999)] <- NA

# Calculate mean data per country
chmalnutrition <- chmalnutrition %>% dplyr::select(ISO3V10, COUNTRY, UW, PCTU5) %>% dplyr::group_by(ISO3V10, COUNTRY) %>% dplyr::summarise_all(mean, na.rm = T)
names(chmalnutrition)[which(names(chmalnutrition) == "ISO3V10")] <- "ISO3"
chmalnutrition <- chmalnutrition[which(!is.na(chmalnutrition$ISO3)),]; rownames(chmalnutrition) <- 1:nrow(chmalnutrition)
plot(chmalnutrition$PCTU5, chmalnutrition$UW, xlab = "% of population under age 5", ylab = "Percentage of children underweight", pch = 20)
chmalnutrition <- chmalnutrition[,c("ISO3", "UW")]; names(chmalnutrition)[2] <- "ChldMalnutrition"
chmalnutrition <- as.data.frame(chmalnutrition)

# 3. FAO's suite of Food Security Indicators
fsecurity <- read.csv("./world_foodSecurity/FAOSTAT_data_2-20-2017.csv")
timeList <- unique(as.character(fsecurity$Year)); fsecurity$Country <- as.character(fsecurity$Country)
fsecurity$Country[which(fsecurity$Country == "Côte d'Ivoire")] <- "Ivory Coast"; fsecurity$Country <- factor(fsecurity$Country)

# Loading ISO3 codes for FAO information
# From: http://www.fao.org/countryprofiles/iso3list/en/
if(!file.exists('./world_foodSecurity/FAO_ISO3_codes.RDS')){
  iso3FAO <- readHTMLTable('http://www.fao.org/countryprofiles/iso3list/en/')
  iso3FAO <- iso3FAO$`NULL`
  saveRDS(object = iso3FAO, file = './world_foodSecurity/FAO_ISO3_codes.RDS')
} else {
  iso3FAO <- readRDS(file = './world_foodSecurity/FAO_ISO3_codes.RDS')
  iso3FAO$`Short name` <- as.character(iso3FAO$`Short name`); iso3FAO$`Short name`[which(iso3FAO$`Short name` == "C�te d'Ivoire")] <- "Ivory Coast"; iso3FAO$`Short name` <- as.factor(iso3FAO$`Short name`)
}

fsecurity <- dplyr::left_join(x = fsecurity, y = iso3FAO, by = c("Country" = "Short name")); rm(iso3FAO)

fsecurityList <- lapply(1:length(timeList), function(i){
  df <- fsecurity %>% dplyr::select(ISO3, Country, Item, Year, Value) %>% dplyr::filter(Year == timeList[i]) %>% tidyr::spread(key = Item, value = Value)
  df <- df[which(!is.na(df$ISO3)),]; rownames(df) <- 1:nrow(df)
  return(df)
}); names(fsecurityList) <- timeList; rm(fsecurity, timeList)

# Year or periods to exclude
ypList <- unlist(lapply(1:length(fsecurityList), function(i){
  
  x <- fsecurityList[[i]]
  dim.mat <- nrow(x[,4:ncol(x)]) * ncol(x[,4:ncol(x)])
  if(sum(is.na(x[,4:ncol(x)])) == dim.mat){
    return(names(fsecurityList)[i])
  } else {
      return(cat(""))
  }
  
}))
fsecurityList <- fsecurityList[setdiff(x = names(fsecurityList), y = ypList)]; rm(ypList)

# for(i in 1:length(fsecurityList)){
#   
#   Sys.sleep(1)
#   pairs(fsecurityList[[i]][,4:ncol(fsecurityList[[i]])])
#   
# }

fsec <- fsecurityList[[50]]; fsec <- fsec[,c(1, 4, 5, 8, 16)]
names(fsec)[2:ncol(fsec)] <- c("sanitation", "water_sources", "GDP", "political_stability")

complete_data <- dplyr::full_join(x = ghi, y = chmalnutrition, by = c("ISO3" = "ISO3")); rm(ghi, chmalnutrition)
complete_data <- dplyr::full_join(x = complete_data, y = h_interventions, by = c("ISO3" = "ISO3")); rm(h_interventions)
complete_data <- dplyr::full_join(x = complete_data, y = fsec, by = c("ISO3" = "ISO3")); rm(fsec)

complete_data <- complete_data[-which(apply(X = complete_data, MARGIN = 1, FUN = function(x){sum(is.na(x))}) >= 7),]
rownames(complete_data) <- 1:nrow(complete_data)

saveRDS(object = complete_data, file = "data_joined.RDS")
