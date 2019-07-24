# Create the drought index according to Cortes et al, 2013
# Implemented by: H. Achicanoy
# CIAT, 2017

suppressMessages(library(raster))
suppressMessages(library(tidyverse))
suppressMessages(library(gtools))

removeTmpFiles(h = 0)

bioList <- list.files("/mnt/data_cluster_4/observed/gridded_products/worldclim/Global_2_5min_v2", full.names = T)
bioList <- bioList[grep(pattern = "tavg", x = bioList)]
bioList <- bioList[grep(pattern = "tif$", x = bioList)] %>% mixedsort
T_j <- raster::stack(bioList)

bioList <- list.files("/mnt/data_cluster_4/observed/gridded_products/worldclim/Global_2_5min_v2", full.names = T)
bioList <- bioList[grep(pattern = "prec", x = bioList)]
bioList <- bioList[grep(pattern = "tif$", x = bioList)] %>% mixedsort
P_j <- raster::stack(bioList)

# Annual heat index
T_j_temp <- T_j
T_j_temp[T_j_temp <= 0] <- 0
writeRaster(x = T_j_temp, "/home/hachicanoy/T_j_temp.nc", format="CDF", overwrite=TRUE)

I <- sum(T_j_temp/5)^1.514
writeRaster(x = I, "/home/hachicanoy/I.tif", format = "GTiff", overwrite = TRUE)

removeTmpFiles(h = 0)

# Cubic function of I
I <- raster("/home/hachicanoy/I.tif")
a <- 6.75 * 10^(-7) * I^3 - 7.71 * 10^(-5) * I^2 + 1.792 * 10^(-2) * I + 0.49239
writeRaster(x = a, "/home/hachicanoy/a.tif", format = "GTiff", overwrite = TRUE)

removeTmpFiles(h = 0)

# Day of the year
library(Hmisc)
days <- rep(NA, 12)
monthList <- c(paste0("0", 1:9), 10:12)
for(i in 1:length(monthList)){
  days[i] <- monthDays(as.Date(paste0('2010-', monthList[i], '-01')))
}
ydayList <- lubridate::yday(seq(from = as.Date('2010-01-01'), to = as.Date('2010-12-31'), by = 1))
J_j <- ydayList[c(15,
           days[1] + 14,
           sum(c(days[1:2])) + 14,
           sum(c(days[1:3])) + 14,
           sum(c(days[1:4])) + 14,
           sum(c(days[1:5])) + 14,
           sum(c(days[1:6])) + 14,
           sum(c(days[1:7])) + 14,
           sum(c(days[1:8])) + 14,
           sum(c(days[1:9])) + 14,
           sum(c(days[1:10])) + 14,
           sum(c(days[1:11])) + 14)]; rm(days, ydayList, monthList)

# Create a latitude raster
tmp_data <- rasterToPoints(T_j_temp[[1]])
tmp_data <- data.frame(tmp_data, cell = cellFromXY(object = T_j_temp[[1]], xy = tmp_data[,1:2]))
latitude <- T_j_temp[[1]]
latitude[] <- NA
latitude[][tmp_data$cell] <- tmp_data$y
writeRaster(x = latitude, "/home/hachicanoy/latitude.tif", format = "GTiff", overwrite = TRUE)

A_j = asin(0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(0.00860 * (J_j - 186)))))

# Day duration
D_j <- lapply(1:12, function(i){
  
  Aj <- A_j[i]
  d_j <- raster::calc(latitude, function(x){
    r <- 24 - (24/pi) * acos((sin((0.8333 * pi)/180) + sin((x * pi)/180) * sin(Aj))/(cos((x * pi)/180) * cos(Aj)))
    return(r)
  })
  return(d_j)
  
})
D_j <- raster::stack(D_j)
writeRaster(x = D_j, "/home/hachicanoy/D_j.nc", format="CDF", overwrite=TRUE)

# Value of adjustment of sunlight depending on the latitude
L_j <- D_j/12

# PET Thornthwaite
a <- raster("/home/hachicanoy/a.tif")
I <- raster("/home/hachicanoy/I.tif")
T_j_temp <- raster::stack("/home/hachicanoy/T_j_temp.nc")
D_j <- raster::stack("/home/hachicanoy/D_j.nc")
L_j <- D_j/12
L_j <- raster:stack(L_j)

library(foreach)
library(doMC)

registerDoMC(8)

PET <- foreach(i = 1:12) %dopar% {
  
  pet_j <- raster::calc(T_j_temp[[i]], function(x){
    r <- 1.6 * L_j * ((10 * x)/I)^a
    return(r)
  })
  
}

DI_j <- lapply(1:12, function(i){
  pet <- PET[[i]]
  pj <- P_j[[i]]
  di_j <- raster::calc(pet, function(x){
    r <- 100 * ((pet - pj)/pet)
    return(r)
  })
  return(di_j)
})
