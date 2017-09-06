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

bioList <- list.files("/mnt/data_cluster_4/observed/gridded_products/worldclim/Global_30s_v2", full.names = T)
bioList <- bioList[grep(pattern = "prec", x = bioList)]
bioList <- bioList[grep(pattern = "tif$", x = bioList)] %>% mixedsort
P_j <- raster::stack(bioList)

# Annual heat index
T_j_temp <- T_j
T_j_temp[T_j_temp <= 0] <- 0
I <- sum(T_j_temp/5)^1.514

# Cubic function of I
a <- 6.75 * 10^(-7) * I^3 - 7.71 * 10^(-5) * I^2 + 1.792 * 10^(-2) * I + 0.49239

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

tmp_data <- rasterToPoints(T_j_temp[[1]])
tmp_data <- data.frame(tmp_data, cell = cellFromXY(object = T_j_temp[[1]], xy = tmp_data[,1:2]))
latitude <- T_j_temp[[1]]
latitude[] <- NA
latitude[][tmp_data$cell] <- tmp_data$y

A_j = asin(0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(0.00860 * (J_j - 186)))))

# Day duration
D_j <- lapply(1:12, function(i){
  r <- 24 - (24/pi) * acos((sin((0.8333 * pi)/180) + sin((latitude * pi)/180) * sin(A_j[i]))/(cos((latitude * pi)/180) * cos(A_j[i])))
  return(r)
  removeTmpFiles(h = 0)
})
D_j <- stack(D_j)

# Value of adjustment of sunlight depending on the latitude
L_j <- D_j/12

as.data.frame(T_j)



PET_j = if(T_j[] > 0){
  PET_j[] = 1.6 * L_j * (10 * (T_j[])/I)^a
} else {
  if (T_j[] <= 0){
    PET_j[] = 0
  }
}



W = 4.95*exp(0.062*bioList)
DI_j = 100 * ((PET_j - P_j)/PET_j)
