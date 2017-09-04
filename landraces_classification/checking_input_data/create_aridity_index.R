# Create the drought index according to Cortes et al, 2013
# Implemented by: H. Achicanoy
# CIAT, 2017

suppressMessages(library(raster))
suppressMessages(library(tidyverse))
suppressMessages(library(gtools))

bioList <- list.files("/mnt/data_cluster_4/observed/gridded_products/worldclim/Global_30s_v2", full.names = T)
bioList <- bioList[grep(pattern = "tmean", x = bioList)]
bioList <- bioList[grep(pattern = "tif$", x = bioList)] %>% mixedsort
T_j <- raster::stack(bioList)

bioList <- list.files("/mnt/data_cluster_4/observed/gridded_products/worldclim/Global_30s_v2", full.names = T)
bioList <- bioList[grep(pattern = "prec", x = bioList)]
bioList <- bioList[grep(pattern = "tif$", x = bioList)] %>% mixedsort
P_j <- raster::stack(bioList)

# Annual heat index
T_j_temp <- T_j
T_j_temp[T_j_temp <= 0] <- 0
I <- sum(T_j_temp/5)^1.514; rm(T_j_temp)
# Cubic function of I
a <- 6.75 * 10^(-7) * I^3 - 7.71 * 10^(-5) * I^2 + 1.792 * 10^(-2) * I + 0.49239
# Day of the year
J = 15
A_j = asin(0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(0.00860 * (J - 186)))))
# Day duration
D = 24 - (24/pi) * acos((sin((0.8333 * pi)/180) + sin((latitude * pi)/180) * sin(A))/(cos((latitude * pi)/180) * cos(A)))
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
