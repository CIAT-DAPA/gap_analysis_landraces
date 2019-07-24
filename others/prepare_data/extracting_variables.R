options(warn = -1); options(scipen = 999); g <- gc(reset = T); rm(list = ls())

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
root     <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9", "Windows" = "//dapadfs/Workspace_cluster_9")


library(raster)

region<- "banana_custom" # Can be "world"

rStack <- list.files(paste0(root, "/gap_analysis_landraces/runs/input_data/generic_rasters/", region), pattern = ".tif$", full.names = TRUE);rStack <- raster::stack(rStack)

banana<-list.files(paste0(root,"/gap_analysis_landraces/runs/input_data/by_crop/banana/raster/",region), pattern = ".tif$",full.names = TRUE);banana <- raster::stack(banana)

elevation<-raster("//dapadfs/data_cluster_4/observed/gridded_products/srtm/SRTM_v41_30s/srtm_v41_30s/w001001.adf") #only, if you have a few occurrences with available data for elevation. You can obtain elevation values for all locations

genotypic_climate <- read.csv("C:/Users/mvdiaz/Desktop/final_database_gbif_genesys_biove_cellID.csv",header=TRUE) # load the dataset with races, longitude and latitude coordinates. Just change the link copying the file and pasting it here.
genotypic_climate <- data.frame(genotypic_climate, raster::extract(x = rStack, y = genotypic_climate %>% dplyr::select(Longitude, Latitude)))
genotypic_climate <- data.frame(genotypic_climate, raster::extract(x = banana, y = genotypic_climate %>% dplyr::select(Longitude, Latitude)))
genotypic_climate <- data.frame(genotypic_climate, raster::extract(x = elevation, y = genotypic_climate %>% dplyr::select(Longitude, Latitude)))
genotypic_climate<-genotypic_climate[,-which(names(genotypic_climate) %in% c("Elevation", "monthCountByTemp10", "Physical.area" ))] #exclude variables that are no needed

colnames(genotypic_climate)[colnames(genotypic_climate) == "raster..extract.x...elevation..y...genotypic_climate.....dplyr..select.Longitude.." ] <- "elevation"

genotypic_climate[is.na(genotypic_climate[,"drymonths_2_5_min"]),"drymonths_2_5_min"]<-0

#### All distance variables, have to be standarize ###
genotypic_climate$dist_toGP1<-genotypic_climate$dist_toGP1/max(na.omit(genotypic_climate$dist_toGP1))
genotypic_climate$dist_h_set<-genotypic_climate$dist_h_set/max(na.omit(genotypic_climate$dist_h_set))
genotypic_climate$dist_rivers<-genotypic_climate$dist_rivers/max(na.omit(genotypic_climate$dist_rivers))

write.csv(genotypic_climate, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/common_bean/lvl_1/classification/lunatus_lvl_1_bd.csv",row.names = FALSE) #Saving the dataframe with all climatic and socioeconomic data 




