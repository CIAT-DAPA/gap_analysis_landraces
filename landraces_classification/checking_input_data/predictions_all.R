# Predicting occurrence data from all sources (CIAT, GBIF, USDA)
# H. Achicanoy
# CIAT, 2017

# R options
options(warn = -1); options(scipen = 999); g <- gc(reset = T); rm(list = ls())

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
root     <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9", "Windows" = "//dapadfs/Workspace_cluster_9")

# Load packages
suppressMessages(if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}else{library(tidyverse)})
suppressMessages(if(!require(rgdal)){install.packages("rgdal");library(rgdal)}else{library(rgdal)})
suppressMessages(if(!require(sp)){install.packages("sp");library(sp)}else{library(sp)})
suppressMessages(if(!require(raster)){install.packages("raster");library(raster)}else{library(raster)})
suppressMessages(if(!require(ncdf4)){install.packages("ncdf4");library(ncdf4)}else{library(ncdf4)})
suppressMessages(if(!require(rasterVis)){install.packages("rasterVis");library(rasterVis)}else{library(rasterVis)})
suppressMessages(if(!require(htmlwidgets)){install.packages("htmlwidgets");library(htmlwidgets)}else{library(htmlwidgets)})
suppressMessages(if(!require(compiler)){install.packages("compiler");library(compiler)}else{library(compiler)})
suppressMessages(if(!require(leaflet)){install.packages("leaflet");library(leaflet)}else{library(leaflet)})
suppressMessages(if(!require(highcharter)){install.packages("highcharter");library(highcharter)}else{library(highcharter)})
suppressMessages(if(!require(plotly)){install.packages("plotly");library(plotly)}else{library(plotly)})
suppressMessages(if(!require(d3heatmap)){install.packages("d3heatmap");library(d3heatmap)}else{library(d3heatmap)})
suppressMessages(if(!require(cluster)){install.packages("cluster");library(cluster)}else{library(cluster)})
suppressMessages(if(!require(FactoMineR)){install.packages("FactoMineR");library(FactoMineR)}else{library(FactoMineR)})
suppressMessages(if(!require(factoextra)){install.packages("factoextra");library(factoextra)}else{library(factoextra)})
suppressMessages(if(!require(gtools)){install.packages("gtools");library(gtools)}else{library(gtools)})
suppressMessages(if(!require(googlesheets)){install.packages("googlesheets");library(googlesheets)}else{library(googlesheets)})
suppressMessages(if(!require(corrplot)){install.packages("corrplot");library(corrplot)}else{library(corrplot)})

# Verify if extracted data exists
if(!file.exists(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_GBIF_data_2017_11_27/AMERICAS/DATA_CLEANED_GBIF_YEAR2000_CLIMATE-SOCIOECONOMIC.RDS"))){
  # Load data
  occ_data <- read.csv(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_GBIF_data_2017_11_27/AMERICAS/DATA_CLEANED_GBIF_YEAR2000.csv"))
  
  # Extract data from spatial sources
  rDir <- paste0(root, "/gap_analysis_landraces/Input_data/raster_sdm/2_5m")
  rStack <- raster::stack(list.files(path = rDir, full.names = T, pattern = "*.tif$"))
  rData  <- raster::extract(x = rStack, y = occ_data[,c("LONGITUDE", "LATITUDE")])
  occ_data <- cbind(occ_data, rData); rm(rDir, rStack, rData)
  saveRDS(object = occ_data, file = paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_GBIF_data_2017_11_27/AMERICAS/DATA_CLEANED_GBIF_YEAR2000_CLIMATE-SOCIOECONOMIC.RDS"))
} else {
  genepool_na <- readRDS(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_GBIF_data_2017_11_27/AMERICAS/DATA_CLEANED_GBIF_YEAR2000_CLIMATE-SOCIOECONOMIC.RDS"))
  genepool_na <- genepool_na %>% dplyr::select(ID, LONGITUDE, LATITUDE, SOURCE, Accessibility:embergerQ, growingDegDays0:Yield) 
  # We start with 33773 records
}

# Counts
table(genepool_na$SOURCE) # Counts by source
sum(duplicated(genepool_na$ID)) # Number of duplicated records
table(genepool_na$SOURCE[which(duplicated(genepool_na$ID))]) # Number of duplicated records by source

genepool_na <- genepool_na[!duplicated(genepool_na$ID),] # Deleting duplicated records: 33209
genepool_na <- genepool_na[complete.cases(genepool_na),] # Deleting records with missing data: 32656
rownames(genepool_na) <- genepool_na$ID
genepool_na <- genepool_na %>% dplyr::select(5:ncol(genepool_na))

model_type <- c("FDA", "glmFit1", "Rforest", "svmFit")


pred.rf <- predict(Rforest, newdata = genepool_na)
pred.svm <- predict(svmFit, newdata = genepool_na)
pred.fda <- predict(FDA$finalModel, newdata = genepool_na, type = "class")
pred.glm <- predict(glmFit1, newdata = genepool_na)

genepool_na$RF.predicted <- pred.rf
genepool_na$SVM.predicted <- pred.svm
genepool_na$FDA.predicted <- pred.fda
genepool_na$GLM.predicted <- pred.fda

getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
genepool_na$Genepool.prediction <- apply(X = genepool_na %>% select(RF.predicted:GLM.predicted), MARGIN = 1, FUN = getmode)
genepool_na$ID <- rownames(genepool_na)


all_data <- read.csv("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_occurrence_data/_GBIF_data_2017_11_27/AMERICAS/DATA_CLEANED_GBIF_YEAR2000.csv")
genepool_na2 <- dplyr::left_join(x = genepool_na, y = all_data, by = "ID")
saveRDS(object = genepool_na2, file = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Results/_ocurrences_predicted/GENEBANKS_OCC_PREDICTED.RDS")

genepool_na <- readRDS("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Results/_ocurrences_predicted/GENEBANKS_OCC_PREDICTED.RDS")

# All sources
# dat_wlrd <- map_data("world")
# gg <-ggplot() + 
#   geom_polygon(data = dat_wlrd, aes(x = long, y = lat, group = group), fill = "gray70", na.rm = T, show.legend = F) +
#   coord_fixed(1.3) +
#   geom_point(aes(x = LONGITUDE, y = LATITUDE, col = Genepool.prediction), size = 1.2, stroke = 1, data = genepool_na, show.legend = TRUE, shape = 17) +
#   coord_equal() + xlim(-150, -30) + ylim(-60, 80)
# gg
# 
# # GBIF
# gg <-ggplot() + 
#   geom_polygon(data = dat_wlrd, aes(x = long, y = lat, group = group), fill = "gray70", na.rm = T, show.legend = F) +
#   coord_fixed(1.3) +
#   geom_point(aes(x = LONGITUDE, y = LATITUDE, col = Genepool.prediction), size = 1.2, stroke = 1, data = genepool_na[genepool_na$SOURCE == "GBIF",], show.legend = TRUE, shape = 17) +
#   coord_equal() + xlim(-150, -30) + ylim(-60, 80)
# gg
# 
# # CIAT
# gg <-ggplot() + 
#   geom_polygon(data = dat_wlrd, aes(x = long, y = lat, group = group), fill = "gray70", na.rm = T, show.legend = F) +
#   coord_fixed(1.3) +
#   geom_point(aes(x = LONGITUDE, y = LATITUDE, col = Genepool.prediction), size = 1.2, stroke = 1, data = genepool_na[genepool_na$SOURCE == "CIAT",], show.legend = TRUE, shape = 17) +
#   coord_equal() + xlim(-150, -30) + ylim(-60, 80)
# gg
# 
# # USDA
# gg <-ggplot() + 
#   geom_polygon(data = dat_wlrd, aes(x = long, y = lat, group = group), fill = "gray70", na.rm = T, show.legend = F) +
#   coord_fixed(1.3) +
#   geom_point(aes(x = LONGITUDE, y = LATITUDE, col = Genepool.prediction), size = 1.2, stroke = 1, data = genepool_na[genepool_na$SOURCE == "USDA",], show.legend = TRUE, shape = 17) +
#   coord_equal() + xlim(-150, -30) + ylim(-60, 80)
# gg

colnames(genepool_na2)[which(colnames(genepool_na2) == "LONGITUDE")] <- "Longitude"
colnames(genepool_na2)[which(colnames(genepool_na2) == "LATITUDE")] <- "Latitude"

saveRDS(genepool_na2, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Results/classification_analysis/genepool_predicted.RDS")

genepool_na_ciat <- genepool_na[genepool_na$SOURCE == "CIAT",]
saveRDS(genepool_na_ciat, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Results/classification_analysis/genepool_predicted_ciat.RDS")
