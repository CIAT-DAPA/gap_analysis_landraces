# Landrace Gap Analysis: Gap proposals
# CIAT, 2017
# C. Sosa, A. Mendez, H. Achicanoy

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)

OSys <- Sys.info()[1]
OSysPath <- switch(OSys,
                   "Linux" = "/mnt",
                   "Windows" = "//dapadfs",
                   "Darwin" = "")
wk_dir   <- switch(OSys,
                   "Linux" = "/mnt/workspace_cluster_9/Sustainable_Food_System/SFS_indicators",
                   "Windows" = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators",
                   "Darwin" = "")
setwd(wk_dir); rm(wk_dir, OSysPath, OSys)

# Load packages
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(maptools)){install.packages('maptools'); library(maptools)} else {library(maptools)})
suppressMessages(if(!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)} else {library(tidyverse)})
suppressMessages(if(!require(sf)){install.packages('sf'); library(sf)} else {library(sf)})
suppressMessages(if(!require(sp)){install.packages('sp'); library(sp)} else {library(sp)})

# Probability raster
pr_map <- raster::raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Results/SDM_modelling/Andean/prj_models/Andean_prj_median.tif")
# Cost distance
costDt <- raster::raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_friction_surface/cost_distances/prueba_cost4OSM.tif")

slope <- raster::raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/raster_sdm/Slope.tif")
distGP <- raster::raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/raster_sdm/dist_to_GP1.tif")

costDt <- raster::crop(x = costDt, y = raster::extent(pr_map))
costDt <- raster::mask(x = costDt, mask = pr_map)
slope <- raster::crop(x = slope, y = raster::extent(pr_map))
slope <- raster::mask(x = slope, mask = pr_map)
distGP <- raster::crop(x = distGP, y = raster::extent(pr_map))
distGP <- raster::mask(x = distGP, mask = pr_map)

rst_stck <- raster::stack(pr_map, costDt, slope, distGP)
rst_val <- raster::rasterToPoints(x = rst_stck)
rst_val <- as.data.frame(rst_val)
rst_val$cellID <- 1:ncell(rst_stck)
rst_val <- rst_val[complete.cases(rst_val),]

library(FactoMineR)
library(factoextra)

pca_tst <- FactoMineR::PCA(X = rst_val[,-(1:2)], scale.unit = T, graph = T)

rst_val2 <- rst_val
rst_val2$prueba_cost4OSM <- -1*(rst_val2$prueba_cost4OSM)
rst_val2$dist_to_GP1 <- -1*(rst_val2$dist_to_GP1)

pca_tst2 <- FactoMineR::PCA(X = rst_val2[,-(1:2)], scale.unit = T, graph = T)

tmplt <- pr_map
tmplt[] <- NA

tmplt[cellFromXY(object = tmplt, xy = rst_val2[,1:2])] <- pca_tst2$ind$coord[,1]

writeRaster(tmplt, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/pca_results.tif")
saveRDS(object = rst_val, file = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/pca_data.RDS")

pca_rslts <- list(First = pca_tst,
                  Second = pca_tst2)
saveRDS(object = pca_rslts, file = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/pca_components.RDS")

library(pcaMethods)
rst_val <- readRDS("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/pca_data.RDS")
rstNlPCA <- pca(rst_val, nPcs = 1, method = "nlpca", maxSteps = 50)

pca_rslts <- readRDS("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/pca_components.RDS")

pairs(cbind(rstNlPCA@scores[,1], pca_rslts$First$ind$coord[,1], pca_rslts$Second$ind$coord[,1]))
hist(rstNlPCA@scores[,1])
summary(rstNlPCA@scores[,1])

tmplt2 <- pr_map
tmplt2[] <- NA
tmplt2[raster::cellFromXY(object = tmplt2, xy = rst_val[,1:2])] <- rstNlPCA@scores[,1]
plot(tmplt2)
writeRaster(tmplt2, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/nlpca_results.tif")
