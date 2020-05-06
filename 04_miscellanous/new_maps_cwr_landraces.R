# Landrace Gap Analysis: PNG maps 
# M.Victoria Díaz
# CIAT, 2020



library(lattice)
library(latticeExtra)
library(RColorBrewer)
pacman::p_load(raster, rasterVis, maptools, tidyverse, xlsx, latticeExtra, sp)


create_cwr_landraces_maps <- function(crop, 
                            grph_dir, type){
  
  
  if(type == "landraces"){
    
    rsin<- raster(paste0("//dapadfs/workspace_cluster_9/gap_analysis_landraces/runs/LGA_documents/gaps_richness_per_crop/", crop, ".tif"))
    
  }else{
    
    rsin<- raster(paste0("//dapadfs/workspace_cluster_9/gap_analysis_landraces/runs/LGA_documents/gaps_richness_per_crop/cwr_gap_richness/", crop, "_crop_richness.tif"))
    
  }
  
  rsin[which(rsin[] == 0)] <- NA
  
  # Loading country shapefile
  data(wrld_simpl)
  
  # Creating reference lines
  grat <<- sp::gridlines(wrld_simpl, easts = seq(-180, 180, by = 10), norths = seq(-90, 90, by = 15))
  
  
  xyp <- raster::rasterToPoints(rsin)
  ext <- raster::extent(xyp[,1:2])
  ext <- raster::alignExtent(ext, rsin, snap = 'out')
  rsin <- raster::crop(rsin, ext)
  
  rsin <- ratify(rsin)
  rat <- levels(rsin)[[1]]
  

  rat$level <- 1:nrow(rat)
  
  
  if(nrow(rat) == 1){
    
    cols<- "red"
  }else{
    
    cols<- colorRampPalette(c("yellow", "red"))(nrow(rat))
    
  }
  
  
  levels(rsin) <- rat
  
  ht <- 12
  fct <- (rsin@extent@xmin-rsin@extent@xmax)/(rsin@extent@ymin-rsin@extent@ymax)
  wt <- ht*(fct+.1)
  
  mThm <- rasterTheme(region = brewer.pal(9, "YlGn"),
                      panel.background = list(col = "white")) 

    p <- rasterVis:::levelplot(rsin,
                             att = 'level',
                             margin = F,
                             par.settings = mThm,
                             col.regions = cols,
                             maxpixels = ncell(rsin)) + 
    latticeExtra::layer(sp.lines(grat, lwd = 0.5, lty = 2, col = "gray")) +
    latticeExtra::layer(sp.polygons(wrld_simpl, lwd = 0.8, col = "black", fill = "white"), under = T)
  
  
  png(paste0(grph_dir, "/",  names(rsin), ".png"), height = 7, width = 10, units = "in", res = 300)
  print(p)
  dev.off()
  cat("Map ", names(rsin), "successfully created. \n")
 
  rm(rsin, p, wrld_simpl, grat)
  
  
}


############Test

crop <- list.files("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/LGA_documents/gaps_richness_per_crop", pattern = ".tif$")
crop<- gsub(".tif", "", crop)

lapply(1:length(crop), function(i){

  cat(i, "\n")
  
    create_cwr_landraces_maps(crop = crop[i], type = "landraces",
                            grph_dir = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/LGA_documents/gaps_richness_per_crop/landraces_maps/dark" )
  

  
})


crop <- list.files("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/LGA_documents/gaps_richness_per_crop/cwr_gap_richness", pattern = ".tif$")
crop<-gsub("_crop_richness.tif", "", crop)

lapply(1:length(crop), function(i){
  
create_cwr_landraces_maps(crop = crop[i], type = "wild",
                          grph_dir = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/LGA_documents/gaps_richness_per_crop/cwr_maps" )
})

  