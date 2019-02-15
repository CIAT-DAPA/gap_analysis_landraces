# Landrace Gap Analysis: Preparing nice maps
# H. Achicanoy, Andres camilo mendez
# CIAT, 2018



# Create combined thresholded gap maps
create_png_maps <- function(summ_filepath     , 
                                 rast_dirPath ,
                                 grph_dir     ,
                                 occName      ,
                                 sdm_filepath ,
                                 occ_filepath , 
                                 colors = list(two_cols =  c('grey70', 'red2') , three_cols = c('grey70', 'goldenrod3', 'red2')), 
                                 new_ext = NULL){
  
  pacman::p_load(raster, rasterVis, maptools, tidyverse, xlsx, latticeExtra, sp)
  
  #import summary table and extrac the thresholds
  summ_table <- read.xlsx(summ_filepath, sheetName = "summary")
  thresh_cost_dist <- as.numeric(as.character(summ_table[7,7])) #6
  thresh_delaunay <- as.numeric(as.character(summ_table[15,7])) #13
  
  #import gap score rasters
  cost_dist_rast <- raster(paste0(rast_dirPath, "/gap_score_cost_dist.tif"))
  dela_rast      <- raster(paste0(rast_dirPath, "/gap_score_delaunay.tif"))
  
  #reclassify and save both rasters using the thressholds if they doesn't exists
  if(!file.exists(paste0(rast_dirPath, "/gap_class_cost_dist.tif"))){
    cost_dist_rast[which(cost_dist_rast[] >= thresh_cost_dist )] <- 1
    cost_dist_rast[which(cost_dist_rast[] != 1)] <- 0
    names(cost_dist_rast) <- "gap_class_cost_dist"
    writeRaster(cost_dist_rast, paste0(rast_dirPath, "/gap_class_cost_dist.tif"), overwrite = TRUE)
  }else{cost_dist_rast <- raster(paste0(rast_dirPath, "/gap_class_cost_dist.tif"))}
  
 if(!file.exists( paste0(rast_dirPath, "/gap_class_delaunay.tif"))){
   dela_rast[which(dela_rast[] >= thresh_delaunay)] <- 1
   dela_rast[which(dela_rast[] != 1)] <- 0
   names(dela_rast)<- "gap_class_delaunay"
   writeRaster(dela_rast, paste0(rast_dirPath, "/gap_class_delaunay.tif"), overwrite = TRUE)
 }else{dela_rast <- raster(paste0(rast_dirPath, "/gap_class_delaunay.tif"))}
  
 if(!file.exists(paste0(rast_dirPath, "/gap_class_final.tif"))){
   final_gap_rast <- dela_rast + cost_dist_rast
   names(final_gap_rast) <- "gap_class_final"
   writeRaster(final_gap_rast, paste0(rast_dirPath, "/gap_class_final.tif"), overwrite = TRUE)
 }else{final_gap_rast <- raster(paste0(rast_dirPath, "/gap_class_final.tif"))}
  
  rm( thresh_delaunay, thresh_cost_dist, summ_table); g <- gc(); rm(g)

  rst_list <- list(cost_dist_rast, dela_rast, final_gap_rast)
  
  # Loading country shapefile
  data(wrld_simpl)
  
  # Creating reference lines
  grat <<- sp::gridlines(wrld_simpl, easts = seq(-180, 180, by = 10), norths = seq(-90, 90, by = 15))
  # creating fancy gap class maps 
  lapply(rst_list, function(rsin){
    
    if(!file.exists(paste0(grph_dir, "/", occName, "_", names(rsin), ".png"))){
    #rsin <- rsin %>% crop_raster_by_extent
    xyp <- raster::rasterToPoints(rsin)
    ext <- raster::extent(xyp[,1:2])
    ext <- raster::alignExtent(ext, rsin, snap = 'out')
    rsin <- raster::crop(rsin, ext)
    
    #prepare raster
    if (!is.null(new_ext)) {rsin <- crop(rsin, new_ext)}
    rsin <- ratify(rsin)
    rat <- levels(rsin)[[1]]
    
    #control the labels dependig the number of levels
    ifelse(nrow(rat) == 3, rat$level <- c('Well conserved', 'Gap by one approach', 'Gap by both approaches'), 
           rat$level <- c('Well conserved', 'High priority'))
    
    #control the color panel denpending of the number of levels
    if(nrow(rat) == 2){cols <- colors$two_cols}
    if(nrow(rat) == 3){cols <- colors$three_cols}
    
    levels(rsin) <- rat
    
    #figure details
    ht <- 12
    fct <- (rsin@extent@xmin-rsin@extent@xmax)/(rsin@extent@ymin-rsin@extent@ymax)
    wt <- ht*(fct+.1)

    # My theme
    mThm <- rasterTheme(region = brewer.pal(9, "YlGn"),
                        panel.background = list(col = "#708090"))
    #produce levelplot
    p <- rasterVis:::levelplot(rsin,
                               att = 'level',
                               margin = F,
                               par.settings = mThm,
                               col.regions = cols,
                               maxpixels = ncell(rsin)) + 
      latticeExtra::layer(sp.lines(grat, lwd = 0.5, lty = 2, col = "white")) +
      latticeExtra::layer(sp.polygons(wrld_simpl, lwd = 0.8, col = "black", fill = "#4B4B4B"), under = T)
    
    
    png(paste0(grph_dir, "/", occName, "_", names(rsin), ".png"), height = 7, width = 10, units = "in", res = 300)
    print(p)
    dev.off()
    cat("Map ", names(rsin), "successfully created. \n")
    }else{
      cat("Map ", names(rsin), "is already created. \n")
    }
  })#end lapply
  
  #create other raster fancy maps
  
  #select the rasters to be mapped
 pos <- setdiff(grep(pattern = "cost|delaunay|score", x = list.files(rast_dirPath, pattern = "\\.tif$" )), grep(pattern = "class", x = list.files(rast_dirPath, pattern = "\\.tif$" )))
 rasts <- c(list.files(rast_dirPath, pattern = "\\.tif$" ,full.names = TRUE)[pos], sdm_filepath)
 
 #import ocurrences

 lapply(rasts, function(filepath){
   
   r_temp <- raster(filepath)
   occ <<- shapefile(occ_filepath)
   
   #control whether use or not the occurence shapefile
   if(!file.exists(paste0(grph_dir, "/", occName, "_", names(r_temp), ".png"))){
     
   if(!grepl(pattern = "/cost_dist.tif|/delaunay.tif", filepath)){occ <<- NULL}
   
   # Crop raster according with the extent function
   xyp <- raster::rasterToPoints(r_temp)
   ext <- raster::extent(xyp[,1:2])
   ext <- raster::alignExtent(ext, r_temp, snap = 'out')
   r_temp <- raster::crop(r_temp, ext)
   
   # My theme
   mThm <- rasterTheme(region = brewer.pal(9, "YlGn"),
                       panel.background = list(col = "#708090"))
   
   
   if(r_temp@data@max <= 1){
     p <- rasterVis::levelplot(r_temp,
                               at = seq(0, 1, .05),
                               margin = F,
                               par.settings = mThm,
                               maxpixels = ncell(r_temp)) + 
       latticeExtra::layer(sp.lines(grat, lwd = 0.5, lty = 2, col = "white")) +
       latticeExtra::layer(sp.polygons(wrld_simpl, lwd = 0.8, col = "black", fill = "#4B4B4B"), under = T) +
       latticeExtra::layer(sp.points(occ, pch = 20, cex = .9, col = "red"))
     
   } else {
     p <- rasterVis::levelplot(r_temp,
                               att = 'level',
                               margin = F,
                               par.settings = mThm,
                               maxpixels = ncell(r_temp)) + 
       latticeExtra::layer(sp.lines(grat, lwd = 0.5, lty = 2, col = "white")) +
       latticeExtra::layer(sp.polygons(wrld_simpl, lwd = 0.8, col = "black", fill = "#4B4B4B"), under = T) + 
       latticeExtra::layer(sp.points(occ, pch = 20, cex = .9, col = "red"))
    
   }
   
   png(paste0(grph_dir, "/", occName, "_", names(r_temp), ".png"), height = 7, width = 10, units = "in", res = 300)
   print(p)
   dev.off()
   cat("Map ", names(r_temp), "successfully created \n")
   }else{
     cat("Map",names(r_temp), "is already created.")
   }
 })#end lapply
 
 
  cat("\n Process done. Please check the path:", grph_dir, "\n")

  p_unload(rasterVis, maptools, xlsx,latticeExtra)
  }

# create_png_maps( summ_filepath= paste0(gap_valDir, "/buffer_100km/validation_results.xlsx"), 
#                   rast_dirPath = paste0(results_dir, "/", crop, "/", level, "/", occName, "/", region, "/gap_models"),
#                   grph_dir     = paste0(results_dir, "/", crop, "/lvl_1/", occName, "/", region, "/graphics"),
#                   occName      = occName,
#                   sdm_filepath = paste0(model_outDir, "/", occName, "_prj_median.tif"),
#                   occ_filepath = paste0(occDir, "/Occ.shp"), 
#                   colors       = list(two_cols =  c('grey70', 'red2') , three_cols = c('grey70', 'goldenrod3', 'red2')), 
#                   w_ext        = NULL)


