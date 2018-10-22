pacman::p_load(rmarkdown, leaflet, knitr, kableExtra, tidyverse, raster)


prepare_raster <- function(r){
  xyp <- raster::rasterToPoints(r)
  ext <- raster::extent(xyp[,1:2])
  ext <- raster::alignExtent(ext, r, snap = 'out')
  r <- raster::crop(r, ext)
  
  ext <- extent(r)
  lat_long_cent <- c((ext[1] + ext[2])/2,(ext[3] + ext[4])/2) 
  r <- ratify(r)
  rat <- levels(r)[[1]]
  rat$level <- c('Well conserved', 'Gap by one approach', 'Gap by both approaches')
  levels(r) <- rat
  
  return(r)
  
}

r_list <- list(
  "ajanhuiri" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/potato/lvl_1/ajanhuiri/americas/gap_models/gap_class_final.tif",
  "tuberosum_andigenum" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/potato/lvl_1/tuberosum_andigenum/americas/gap_models/gap_class_final.tif",
  "tuberosum_chilotanum" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/potato/lvl_1/tuberosum_chilotanum/americas/gap_models/gap_class_final.tif"
)
r <- lapply(r_list, function(i){
  
  r <- prepare_raster(r = raster(i))
  return(r)
  
})



  pal <- colorFactor(c("#BDBDBD", "#00BFFF", "#013ADF"), domain = r[],
                    na.color = "transparent")

leaflet() %>% setView(lng = lat_long_cent[1], lat = lat_long_cent[2], zoom = 3) %>%
  addProviderTiles(providers$Esri.WorldStreetMap, options = providerTileOptions(noWrap = TRUE))%>%
  addRasterImage(group= names(r)[1], r[[1]] , colors = pal, opacity = 0.8)  %>%
  addRasterImage(group= names(r)[2], r[[2]], colors = pal, opacity = 0.8) %>%
  addRasterImage(group= names(r)[3], r[[3]], colors = pal, opacity = 0.8) %>% addLegend("bottomright", colors = c("#BDBDBD", "#00BFFF", "#013ADF"), values = unique(r[[3]]),
                                                                                        labels = c('Well conserved', 'Gap by one approach', 'Gap by both approaches'),
                                                                                        title = "Gap status", opacity = 1) %>%
  addLayersControl(baseGroups = c(names(r)[1], names(r)[2],names(r)[3]))
  
  
  


