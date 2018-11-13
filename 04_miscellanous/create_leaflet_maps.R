pacman::p_load(rmarkdown, leaflet, knitr, kableExtra, tidyverse, raster, ggplot2)


prepare_raster <- function(r){
  xyp <- raster::rasterToPoints(r)
  ext <- raster::extent(xyp[,1:2])
  ext <- raster::alignExtent(ext, r, snap = 'out')
  r <- raster::crop(r, ext)
  
   
  r <- ratify(r)
  rat <- levels(r)[[1]]
  rat$level <- c('Well conserved', 'Gap by one approach', 'Gap by both approaches')
  levels(r) <- rat
  
  return(r)
  
}

r_list <- list(
  "andean" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/common_bean/lvl_1/andean/americas/gap_models/gap_class_final.tif",
  "mesoamerican" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/common_bean/lvl_1/mesoamerican/americas/gap_models/gap_class_final.tif"
)
r <- lapply(r_list, function(i){
  
  r <- prepare_raster(r = raster(i))
  return(r)
  
})

ext <- extent(r[[1]])
lat_long_cent <- c((ext[1] + ext[2])/2,(ext[3] + ext[4])/2)

  pal <- colorFactor(c("#BDBDBD", "#00BFFF", "#013ADF"), domain = r[[1]][],
                    na.color = "transparent")

leaflet() %>% setView(lng = lat_long_cent[1], lat = lat_long_cent[2], zoom = 3) %>%
  addProviderTiles(providers$Esri.WorldStreetMap, options = providerTileOptions(noWrap = TRUE))%>%
  addRasterImage(group= names(r)[1], r[[1]] , colors = pal, opacity = 0.8)  %>%
  addRasterImage(group= names(r)[2], r[[2]], colors = pal, opacity = 0.8) %>%
  addRasterImage(group= names(r)[3], r[[3]], colors = pal, opacity = 0.8) %>% addLegend("bottomright", colors = c("#BDBDBD", "#00BFFF", "#013ADF"), values = unique(r[[3]]),
                                                                                        labels = c('Well conserved', 'Gap by one approach', 'Gap by both approaches'),
                                                                                        title = "Gap status", opacity = 1) %>%
  addLayersControl(baseGroups = c(names(r)[1], names(r)[2],names(r)[3]))
  
  
  
######## PLOT ALL RASTERS IN ONE PLOT using rastervis
r_list <- list(
  "bicolor" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/sorghum/lvl_1/bicolor/americas/gap_models/gap_class_final.tif",
  "caudatum" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/sorghum/lvl_1/caudatum/americas/gap_models/gap_class_final.tif",
  "durra" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/sorghum/lvl_1/durra/americas/gap_models/gap_class_final.tif",
  "guinea" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/sorghum/lvl_1/guinea/americas/gap_models/gap_class_final.tif",
  "kafir" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/sorghum/lvl_1/kafir/americas/gap_models/gap_class_final.tif"
)

rsin <- lapply(r_list, function(i){
  
rsin <-  raster(i)
rsin <- ratify(rsin)
rat <- levels(rsin)[[1]]
rat$level <- c('Well conserved', 'Gap by one approach', 'Gap by both approaches')
levels(rsin) <- rat

return(rsin)
})

rsin <- raster::stack(rsin)
xyp <- raster::rasterToPoints(rsin)
ext <- raster::extent(xyp[,1:2])
ext <- raster::alignExtent(ext, rsin, snap = 'out')
rsin <- raster::crop(rsin, ext)
data(wrld_simpl)
grat <<- sp::gridlines(wrld_simpl, easts = seq(-180, 180, by = 10), norths = seq(-90, 90, by = 15))


#figure details
ht <- 12
fct <- (rsin@extent@xmin-rsin@extent@xmax)/(rsin@extent@ymin-rsin@extent@ymax)
wt <- ht*(fct+.1)

# My theme
mThm <- rasterTheme(region = brewer.pal(9, "YlGn"),
                    panel.background = list(col = "#708090"))
#produce levelplot
cols <- c("#BDBDBD", "#00BFFF", "#013ADF")
p <- rasterVis:::levelplot(rsin,
                           att = 'level',
                           margin = F,
                           par.settings = mThm,
                           col.regions = cols,
                           maxpixels = ncell(rsin)) + 
  latticeExtra::layer(sp.lines(grat, lwd = 0.5, lty = 2, col = "white")) +
  latticeExtra::layer(sp.polygons(wrld_simpl, lwd = 0.8, col = "black", fill = "#4B4B4B"), under = T)

png("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_datosAndres/presentation/bean_map(rastervis).png", height = 7, width = 10, units = "in", res = 300)
print(p)
dev.off()


##################   USING GGPLOT AND TABLES ########################
r_list <- list(
  "andean" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/common_bean/lvl_1/andean/americas/gap_models/gap_class_final.tif",
  "mesoamerican" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/common_bean/lvl_1/mesoamerican/americas/gap_models/gap_class_final.tif"
)

rsin <- lapply(r_list, function(i){
  
  rsin <-  raster(i)
  #rsin <- ratify(rsin)
  #rat <- levels(rsin)[[1]]
  #rat$level <- c('Well conserved', 'Gap by one approach', 'Gap by both approaches')
  #levels(rsin) <- rat
  
  return(rsin)
})


rsin2 <- raster::stack(rsin)
rsin2[which(rsin2[] != 2)] <- NA
rsin2
rsin_points <- rasterToPoints(rsin2)
current_table <- rsin_points %>% as.data.frame() %>% dplyr::mutate(., mesoamerican = 3 + mesoamerican, type = ifelse(is.na(andean), mesoamerican, ifelse(is.na(mesoamerican), andean, ifelse(!is.na(andean) & !is.na(mesoamerican), andean + mesoamerican, NA))                                                                                                                                                                                                                   )) 


shp <- shapefile("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/shapefiles/world_shape_simplified/all_countries_simplified.shp")
shp <- shp[shp@data$REGION == 19,]

gg2 <- current_table %>% ggplot(aes(x = x, y = y, fill = factor(type))) +
  geom_tile() +
  scale_fill_manual(labels = c("Andean gaps", "Mesoamerican gaps", "Overlap"), values = c("#7AD8BD", "#2170AD", "#1A0B61")) + # Paired, Set2, Accent
  labs(fill = '') +
  geom_polygon(data = shp, aes(x = long, y = lat, group = group), color = 'black', fill = 'NA', linetype = 1, size = 0.5  ) +
  coord_equal() +
  xlab('Longitude') + 
  ylab('Latitude') +
  theme_bw() + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right')+
    coord_cartesian(xlim = c(-120, -20), ylim = c(-40, 35))



ggplot2::ggsave(filename = "./current_Set2.jpeg", plot = gg2, device = "jpeg", units = "in", width = 10, height = 6, dpi = 600)




