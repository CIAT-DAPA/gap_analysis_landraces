
suppressMessages(if(!require(pacman)){install.packages('pacman'); library(pacman)} else {library(pacman)})
pacman::p_load(dplyr, psych, tm, raster, rgdal, rasterVis, rgeos, deldir, sp, tidyverse, FactoMineR, factoextra, ggdendro, rlang, fastcluster ) 

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
root     <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9/gap_analysis_landraces", "Windows" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces")


#
wmask <- raster(paste0(root,"/Input_data/mask_wb_c_ant.tif") )

cost <- raster(paste0(root,"/Input_data/_friction_surface/cost_distances/prueba_cost4OSM.tif") )
dgp1 <- raster(paste0(root,"/Input_data/raster_sdm/dist_to_GP1.tif") )



delanuy <- shapefile(paste0(root,"/Results/accession_density/Andean/delaunay_triang.shp") )

ocur <- readRDS(paste0(root,"/Input_data/_datosAndres/acp/data4modeling.RDS" ) )
ocur <- ocur %>% dplyr::select(., Longitude, Latitude, Analysis) %>% filter(., Analysis == "Americas") %>% dplyr::select(., 1:2)
sp_ocur <- SpatialPoints(ocur, proj4string = crs(wmask))

am_shp <- shapefile(paste0(root,"/Input_data/_datosAndres/americas_shape.shp") )




ov <- sp::over(sp_ocur, am_shp)
am_ocur <- ocur[which(!is.na(ov$NAME)), ]
am_ocur <- SpatialPointsDataFrame(am_ocur,data= data.frame( presc = rep(1,nrow(am_ocur))) , proj4string = crs(wmask) )
writeOGR(am_ocur, dsn = paste0(root,"/Input_data/_datosAndres/beans_sp.shp" ), driver = "ESRI Shapefile", layer="presencias"  )

crs(delanuy) <- crs(wmask)
delanuy$area <- area(delanuy)/1000000
delanuy <- delanuy[delanuy$area >= 0.9,]

summary(delanuy$area)

stk <- raster::stack(cost, delanuy) 





cost_sdm <- raster::mask(x = cost, mask = delanuy[delanuy$ID == 1038,] )
dist_sdm <- raster::mask(x = dgp1, mask = delanuy[delanuy$ID == 1038,] )

stk <- raster::stack(lagran, dist)
rvl <- raster::rasterToPoints(x = stk)
rvl

### Script to make a delanuay triangulation
crds <- am_ocur@coords
dup <- duplicated(crds)
crds <- crds[!dup, ]

dela <- deldir(crds[,1], crds[,2] )
w <- triang.list(dela)
polys <- vector(mode='list', length=length(w))
centroids<- matrix(0,nrow = length(w) , ncol = 2)

for (i in seq(along=polys)) {
  pcrds = cbind(w[[i]]$x, w[[i]]$y)
  pcrds = rbind(pcrds, pcrds[1,])
  polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  centroids[i,] <- rbind(gCentroid(SpatialPolygons(list(polys[[i]]), proj4string = crs(wmask)) )@coords)
  
  
}
SP <- SpatialPolygons(polys, proj4string = crs(wmask))

delanuay <- SpatialPolygonsDataFrame(SP, data=data.frame( ID=sapply(slot(SP, 'polygons'), 
                                                                    function(x) slot(x, 'ID')), centroid_x= centroids[,1], 
                                                          centroid_y=centroids[,2] , 'area'  = area(SP)/1000000) )

writeOGR(delanuay,paste0(root,"/Input_data/_datosAndres/delanuay"), driver = "ESRI Shapefile", layer="delanuay_andres", overwrite_layer= TRUE )

#End delanuay triangulation

#Check and fix some errors
df <- delanuay@data
plot(delanuay[delanuay$ID == 958,])


### ACP and Clustering proposal


cost <- raster(paste0(root,"/Input_data/_friction_surface/cost_distances/prueba_cost4OSM.tif"))
dgp1 <- raster(paste0(root,"/Input_data/raster_sdm/dist_to_GP1.tif") )
slope <- raster(paste0(root,"/Input_data/raster_sdm/Slope.tif"))
sdm_rast <- raster(paste0(root,"/Results/SDM_modelling/Andean/prj_models/Andean_prj_median.tif"))

knl_M <- raster(paste0(root, "/Input_data/_kernel_densities/Mesoamerican__Kernel.tif") )
knl_A <- raster(paste0(root, "/Input_data/_kernel_densities/Andean__Kernel.tif") )

#kernel density made in Argcis
Knl_W <- raster(paste0(root, "/Input_data/_kernel_densities/kernel_arcgis_gnpl.tif" ))

fsurfc <- raster(paste0(root, "/Input_data/_friction_surface/frsurface_5.tif") )


cost_dt <- raster::crop(x = cost, y = raster::extent(sdm_rast)) 
cost_dt <- raster::mask(x = cost_dt, mask = sdm_rast )

dgp1_mk <- raster::crop(x= dgp1, y = extent(sdm_rast))
dgp1_mk <- raster::mask(x= dgp1_mk, mask = sdm_rast )

slope_mk <- raster::crop(x = slope, y = extent(sdm_rast) )
slope_mk <- raster::mask(x = slope_mk, mask = sdm_rast)

knl_M_mk <- raster::crop(x= knl_M, y= extent(sdm_rast))
knl_M_mk <- raster::mask(x = knl_M_mk , mask= sdm_rast)

knl_A_mk <- raster::crop(x= knl_A, y= extent(sdm_rast))
knl_A_mk <- raster::mask(x = knl_A_mk , mask= sdm_rast)


knl_W_mk <- raster::crop(x= knl_W, y= extent(sdm_rast))
knl_W_mk <- raster::mask(x = knl_W_mk , mask= sdm_rast)

fsurfc_mk <- raster::crop(x = fsurfc, extent(sdm_rast) )
fsurfc_mk <- raster::mask(x = fsurfc_mk, mask = sdm_rast)

r_stk <- raster::stack(sdm_rast,dgp1_mk ,cost_dt, slope_mk, knl_W_mk )
bd_acp <- raster::rasterToPoints(x = r_stk)
bd_acp <- as.data.frame(bd_acp)
bd_acp$cellID <- 1:nrow(bd_acp)
bd_acp <- bd_acp[complete.cases(bd_acp),]



pca <- FactoMineR::PCA(X= bd_acp[,-c(1,2,ncol(bd_acp))], scale.unit = T, graph = T)

clust <- HCPC(pca$ind$coord[,1:2], graph = F)


#Clustering 
clust <- fastcluster::hclust.vector(pca$ind$coord[,1:2], method = "ward", metric = "euclidean")
save(clust, file = paste0(root,"/Scripts/finding_gaps/clust.RData"))

#cargar archivo .Rdata con la clusterización
load(paste0(root, "/finding_gaps/clust.RData" ))





# Plot dendogram
hcd <- as.dendrogram(clust)
hcd_cut <- cut(hcd, h= 75)
plot(hcd_cut$upper) #there is 3 or five clusters


memb <- cutree(clust, k=3)
df_acp <- data.frame( pca$ind$coord[,1:2], clust=memb)
ggplot(df_acp, aes(x = Dim.1, y = Dim.2)) + geom_point(aes(color = clust))


ggplot(df_acp, aes(x = Dim.1, y = Dim.2)) + geom_point(aes(color = clust))

#descriptivos para cada cluster

bd_acp2 <- data.frame( bd_acp, clust = memb)

#plots

ggplot(bd_acp2, aes(x = x, y = y)) + geom_point(aes(color = factor(clust) ))

#descritptivos
#means <- bd_acp2 %>% dplyr::select(.,3:(ncol(bd_acp2)-2),clust) %>% group_by(., clust )  %>% summarise_each(., funs(mean) ) 

descrip <- bd_acp2 %>% dplyr::select(.,3:(ncol(bd_acp2)-2),clust) 
descrip$clust <- factor(descrip$clust)
catdes(descrip, num.var=5 )

hist(bd_acp2[bd_acp2$clust==4,"Slope"])
abline(v = mean(bd_acp2$Slope), col="red" )
abline(v = median(bd_acp2$Slope), col="blue" )

clust3 <- bd_acp2 %>% dplyr::filter(., clust == 3)  %>% dplyr::select(., 1:3) %>% dplyr::filter(., Andean_prj_median >= 0.7726 )

clust3_sp <- sp::SpatialPointsDataFrame( coords = clust3[,1:2], data = data.frame( probs = clust3[,3]), proj4string = crs(sdm_rast) )

clust3_rs <- raster::rasterize( x = clust3_sp, y = sdm_rast, field = clust3_sp$probs  )


clust3_rs <- raster::crop( x = clust3_rs, y = raster::extent(sdm_rast))

pres <- raster::mask(x = clust3_rs ,mask = sdm_rast)

writeRaster(pres,filename= paste0(root,"/Results/finding_gaps/highprobs_cluster3.tif") , format="GTiff", overwrite = T )

summary(clust3$Andean_prj_median)
abline(v=0.7726,col="red")


mcl <- Mclust(pca$ind$coord[,1:2])

plot(mcl)
summary(mcl)




























for(i in 1:100000){
  a <- rep("*",sample(c(1:15),1))
  cat("Procesando", i/100000*100,"%",a, "\n")
  Sys.sleep(1)
}
  









