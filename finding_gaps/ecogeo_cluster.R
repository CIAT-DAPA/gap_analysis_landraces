
#SCRIPT CREATED BY: ANDRES CAMILO MENDEZ
#THIS SCRIPT COMES WITH ABSOLUTELY NO WARRANTY
#FEEL FREE TO USE IN THE GAP ANALISYS FOR LANDRACES PROJECT

#THIS SCRIPT ALLOW YOU TO CONSTRUCT A ECO-GEOGRAPHIC CLUSTER USING MAHALANOBIS DISTANCES AND WARD METHOD TO CONSOLIDATE THE CLUSTERS


                #### FUNCTION TO CREATE ECOGEOGRAFICAL CLUSTER##############
ecogeo_clustering <- function( n.sample = 10000, k.clust = 11){
  
  
  # Analysis region: "americas", "world"
cat(red$bgWhite$bold("Importing packages..... \n \n \n"))
  
  suppressMessages(if(!require(pacman)){install.packages('pacman'); library(pacman)} else {library(pacman)})
  pacman::p_load(dplyr, psych, tm, raster, rgdal, rasterVis, rgeos, deldir, sp, tidyverse, FactoMineR, factoextra, ggdendro, 
                 rlang, fastcluster, sf , sdm, wordspace, ff, ggplot2, cluster, parallelDist, clusterR, caret, crayon) 
  cat("Finished Importing process  \n   \n \n")
  
  
  #************************************************************************************************************#
  #************************************************************************************************************#
  #***************************** BEGIN FUNCTION TO CREATE A ECOGEOGRAFICAL CLUSTER *************************#
  #************************************************************************************************************#
  #************************************************************************************************************#
  cat( red$bgBlue$bold("Importin SDM raster \n   \n \n"))

  SDM <- raster(paste0(model_outDir, "/",occName,"_prj_median.tif ")) 
  
  #selec rasters
  
  sdm_obj <- read.sdm(paste(sp_Dir,"/sdm.sdm",sep=""))
  var_names <- names(sdm_obj@data@features)[2:ncol(sdm_obj@data@features)]
  
  var_names <- paste0(var_names,".tif")
  # vars <- c("Accessibility"        ,    "Altitude"       ,          "aridityIndexThornthwaite" ,"bio_14"  ,                
  #  "bio_15"  ,                "bio_18"    ,               "bio_19"    ,               "bio_2"   ,                
  # "climaticMoistureIndex" ,   "continentality"     ,      "dist_rivers"         ,     "embergerQ" ,              
  # "Irrigation"          ,    "minTempWarmest"        ,   "monthCountByTemp10"     ,  "PETDriestQuarter" ,       
  # "PETWarmestQuarter"      ,  "PETWettestQuarter"     ,   "Physical.area")
  
  pos <- which(list.files(path = climDir ) %in% var_names)
  
  
  cat( black$bgWhite$blod("Importin WorldClim and Envirem rasters \n   \n  \n")  )
  cat( red$bgWhite$blod("Take care of your RAM  \n")  )
  environ <- list.files(path = climDir, full.names=TRUE)[pos]%>% 
                raster::stack(.)%>% raster::crop(x = ., extent(SDM) )%>% 
                 raster::mask(x = ., mask = SDM) 
                   

environ_df <- environ %>% raster::rasterToPoints(x=.) %>%  as.data.frame(.) %>%
                      dplyr::mutate(., ID = 1:nrow(.)) %>%
                       dplyr::select(., ncol(.) , 1:(ncol(.)-1) )

rm(pos, var_names, sdm_obj); g <- gc; rm(g); removeTmpFiles(h=1)


rownames(environ_df) <- environ_df$ID

environ_df_in <- environ_df[,-c(1,2,3)]
rownames(environ_df_in) <- rownames(environ_df)
#environ <- environ[complete.cases(environ),]

environ_scaled <- scale(environ_df_in, center = T, scale = T)

####
cat( red$bgWhite$blod(paste("Starting custerin process whit: ", n.sample," Sample size \n   \n \n")  ))

set.seed(10000)
muestra <- sample(1:nrow(environ_df),10000)
df_temp <- environ_df[muestra,]; rownames(df_temp) <- rownames(environ_df[muestra, ])
df_temp_in <-  df_temp[, 4:ncol(df_temp)]

cat( red$bgGreen("Calculating Mahalanobis distances... \n   \n \n"))

mahaRed_dist <- parDist(as.matrix(df_temp_in), method = "mahalanobis")

cat( red$bgGreen("Hierarchical Clustering to distances using the WARD method \n   \n \n"))

clust_hc <- fastcluster::hclust(mahaRed_dist, method = "ward.D") 


memb <- cutree(clust_hc, k= k.clust)
environR_clust <- data.frame( df_temp, clust= factor(memb) )

cat( red$bgwhite$bold("Starting assignation of occurrences to each cluster using FDA \n   \n \n"))
cat("This can take several minutes \n   \n \n")

ctrol2 <-  trainControl(method = "LGOCV", p = 0.8, number = 10, savePredictions = T, verboseIter = TRUE )

set.seed(825)
cat(bold("Running Flexible Discriminant Analisys ...\n   \n \n"))

FDA <- train(clust ~ ., data = environR_clust[, 4:ncol(environR_clust)], method = 'bagFDA', trControl = ctrol2)
cat(green$bold("finishing Rforest ...\n   \n \n"))

to_assign <- environ_df[-muestra, ]
rownames(to_assign) <- rownames(environ_df[-muestra,])
to_assign$clust <-  predict(FDA, newdata = to_assign[,4:ncol(to_assign)]  )
to_assign$clust <- as.factor(to_assign$clust)
environR_clust$clust <- as.factor(environR_clust$clust)
environ_clust <- dplyr::bind_rows(environR_clust, to_assign)

cat( red$bgwhite$bold("Starting rasterization and saving process \n   \n  \n"))


 SPF <- sp::SpatialPointsDataFrame( coords = environ_clust[,2:3], data = data.frame( cluster = environ_clust[,ncol(environ_clust)] ), proj4string = crs(SDM) )

r <- raster()
extent(r) <- extent(SDM)
crs(r) <- crs(SDM)
res(r) <- res(SDM)

to_rasterize <- raster::rasterize( x = SPF, y = r, field = as.numeric(SPF$cluster)  )
#raster mahalanobis
writeRaster(to_rasterize,filename= paste0( gap_outDir, "/ecogeo_hclust_mahalanobis.tif") , format="GTiff", overwrite = T )

cat(red$bgWhite$bold(paste("Process Done... Pls check the Path:", gap_outDir,"\n   \n \n")))




}# ENDCLUSTER FUNCTION

 


