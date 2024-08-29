
#SCRIPT CREATED BY: ANDRES CAMILO MENDEZ
#THIS SCRIPT COMES WITH ABSOLUTELY NO WARRANTY
#FEEL FREE TO USE IN THE GAP ANALISYS FOR LANDRACES PROJECT

#THIS SCRIPT ALLOW YOU TO CONSTRUCT A ECO-GEOGRAPHIC CLUSTER USING MAHALANOBIS DISTANCES AND WARD METHOD TO CONSOLIDATE THE CLUSTERS


                #### FUNCTION TO CREATE ECOGEOGRAFICAL CLUSTER##############
ecogeo_clustering <- function(n.sample = 10000, var_names,k.clust = 11){
  

  # Analysis region: "americas", "world"
  
  
  
  cat("Importing SDM raster \n   \n \n")

  SDM <- raster(paste0(model_outDir, "/",occName,"_prj_median.tif ")) 
  
  #selec rasters
  
  # sdm_obj <- read.sdm(paste(sp_Dir,"/sdm.sdm",sep=""))
  # var_names <- names(sdm_obj@data@features)[2:ncol(sdm_obj@data@features)]
  # 
  #var_names <- paste0(var_names,".tif")

  # vars <- c("Accessibility"        ,    "Altitude"       ,          "aridityIndexThornthwaite" ,"bio_14"  ,                
  #  "bio_15"  ,                "bio_18"    ,               "bio_19"    ,               "bio_2"   ,                
  # "climaticMoistureIndex" ,   "continentality"     ,      "dist_rivers"         ,     "embergerQ" ,              
  # "Irrigation"          ,    "minTempWarmest"        ,   "monthCountByTemp10"     ,  "PETDriestQuarter" ,       
  # "PETWarmestQuarter"      ,  "PETWettestQuarter"     ,   "Physical.area")
  
  pos <- which(list.files(path = climDir ) %in% paste0(var_names, ".tif"))
  
  path <- list.files(path = climDir, full.names=TRUE)[pos]
  cat( "Importing WorldClim and Envirem rasters \n   \n  \n"  )
  cat( "Take care of your RAM  \n"  )
  environ_df <- list.files(path = climDir, full.names=TRUE)[pos]%>%
                raster::stack(.)%>% raster::crop(x = ., extent(SDM) )%>%
                 raster::mask(x = ., mask = SDM)  %>%
                  raster::rasterToPoints(x=.) %>%
                   as.data.frame(.) %>%
                    dplyr::mutate(., ID = 1:nrow(.)) %>%
                      dplyr::select(., ncol(.) , 1:(ncol(.)-1) )

#  environ_list <- lapply(path, function(x){
#     raster(x) %>% raster::crop(x = ., extent(SDM)) %>% 
#      raster::mask(x = ., mask = SDM) %>%
#      raster::rasterToPoints(x = .) %>% 
#      as.data.frame(.) 
#  })
  
#  gc()
  
 # environ_df <- environ_list %>% reduce(inner_join, by = c("x", "y")) %>% 
 #   dplyr::mutate(., ID = 1:nrow(.)) %>%
 #   dplyr::select(., ncol(.) , 1:(ncol(.)-1) )
  

 # rm(pos, var_names, environ_list); g <- gc(); rm(g); removeTmpFiles()


rownames(environ_df) <- environ_df$ID

zeroVar <- caret::nearZeroVar(environ_df)
if(length(zeroVar) != 0){
  environ_df <- environ_df[, -zeroVar]
}

environ_df_in <- environ_df[,-c(1,2,3)]
rownames(environ_df_in) <- rownames(environ_df)
#environ <- environ[complete.cases(environ),]

#environ_scaled <- scale(environ_df_in, center = T, scale = T)

####
cat( paste("Starting clustering process whit: ", n.sample," Sample size \n   \n \n")  )

set.seed(100)
muestra <- sample(1:nrow(environ_df), n.sample)
df_temp <- environ_df[muestra,]
rownames(df_temp) <- rownames(environ_df[muestra, ])
df_temp_in <-  df_temp[, 4:ncol(df_temp)]

cat( "Calculating Mahalanobis distances... \n   \n \n")



mahaRed_dist <-  parallelDist::parDist(as.matrix(df_temp_in), method = "mahalanobis")

rm(df_temp_in); g <- gc(); rm(g)

cat("Hierarchical Clustering to distances using the WARD method \n   \n \n")

clust_hc <- fastcluster::hclust(mahaRed_dist, method = "ward.D") 

rm(mahaRed_dist); g <- gc(); rm(g)

memb <- cutree(clust_hc, k= k.clust)

rm(clust_hc); g <- gc(); rm(g)

environR_clust <- data.frame( df_temp, clust= factor(memb) )

cat("Starting assignation of occurrences to each cluster using RF \n   \n \n")
cat("This can take several minutes \n   \n \n")

ctrol2 <-  trainControl(method = "LGOCV", p = 0.8, number = 5, savePredictions = T, verboseIter = TRUE )

set.seed(825)
cat("Fitting Random Forest model ...\n   \n \n")

tunegrid <- expand.grid(mtry = 8:10)
FDA <- train(clust ~ ., data = environR_clust[, 4:ncol(environR_clust)],  method = 'rf', ntree = 1000, tuneGrid = tunegrid, trControl = ctrol2)
cat("finishing Random Forest ...\n   \n \n")

cat("Classifying the rest of occurrences \n \n \n")
to_assign <- environ_df[-muestra, ]
rownames(to_assign) <- rownames(environ_df[-muestra,])

to_assign$clust <-  predict(FDA, newdata = to_assign[,4:ncol(to_assign)]  )
cat("converting predictions to a factor \n")
to_assign$clust <- as.factor(to_assign$clust)

environR_clust$clust <- as.factor(environR_clust$clust)
  
cat("Binding both dataframes \n \n \n")
environ_clust <- dplyr::bind_rows(environR_clust, to_assign)

cat( "Starting rasterization and saving process \n   \n  \n")


 SPF <- sp::SpatialPointsDataFrame( coords = environ_clust[,2:3], data = data.frame( cluster = environ_clust[,ncol(environ_clust)] ), proj4string = crs(SDM) )

r <- raster()
extent(r) <- extent(SDM)
crs(r) <- crs(SDM)
res(r) <- res(SDM)

to_rasterize <- raster::rasterize( x = SPF, y = r, field = as.numeric(SPF$cluster)  )
#raster mahalanobis
writeRaster(to_rasterize,filename= paste0( gap_outDir, "/ecogeo_hclust_mahalanobis.tif") , format="GTiff", overwrite = T )
 gc()
cat(paste("Process Done... Pls check the Path:", gap_outDir,"\n   \n \n"))

return(to_rasterize)


}# ENDCLUSTER FUNCTION

 


