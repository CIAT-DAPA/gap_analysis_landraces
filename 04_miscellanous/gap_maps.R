# Maria Victoria
# CIAT, April 2019

library(dplyr)

json_table<-function(crop){
  
  
  cat("writting the JSON file to map", "\n")
  
  
  db<-read.csv(paste0(baseDir,"/diversity_tree_results/",crop,"/",  "/gap_count.csv"))
  db<-db[complete.cases(db),]
  colnames(db)[3:(ncol(db)-1)]<- gsub("_Gaps", "", colnames(db)[3:(ncol(db)-1)])
  db$alternative_column<-NA
  
  
  db1<-as.data.frame(db[,3:(ncol(db)-2)])
  colnames(db1)<-colnames(db)[3:(ncol(db)-2)]
  
  
  
  
  for(i in 1:nrow(db)){
    
    condition<-which(db1[i,] != 0)
    
    if(length(condition) == 0){
      
      db$alternative_column[i]<- "No gaps"
      
      
    }else{
      
      db$alternative_column[i]<- paste(names(db1)[which(db1[i,] != 0)], collapse = " ; ")
      
    }
    
  }
  
  
  
  
  db<-db[,c(1,2, ncol(db)-1,ncol(db))]
  
  col_names_1 <- gsub(colnames(db)[1], paste0("var crop = [['", colnames(db)[1], "',"), colnames(db)[1])
  col_names_2 <- gsub(colnames(db)[2], paste0("'", colnames(db)[2], "_name',"), colnames(db)[2])
  col_names_3 <- gsub(colnames(db)[3], paste0("'",colnames(db)[3], "',"), colnames(db)[3])
  col_names_4 <- gsub(colnames(db)[4], paste0("{role: 'tooltip', p:{html:true}}],"), colnames(db)[4])
  
  
  
  col_names<- c(col_names_1, col_names_2, col_names_3, col_names_4); rm(col_names_1, col_names_2, col_names_3, col_names_4)
  
  
  
  
  x <- lapply(1:nrow(db), function(h){ 
    cat(h, "\n")
    
    a <-db[h,1] %>%  gsub(., paste0("['", as.character(.), "',"), .)
    
    b <- db[h,2] %>% gsub(., paste0("'",., "',"), . )
    
    c <- db[h,3] %>% gsub(., paste0(., ","), . )
    
    d <- db[h,4] %>% gsub(., paste0("'",., "'],"), . )
    
    if(h == nrow(db)){d <- db[h,4] %>% gsub(., paste0("'",., "']];"), . )}
    
    
    base <- data.frame(a, b,c,d )
    colnames(base)<-col_names
    
    return(base)
  })
  
  
  x<-do.call(rbind,x)
  x<-as.data.frame(x)
  rownames(x)<- seq(1:nrow(x))
  
  write.table(x, paste0(baseDir,"/diversity_tree_results/",crop,"/",crop,  ".js"), row.names = F, quote = F)
  gc()
  
  return(x)
}




gap_maps<-function(crop, occName , level ,  region, mask_dir, type){
  
  
  
  wmask <- raster::raster(paste0(mask_dir, "/mask_",region, ".tif" ))
  
  
  sdm_models_1<-lapply(1:length(occName), function(i){
    sdm_models<- raster(paste0(results_dir, "/", crop, "/", level, "/", occName[i], "/", region, "/prj_models/", occName[i],"_prj_median.tif"))
    sdm_models[which(sdm_models[] != 0)]<-1
    sdm_models[is.na(sdm_models[])]<-0
    
    return(sdm_models)
    
  })
  
  
  sdm_models<-stack(sdm_models_1)
  
  
  sdm_models<-sum(sdm_models)
  sdm_models<-crop(sdm_models, wmask)
  sdm_models<- mask(sdm_models, wmask)
  sdm_models[which(sdm_models[] == 0)]<-NA
  
  
  #sdm_models[which(sdm_models[] == 0 )]<-NA
  
  final_gap_rast<-lapply(1:length(occName), function(i){
    
    final_gap_rast1 <- raster(paste0(results_dir, "/", crop, "/", level, "/", occName[i], "/", region, "/gap_models/gap_class_final.tif"))
    final_gap_rast1[which(final_gap_rast1[] != 2)]<-0
    final_gap_rast1[which(final_gap_rast1[] == 2)]<-1
    final_gap_rast1[which(is.na(final_gap_rast1[]))] <- 0
    
    
    final_gap_rast1<-crop(final_gap_rast1, sdm_models)
    final_gap_rast1<-mask(final_gap_rast1, sdm_models)  
    
    return(final_gap_rast1)
    
  })
  
  final_gap_rast_1<-stack(final_gap_rast)
  final_gap_rast_1<-sum(final_gap_rast_1)
  
  
  writeRaster(sdm_models, paste0(results_dir,"/",crop,"/", level, "/model_final_", crop, ".tif"), overwrite = TRUE)
  
  writeRaster(final_gap_rast_1, paste0(results_dir,"/",crop,"/", level, "/gap_final_", crop,".tif"), overwrite = TRUE)
  
  gc()
  
  shp<-shapefile("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/shapefiles/shp/all_countries.shp"); global<-as.data.frame(shp)
  
  
  if(type == "count"){
    
    cat(paste0("Calculating the amount of gap in ", crop, " landraces"), "\n")
    
    
    shp<-crop(shp, wmask)
    
    nms <-shp@data$ISO2
    
    x<-lapply(1:length(occName), function(j){
      
      cat("         ", "\n")
      cat("         ", "\n")
      
      cat("Processing ", occName[j], " \n")
      cat("         ", "\n")
      cat("         ", "\n")
      
      count_crops <- lapply(1:length(nms), function(i){
        cat("Extracting values from: ", nms[i], " \n")
        
        shp_new <- shp[which(shp@data$ISO2 == nms[i]), ]
        
        extracted<-raster::extract(final_gap_rast[[j]], shp_new)
        
        if(is.null(extracted[[1]])){
          vals<- NA
          
        }else{
          
          vals <- max(unlist(extracted), na.rm =TRUE)
          
        }
        
        return(vals)
        
      })
      
      
      count_crops<-do.call(rbind, count_crops)
      count_crops[which(count_crops[,1] == "-Inf"),1]<-NA
      return(count_crops)
      
    })
    
    
    x1<-do.call(cbind, x); x1<-data.frame(x1)
    
    
    suma<-lapply(1:nrow(x1), function(k){
      
      
      suma<- ifelse(any(is.na(x1[k,])),x1[k,!is.na(x1[k,])],sum(x1[k,]))
      
      suma<-ifelse(is.null(suma[[1]]), NA, suma)
      
      
      return(suma)
    })
    
    suma<-do.call(rbind,suma )
    
    
    x1$crop_gaps<-NA
    x1$crop_gaps<-suma; rm(suma)
    
    
    x1<-data.frame(iso2 = shp@data$ISO2 , country_name = shp@data$NAME, x1 )
    colnames(x1)<- c("ISO2", "Country", paste0(occName), "Total_gaps")
    
    if(!file.exists(paste0(baseDir,"/diversity_tree_results/",crop))){dir.create(paste0(baseDir,"/diversity_tree_results/",crop))}
    
    x1<-x1[complete.cases(x1),]
    
    write.csv(x1, paste0(baseDir,"/diversity_tree_results/",crop,"/",  "/gap_count.csv"), row.names = F)
    
    gc()
    
    
    
  }else{
    
    
    cat(paste0("Calculating the amount of gap area  in ", crop, " landraces"), "\n")
    
    gaps<- read.csv(paste0(baseDir, "/diversity_tree_results/common_bean/gaps_common_bean.csv"))
    world_mask <- raster::raster(paste0(mask_dir, "/mask_world.tif"))
    
    
    x<-lapply(1:length(occName), function(j){
      
      cat("         ", "\n")
      cat("         ", "\n")
      
      cat("Processing ", occName[j], " \n")
      cat("         ", "\n")
      cat("         ", "\n")
      
      #area km de gap
      gap_harold <- final_gap_rast[[j]]
      gap_harold[which(gap_harold[] != 1)] <- NA
      rast_area <- raster::mask(raster::area(gap_harold), gap_harold)
      
      
      #writeRaster(rast_area, paste0(baseDir, "/diversity_tree_results/common_bean/",occName[j],"/gap_area.tif"))  
      
      
      gaps_occname<-gaps[ ,which(tolower(colnames(gaps)) %in% tolower(occName[j]))] %>% as.character()
      
      if(length(which(gaps_occname == "")) != 0){gaps_occname<-gaps_occname[-which(gaps_occname == "")]}else{gaps_occname<-gaps_occname}
      
      
      #area km de gap / area del pais
      
      if(occName[j] == "andean"){
        
        sh <-  raster::shapefile("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/diversity_tree_results/common_bean/andean/Gionvanini_andean.shp")
        
      }else{
        
        sh <-  raster::shapefile("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/diversity_tree_results/common_bean/mesoamerican/Gionvanini_mesoamerican.shp")
        
      }
      
      sh@data$NAME <- iconv(sh@data$NAME, "UTF-8", "latin1")
      
      
   a  <-   lapply(1:length(gaps_occname), function(h){
     
     
     cat(paste0("Calculating  ", gaps_occname[h], " gap area"), " \n")
     

        countries_h<-sh[which(sh@data$NAME %in% gaps_occname[h]), ]
        
        
        gap_area<-raster::mask(rast_area, countries_h)
        
        x<-rasterize(countries_h, world_mask)
        gap_area_country <- raster::mask(raster::area(x), x)
        
        g <-sum(gap_area[],na.rm = T)/sum(gap_area_country[],na.rm = T)
        
        #g<- gap_area/gap_area_country
        x<-sh[which(sh@data$NAME %in% gaps_occname[h]),]
        x@data$Gap_area<- g
        x@data$Gap_relative <- sum(gap_area[],na.rm = T)
        
        return(x)
        
      })
   
   
   library(sf)
   
ocs<-list()   
   
   for(i in 1:length(a)){
     
     ocs[[i]] <- sf::st_as_sf(a[[i]])


   }

meso_test<-do.call(rbind, ocs)
   
   
   cat("         ", "\n")
   cat("         ", "\n")
   
   cat(paste0("Writing  ", occName[j], " shapefile. Please check in: ", baseDir, "/diversity_tree_results/common_bean/",occName[j],"/gap_area.tif"), " \n")
   cat("         ", "\n")
   cat("         ", "\n")
   
   
   st_write(meso_test,  paste0(baseDir, "/diversity_tree_results/common_bean/",occName[j],"/gap_area.shp"))
   
      gc()

      
    })
    
    
  }
  
}


#### Test funcion ### 


crop<-c("banana", "sorghum", "potato", "barley", "maize", "african_maize", "wheat_durum", "wheat_bread", "rice_african", "common_bean")
region<- c("banana_custom", "sgh_custom", "americas", "world", "americas", "africa", "world", "world", "africa", "americas")


f<-lapply(1:length(crop), function(i){
  
  
  cat(paste0("Processing ", crop[i]), "\n")
  
  occnames<-read.csv("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/diversity_tree_results/crops_occname.csv")
  
  occName<-occnames[,which(colnames(occnames) %in% crop[i])]; occName<-occName %>% as.character();  if(length(which(occName == "" | is.na(occName))) != 0){occName<-occName[-which(occName == "" | is.na(occName))]}else{occName<-occName}
  
  a<-gap_maps(crop = crop[i], occName = occName, level = level, region = region[i], type = "count", mask_dir = mask_dir )
  
  b<-json_table(crop = crop[i])
})


gap_maps(crop = "common_bean", occName = c("mesoamerican"), level = "lvl_1", region = "americas",
         mask_dir =  "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask", type = "gap")
