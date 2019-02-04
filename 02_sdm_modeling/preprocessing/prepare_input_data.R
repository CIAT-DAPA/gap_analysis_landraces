####################
# function to prepare the main input datafile and do predictions if it is needed
# authors: Andres camilo mendez, Harold achicanoy
###################


prepare_input_data <- function(data_path = choose.files( caption = "Select a valid .csv file"), 
                               col_number = NULL, 
                               do.ensemble.models = TRUE,
                               add.latitude = FALSE,
                               add.longitude = FALSE,
                               do.predictions = FALSE,
                               sampling_mthd = "none",
                               mask = mask){
  
  msk <- raster(mask)
  data <- read.csv(data_path, header = T)
  cat("Preview Data base \n")
  print(head(data))
  
  #in case of col_number empty request a value from the user
  if(is.null(col_number)){
    warning("please enter the column number of response variable", immediate. = TRUE, noBreaks. = T)
    col_number <- readline(prompt="Enter an integer: ") 
  }
  #check if the number enter by de user has the correct format(only a number)
  if(!grepl("^[0-9]+$", col_number)){
    warning("Only numbers are allowed", immediate. = TRUE, noBreaks. = T)
    col_number <- readline(prompt="Enter an integer: ")
  }
  
  if(grepl("status", names(data))){
    status <- data %>% dplyr::select(., matches("status", ignore.case = T))  
  }else(status <- NULL)
   
  #select only the response variable and lat / long
  data <- data %>% dplyr::select(., as.integer(col_number), 
                             matches("declat|latitude", ignore.case = T), 
                             matches("declon|longitude", ignore.case = T) ) 
  if(ncol(data) > 3){
    stop("Data base has more than 1 lat/long variable")
  }
  
  if(ncol(data) <= 2){
    stop("Data base doesn't have one of lat/long variable")
  }
  #change names
  names(data) <- c("Y", "Latitude", "Longitude")
  
  cat("Cleaning zero lat/lon \n")
  
  data <- data %>% dplyr::filter(., Latitude != 0 | Longitude != 0)
  
  cat("Removing coordinates on Oceans/Seas \n")
  data <- data[which(!is.na(raster::extract(x = msk, y = data[,c("Longitude", "Latitude")]))),]
  
  cat("Removing duplicated coordinates \n")
  rep <- which(duplicated( raster::extract(msk, data[, c("Longitude", "Latitude")], cellnumbers = TRUE)  ))
  data  <- data[-rep, ]
  
  rows_id <- row.names(data)
  #loading all input rasters
  generic_rasts <- list.files(paste0(baseDir, "/input_data/generic_rasters/", region), pattern = ".tif$", full.names = TRUE)
  sp_rasts      <- list.files(paste0(baseDir, "/input_data/by_crop/", crop, "/raster/", region), pattern = ".tif$", full.names = TRUE)
  
  #climate extraction
  current_clim_layer_generic <- lapply(generic_rasts, raster)
  current_clim_layer_sp      <- lapply(sp_rasts, raster)
  current_clim_layer         <- stack(c(current_clim_layer_generic, current_clim_layer_sp))
  
  sp_data <- SpatialPoints(data[, c("Longitude", "Latitude")], crs(raster(msk)))
  clim_table <- raster::extract(current_clim_layer, sp_data)
  
  full_data <- data.frame(data, clim_table) 
  full_data <- full_data[complete.cases(full_data[, c(-1, -2, -3)]), ]
  full_data <- droplevels(full_data)
  
  if(do.ensemble.models){
    cat("fitting an ensemble model using selected varaibles \n")
    to_train   <- full_data[which(!is.na(full_data$Y)),]
    
    
    #control whether to use latitude or longitude to models training 
        if(!add.latitude){
          to_train <- to_train %>% dplyr::select(., -Latitude)

      }else if(!add.longitude){
        to_train <- to_train %>% dplyr::select(., -Longitude)
        }
    
    if(do.predictions){
      
      #select occurrences to predict
      to_predict <- full_data[which(is.na(full_data$Y)), names(to_train)]
      
      #execute classification function predicting occurrences
      clas_res <- classification_fun(df = to_train,
                                     standardize_all = T,
                                     sampling_mthd = sampling_mthd,
                                     omit_correlated = T,
                                     top_variables = 5,
                                     external_df = to_predict)
      
      #fill NA cells using ensemble model predictions
      full_data[which(is.na(full_data$Y)), "Y"] <- clas_res$External_data_predictions %>% dplyr::select(., ensemble)
      
      #add status column if it exists
      if(!is.null(status)){
        full_data$status <- status[rows_id]
      }
      
      #saving results
      saveRDS(clas_res, file = paste0(classResults, "/", crop, "_descriptive_results.rds") )
      saveRDS(clas_res, file = paste0(input_data_aux_dir, "/", crop, "_descriptive_results.rds") )
      
      write.csv(full_data, paste0(classResults, "/", crop, "_", level, "_bd.csv"))
      write.csv(full_data, paste0(input_data_aux_dir, "/", crop, "_", level, "_bd.csv"))
      
    }else{
      #execute classification function without predict occurrences
      
      clas_res <- classification_fun(df = to_train,
                                     standardize_all = T,
                                     sampling_mthd = sampling_mthd,
                                     omit_correlated = T,
                                     top_variables = 5,
                                     external_df = NULL)
      #saving results
      saveRDS(clas_res, file = paste0(classResults, "/", crop, "_descriptive_results.rds") )
      saveRDS(clas_res, file = paste0(input_data_aux_dir, "/", crop, "_descriptive_results.rds") )
      
      
    }
    
    
  }else{
    
    if(!is.null(status)){
      full_data$status <- status[rows_id]
    }
    
    write.csv(full_data, paste0(classResults, "/", crop, "_", level, "_bd.csv"))
    write.csv(full_data, paste0(input_data_aux_dir, "/", crop, "_", level, "_bd.csv"))
    
  }
 
  
  
}#end fucntion