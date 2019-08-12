# Maxent implementation using maxnet R package
# Andres Camilo Mendez and H. Achicanoy
# CIAT, 2018

# Old function
# cat("Running modeling approach\n")
# m2 <- sdm_approach_function(occName      = occName,
#                             spData       = spData,
#                             model_outDir = sp_Dir,
#                             var_names    = var_names,
#                             nCores       = 5,
#                             nFolds       = 5,
#                             beta         = beta,
#                             feat         = feat)

# What is missing
# 1. Create replicates manually: k-fold cross-validation ** done
# 1.1 Select independently k training points (presences, pseudo-absences) ** done
# 1.2 Fit the model using previous subsets ** done
# 2. Paralellization of the process (if necessary) ** not necessary
# 3. Validation metrics * done
#4. project the model to a raster ** done

sdm_maxnet_approach_function <- function(occName      = occName,
                                  spData       = spData,
                                  var_names    = var_names,
                                  model_outDir = model_outDir,
                                  sp_Dir        = spDir,
                                  nFolds       = 5,
                                  beta         = beta,
                                  feat         = feat,
                                  doSDraster   = TRUE,
                                  varImp       = FALSE,
                                  validation   = FALSE){
  
  
  cat("Loading environmental raster files \n")
  clim_vars     <- paste0(var_names, ".tif") %in% list.files(climDir, pattern = ".tif$") 
  generic_vars  <- paste0(var_names, ".tif") %in% list.files(clim_spReg, pattern = ".tif$")
  clim_layer    <- lapply(paste0(climDir, "/", var_names[clim_vars], ".tif"), raster)
  if(any(generic_vars)){
    generic_layer <- lapply(paste0(clim_spReg,"/", var_names[generic_vars],".tif"), raster)
    clim_layer  <- raster::stack(c(clim_layer, generic_layer))
  }else{
    clim_layer  <- raster::stack(clim_layer)
  }
 
  
  cat("Initializing MAXNET model fitting throught cross validation. \n ")
  
  #split data  in nFolds for cross - validation process
  cvfolds <- modelr::crossv_kfold(spData, k= nFolds)
  cat("Number of folds:", nFolds, "\n")
  #Do all sdm process
  sdm_results <- cvfolds %>% dplyr::mutate(.
                                            #train 5 sdm models using Maxnet and train data
                                            ,model_train = purrr::map2(.x = train, .y = .id, function(.x, .y){
                                              
                                              cat("Training MAXNET model for fold", .y, ", all presence points added to background \n")
                                              
                                              data_train <- as.data.frame(.x) 
                                              
                                              #adding all presence points to background
                                              pres_to_bg <- data_train[which(data_train[, 1] == 1), ]
                                              pres_to_bg[,1] <- rep(0, length(pres_to_bg [, 1]))
                                              
                                              p <- c(data_train[, 1], pres_to_bg[,1])#adding all presence points to background
                                              data <- rbind(data_train[, -c(1,2,3)], pres_to_bg[, -c(1,2,3)])#adding all presence points to background
                                              
                                              fit.maxent <- maxnet::maxnet(p       = p,
                                                                           data    = data,
                                                                           regmult = beta,
                                                                           f       = maxnet.formula(p, data, classes = paste0(substr(feat, start = 1, stop = 1), collapse = "")))
                                              
                                              return(fit.maxent)
                                              
                                            })
                                            
                                            #evaluate trained model
                                            , predictions_train = purrr::pmap(list(.x = model_train, .y = .id, .z = train), function(.x, .y, .z){
                                              cat("Predicting train data for fold", .y, "\n")
                                              train <- as.data.frame(.z)
                                              predictions <- raster::predict(object = .x, newdata = train[, -1], type = "cloglog")
                                              dt <-  data.frame(obs = factor(train[, 1]), pred = predictions)
                                              
                                              return(dt)
                                            })
                                            #calculate auc for trained model
                                            ,AUC_train = purrr::map2(.x = predictions_train, .y = .id, function(.x, .y){
                                              cat("Calculating AUC_train for model", .y,"\n")
                                              croc <- pROC::roc(response = .x$obs, predictor = .x$pred)
                                              
                                              return(as.numeric(croc$auc))
                                            } ) 
                                            #calculate max preformance measures (sensitivity, specificity and Treshold) using train data
                                            ,evaluation_train = purrr::map2(.x = predictions_train, .y = .id, function(.x, .y){
                                              
                                              cat("Calculating optimal threshold for model", .y, "\n")
                                              croc <- pROC::roc(response = .x$obs, predictor = .x$pred)
                                              croc_summ <- data.frame (sensi = croc$sensitivities, speci = croc$specificities, threshold =  croc$thresholds) %>% 
                                                round(., 3) %>% 
                                                dplyr::mutate(., max.TSS = sensi + speci - 1) %>% 
                                                dplyr::mutate(., minROCdist = sqrt((1- sensi)^2 + (speci -1)^2))
                                              
                                              max.tss <- croc_summ %>% dplyr::filter(., max.TSS == max(max.TSS)) %>% 
                                                dplyr::mutate(., method = rep("max(TSS)", nrow(.)))
                                              
                                              minRoc <- croc_summ %>% 
                                                dplyr::filter(., minROCdist == min(minROCdist))%>% 
                                                dplyr::mutate(., method = rep("minROCdist", nrow(.)))
                                              
                                              croc_summ <- rbind(max.tss, minRoc) %>% 
                                                dplyr::filter(., speci == max(speci))  %>% 
                                                dplyr::sample_n(., 1)
                                              
                                              return(croc_summ)
                                            })
                                            #Make predictions using testing data
                                            , predictions_test = purrr::pmap(list(.x = test, .y = model_train, .z = .id), function(.x, .y, .z){
                                              
                                              cat("Using test data to predict model", .z," \n")
                                              test <- as.data.frame(.x)
                                              predictions <- raster::predict(object = .y,newdata = test[, -1],type = "cloglog")
                                              dt <-  data.frame(obs = factor(test[, 1]), pred = predictions)
                                              
                                              return(dt )
                                            })
                                            #Calculate AUC for testing 
                                            , AUC = map2(.x = predictions_test, .y = .id, function(.x, .y){
                                              cat("Calculating AUC for model", .y,"\n")
                                              croc <- pROC::roc(response = .x$obs, predictor = .x$pred)
                                              
                                              return(as.numeric(croc$auc))
                                            } ) 
                                            #calculate max preformance measures (sensitivity, specificity and Treshold) using max(TSS) criterion
                                            , evaluation_test = pmap(list(.x = evaluation_train, .y = .id, .z = predictions_test), function(.x, .y, .z){
                                              
                                              thr <- .x$threshold
                                              
                                              a <- .z %>% dplyr::filter(., pred >= thr & obs == 1) %>% nrow()
                                              b <- .z %>% dplyr::filter(., pred >= thr & obs == 0) %>% nrow()
                                              c <- .z %>% dplyr::filter(., pred < thr & obs == 1) %>% nrow()
                                              d <- .z %>% dplyr::filter(., pred < thr & obs == 0) %>% nrow()
                                              
                                              #senitivity and specificity
                                              se <- a/(a+c)
                                              es <- d/(b+d)
                                              #Matthews correlation coefficient
                                              den <- sqrt(a+b)*sqrt(a+c)*sqrt(d+b)*sqrt(d+c)
                                              den <- ifelse(den  != 0 ,den, 1 )
                                              mcc <- (a*d - b*c)/den
                                              #Likelyhood Ratio +
                                              lr_ps <- se/(1 - es)
                                              #Likelihood ratio -
                                              lr_ne <- (1 - se)/es
                                              
                                              #calculate kappa index
                                              pr_a <- (a+d)/(a+b+c+d)
                                              pr_e <- (((a+b)/(a+b+c+d))* ((a+c)/(a+b+c+d))) + ( ((c+d)/(a+b+c+d) )* ((b+d)/(a+b+c+d) )) 
                                                       kappa <- (pr_a - pr_e)/(1 - pr_e) 
                                                       
                                                       
                                                       evaluation <- data.frame(threshold= thr, sensi = se, speci = es, matthews.cor = mcc, LR_pos = lr_ps, LR_neg = lr_ne, kappa_index = kappa)
                                                       
                                                       return(evaluation)
                                                       # cat("Calculating optimal threshold for model", .y, "\n")
                                                       # croc <- pROC::roc(response = .x$obs, predictor = .x$pred)
                                                       # croc_summ <- data.frame (sensi = croc$sensitivities, speci = croc$specificities, threshold =  croc$thresholds) %>% 
                                                       #   round(., 3) %>% 
                                                       #   dplyr::mutate(., max.TSS = sensi + speci - 1) %>% 
                                                       #   dplyr::mutate(., minROCdist = sqrt((1- sensi)^2 + (speci -1)^2))
                                                       # 
                                                       # max.tss <- croc_summ %>% dplyr::filter(., max.TSS == max(max.TSS)) %>% 
                                                       #   dplyr::mutate(., method = rep("max(TSS)", nrow(.)))
                                                       # 
                                                       # minRoc <- croc_summ %>% 
                                                       #   dplyr::filter(., minROCdist == min(minROCdist))%>% 
                                                       #   dplyr::mutate(., method = rep("minROCdist", nrow(.)))
                                                       # 
                                                       # croc_summ <- rbind(max.tss, minRoc) %>% 
                                                       #   dplyr::filter(., speci == max(speci))  %>% 
                                                       #   dplyr::sample_n(., 1)
                                                       # 
                                                       # return(croc_summ)
                                            })
                                              #Calculate nAUC using both train and test data
                                              , nAUC = pmap(list(.x = train, .y = test, .z = .id), function(.x, .y, .z){
                                                cat("calculating AUC from NULL model", .z,"\n")
                                                train_dt <- as.data.frame(.x) %>% dplyr::select(., occName, starts_with("lon"), starts_with("lat")  )
                                                test_dt  <- as.data.frame(.y) %>% dplyr::select(., occName, starts_with("lon"), starts_with("lat")  )
                                                
                                                train_p <- train_dt[which(train_dt[, 1] == 1) , 2:3]
                                                train_a <- train_dt[which(train_dt[, 1] == 0) , 2:3]
                                                
                                                gd <- dismo::geoDist(p = train_p, a = train_a, lonlat=TRUE)
                                                pred <- predict(gd, test_dt[, 2:3])
                                                
                                                nAUC <- pROC::roc(response = test_dt[, 1], predictor = pred)
                                                return(as.numeric(nAUC$auc))
                                              }) 
                                              #Calculate cAUC using the formula cAUC = AUC + 0.5 - max( 0.5, nAUC)
                                              , cAUC = purrr::pmap(list(.x = AUC, .y = nAUC, .z = .id), function(.x, .y, .z){
                                                cat("Calculating AUC correction using NULL model", .z, " \n")
                                                cAUC = .x + 0.5 - max( 0.5, .y)
                                                return(cAUC)
                                              })
                                              #Project rasters using maxnet model for mean, median and sd
                                              , do.projections =  purrr::pmap(list(.x = model_train, .y = .id, .z = evaluation_train) ,function(.x, .y, .z){
                                                
                                                cat(">>> Proyecting MAXNET model", .y,"to a raster object \n")
                                                r <- raster::predict(clim_layer, .x, type = "cloglog", progress='text')
                                                writeRaster(r, paste0(model_outDir,"/replicates/",occName,"_prj_rep-", .y,".tif"), format="GTiff", overwrite = TRUE)
                                                #thresholding raster 
                                                if(!validation){
                                                  r[which(r[] < .z$threshold)] <- NA 
                                                }
                                                writeRaster(r, paste(model_outDir,"/replicates/",occName,"_prj_th_rep-", .y,".tif",sep=""), format="GTiff", overwrite = TRUE)
                                                return(r)
                                              })
                                              
                                              
                                              
        )#end mutate
  
  
  

#calculate  mean, median and sd raster from replicates 
  prj_stk <- sdm_results %>% dplyr::select(., do.projections) %>% unlist() %>% raster::stack() 
cat("Calculating mean, median and sd for replicates \n")
mean(prj_stk) %>% writeRaster(., paste0(model_outDir,"/", occName, "_prj_mean.tif" ), overwrite = TRUE)
cat("Mean raster calculated \n")
raster::calc(prj_stk, fun = function(x) {median(x)}) %>% writeRaster(., paste0(model_outDir,"/", occName, "_prj_median.tif" ), overwrite = TRUE)
cat("Median raster calculated \n")
if(doSDraster){
  raster::calc(prj_stk, fun = function(x) {sd(x)}) %>% writeRaster(., paste0(model_outDir,"/", occName, "_prj_std.tif" ), overwrite = TRUE)
  cat("Sd raster calculated \n")
}


######## calculate varImportance ########

if(varImp){

  sdm_results <- sdm_results %>% dplyr::mutate(., varImp = purrr::pmap(list(.x = train, .y = .id, .z = model_train), function(.x, .y, .z){
    cat("Calculating var importance for model", .y, "... \n")
    
    data_train <- as.data.frame(.x) 
    
    pres_to_bg <- data_train[which(data_train[, 1] == 1), ]
    pres_to_bg[,1] <- rep(0, length(pres_to_bg [, 1]))
    
    p <- c(data_train[, 1], pres_to_bg[,1])#adding all presence points to background
    data <- rbind(data_train[, -c(1,2,3)], pres_to_bg[, -c(1,2,3)])#adding all presence points to background
    
    pred_all_vars <-  data.frame(obs = p, pred = raster::predict(object = .z , newdata = data, type = "cloglog"))
    
    roc_all_vars <- pROC::roc(response = pred_all_vars$obs, predictor = pred_all_vars$pred)
    #lovout -> leave one variable out
    #calculate varImportance through randomization procedure as  the sdm package does (https://onlinelibrary.wiley.com/doi/epdf/10.1111/ecog.01881) page.373
    corTest <- c() 
    aucTest <- c()
    for(i in 1:length(var_names)){
      
    cors <- c()
    aucs <- c()
    for(k in 1:5){
      data_rand <- data 
      #randonmize var i 
      data_rand[, i] <- data_rand[(nrow(data_rand)), i]
      
      pred_rand <- data.frame(obs = p, pred = raster::predict(object = .z , newdata = data_rand, type = "cloglog"))
      
      #varImportance by correlation
      cor_eval <- cor(pred_all_vars$pred ,pred_rand$pred)
      if(cor_eval < 0  ) cor_eval <- 0
      cors[k] <- 1 - cor_eval
      
      #varImportance by AUC
      roc_rand <- pROC::roc(response = pred_rand$obs, predictor = pred_rand$pred)
      auc_rand <- (roc_all_vars$auc - roc_rand$auc)*2
      if (auc_rand > 1) auc_rand <- 1
      else if (auc_rand < 0) auc_rand <- 0
      aucs[k] <- auc_rand 
    }
    corTest[i] <- round(mean(cors, na.rm = TRUE), 4)
    aucTest[i] <- round(mean(aucs, na.rm = TRUE), 4)
  }
   
    varImp <- data.frame(vars = var_names, "corTest"= corTest, "aucTest" = aucTest)
    return(varImp)
  })
  )#end mutate

}
#save all results in an .rds file
cat("Process Done... Saving results as .rds file in the path", paste0(sp_Dir, "/sdm.rds"), " \n")
saveRDS(sdm_results, paste0(sp_Dir, "/sdm.rds"))
}#end sdm_maxnet function 
 

#######################################################
########### OTHER WAY TO RUN MAXNET ###################
######################################################


# sdm_results <- cvfolds %>% dplyr::mutate(.
#                                          #train 5 sdm models using Maxnet and train data
#                                          ,model_train = purrr::map2(.x = train, .y = .id, function(.x, .y){
#                                            
#                                            cat("Training MAXNET model for fold", .y, ", all presence points added to background \n")
#                                            
#                                            data_train <- as.data.frame(.x) 
#                                            
#                                            #adding all presence points to background
#                                            pres_to_bg <- data_train[which(data_train[, 1] == 1), ]
#                                            pres_to_bg[,1] <- rep(0, length(pres_to_bg [, 1]))
#                                            
#                                            p <- c(data_train[, 1], pres_to_bg[,1])#adding all presence points to background
#                                            data <- rbind(data_train[, -c(1,2,3)], pres_to_bg[, -c(1,2,3)])#adding all presence points to background
#                                            
#                                            fit.maxent <- maxnet::maxnet(p       = p,
#                                                                         data    = data,
#                                                                         regmult = beta,
#                                                                         f       = maxnet.formula(p, data, classes = paste0(substr(feat, start = 1, stop = 1), collapse = "")))
#                                            
#                                            return(fit.maxent)
#                                            
#                                          })
#                                          #Make predictions using testing data
#                                          , predictions_test = purrr::pmap(list(.x = test, .y = model_train, .z = .id), function(.x, .y, .z){
#                                            
#                                            cat("Using test data to evaluate model", .z," \n")
#                                            test <- as.data.frame(.x)
#                                            predictions <- raster::predict(object = .y,newdata = test[, -1],type = "cloglog")
#                                            dt <-  data.frame(obs = factor(test[, 1]), pred = predictions)
#                                            
#                                            return(dt )
#                                          })
#                                          #Calculate AUC for testing 
#                                          , AUC = map2(.x = predictions_test, .y = .id, function(.x, .y){
#                                            cat("Calculating AUC for model", .y,"\n")
#                                            croc <- pROC::roc(response = .x$obs, predictor = .x$pred)
#                                            
#                                            return(as.numeric(croc$auc))
#                                          } ) 
#                                          #calculate max preformance measures (sensitivity, specificity and Treshold) using max(TSS) criterion
#                                          , threshold_max_TSS = map2(.x = predictions_test, .y = .id, function(.x, .y){
#                                            
#                                            cat("Calculating optimal threshold for model", .y, "\n")
#                                            croc <- pROC::roc(response = .x$obs, predictor = .x$pred)
#                                            croc_summ <- data.frame (sensi = croc$sensitivities, speci = croc$specificities, threshold =  croc$thresholds) %>% 
#                                              round(., 3) %>% 
#                                              dplyr::mutate(., max.TSS = sensi + speci - 1) %>% 
#                                              dplyr::mutate(., minROCdist = sqrt((1- sensi)^2 + (speci -1)^2))
#                                            
#                                            max.tss <- croc_summ %>% dplyr::filter(., max.TSS == max(max.TSS)) %>% 
#                                              dplyr::mutate(., method = rep("max(TSS)", nrow(.)))
#                                            
#                                            minRoc <- croc_summ %>% 
#                                              dplyr::filter(., minROCdist == min(minROCdist))%>% 
#                                              dplyr::mutate(., method = rep("minROCdist", nrow(.)))
#                                            
#                                            croc_summ <- rbind(max.tss, minRoc) %>% 
#                                              dplyr::filter(., speci == max(speci))  %>% 
#                                              dplyr::sample_n(., 1)
#                                            
#                                            return(croc_summ)
#                                          })
#                                          #Calculate nAUC using both train and test data
#                                          , nAUC = pmap(list(.x = train, .y = test, .z = .id), function(.x, .y, .z){
#                                            cat("calculating AUC from NULL model", .z,"\n")
#                                            train_dt <- as.data.frame(.x) %>% dplyr::select(., occName, starts_with("lon"), starts_with("lat")  )
#                                            test_dt  <- as.data.frame(.y) %>% dplyr::select(., occName, starts_with("lon"), starts_with("lat")  )
#                                            
#                                            train_p <- train_dt[which(train_dt[, 1] == 1) , 2:3]
#                                            train_a <- train_dt[which(train_dt[, 1] == 0) , 2:3]
#                                            
#                                            gd <- dismo::geoDist(p = train_p, a = train_a, lonlat=TRUE)
#                                            pred <- predict(gd, test_dt[, 2:3])
#                                            
#                                            nAUC <- pROC::roc(response = test_dt[, 1], predictor = pred)
#                                            return(as.numeric(nAUC$auc))
#                                          }) 
#                                          #Calculate cAUC using the formula cAUC = AUC + 0.5 - max( 0.5, nAUC)
#                                          , cAUC = purrr::pmap(list(.x = AUC, .y = nAUC, .z = .id), function(.x, .y, .z){
#                                            cat("Calculating AUC correction using NULL model", .z, " \n")
#                                            cAUC = .x + 0.5 - max( 0.5, .y)
#                                            return(cAUC)
#                                          })
#                                          # Project rasters using maxnet model for mean, median and sd
#                                          , do.projections =  purrr::pmap(list(.x = model_train, .y = .id, .z = threshold_max_TSS) ,function(.x, .y, .z){
#                                            
#                                            cat(">>> Proyecting MAXNET model", .y,"to a raster object \n")
#                                            r <- raster::predict(clim_layer, .x, type = "cloglog", progress='text')
#                                            writeRaster(r, paste0(model_outDir,"/replicates/",occName,"_prj_rep-", .y,".tif"), format="GTiff", overwrite = TRUE)
#                                            #thresholding raster
#                                            r[which(r[] < .z$threshold)] <- NA
#                                            writeRaster(r, paste(model_outDir,"/replicates/",occName,"_prj_th_rep-", .y,".tif",sep=""), format="GTiff", overwrite = TRUE)
#                                            return(r)
#                                          })
#                                          
#                                          
#                                          
# )









