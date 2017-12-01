# Explore and classify occurrence data 2
# A. Mendez & H. Achicanoy
# CIAT, 2017

# R options
options(warn = -1); options(scipen = 999); g <- gc(reset = T); rm(list = ls())

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
root     <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9", "Windows" = "//dapadfs/Workspace_cluster_9")

# Load packages
suppressMessages(if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}else{library(tidyverse)})
suppressMessages(if(!require(rgdal)){install.packages("rgdal");library(rgdal)}else{library(rgdal)})
suppressMessages(if(!require(sp)){install.packages("sp");library(sp)}else{library(sp)})
suppressMessages(if(!require(raster)){install.packages("raster");library(raster)}else{library(raster)})
suppressMessages(if(!require(ncdf4)){install.packages("ncdf4");library(ncdf4)}else{library(ncdf4)})
suppressMessages(if(!require(rasterVis)){install.packages("rasterVis");library(rasterVis)}else{library(rasterVis)})
suppressMessages(if(!require(htmlwidgets)){install.packages("htmlwidgets");library(htmlwidgets)}else{library(htmlwidgets)})
suppressMessages(if(!require(compiler)){install.packages("compiler");library(compiler)}else{library(compiler)})
suppressMessages(if(!require(leaflet)){install.packages("leaflet");library(leaflet)}else{library(leaflet)})
suppressMessages(if(!require(highcharter)){install.packages("highcharter");library(highcharter)}else{library(highcharter)})
suppressMessages(if(!require(plotly)){install.packages("plotly");library(plotly)}else{library(plotly)})
suppressMessages(if(!require(d3heatmap)){install.packages("d3heatmap");library(d3heatmap)}else{library(d3heatmap)})
suppressMessages(if(!require(cluster)){install.packages("cluster");library(cluster)}else{library(cluster)})
suppressMessages(if(!require(FactoMineR)){install.packages("FactoMineR");library(FactoMineR)}else{library(FactoMineR)})
suppressMessages(if(!require(factoextra)){install.packages("factoextra");library(factoextra)}else{library(factoextra)})
suppressMessages(if(!require(Rtsne)){install.packages("Rtsne");library(Rtsne)}else{library(Rtsne)})
suppressMessages(if(!require(InformationValue)){install.packages("InformationValue");library(InformationValue)}else{library(InformationValue)})
suppressMessages(if(!require(corrplot)){install.packages("corrplot");library(corrplot)}else{library(corrplot)})
suppressMessages(if(!require(caTools)){install.packages("caTools");library(caTools)}else{library(caTools)})
suppressMessages(if(!require(caret)){install.packages("caret");library(caret)}else{library(caret)})
suppressMessages(if(!require(shiny)){install.packages("shiny");library(shiny)}else{library(shiny)})
suppressMessages(if(!require(miniUI)){install.packages("miniUI");library(miniUI)}else{library(miniUI)})
suppressMessages(if(!require(assertthat)){install.packages("assertthat");library(assertthat)}else{library(assertthat)})
suppressMessages(if(!require(nnet)){install.packages("nnet");library(nnet)}else{library(nnet)})
suppressMessages(if(!require(earth)){install.packages("earth");library(earth)}else{library(earth)})

# Load data
genotypic_climate <- readRDS(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/BEAN-GRP-COORDINATES-CLIMATE-HUMAN-FACTORS.RDS"))
rownames(genotypic_climate) <- genotypic_climate$ID
#barplot(table(genotypic_climate$Genepool.predicted)/sum(table(genotypic_climate$Genepool.predicted)))
# Descriptive analysis
# source("descriptive_analysis4cleanedDB.R")

# Shiny app for selection of variables
ui <- miniPage(
  gadgetTitleBar("Variable Selector"),
  miniContentPanel(padding = 0,
                   checkboxGroupInput("vars", "Select Vars", choices = names(genotypic_climate ), selected = names(genotypic_climate))
  )
)
server <- function(input, output, session){
  
  observeEvent(input$done, {
    genotypic_climate <<- genotypic_climate[,input$vars]
    stopApp(genotypic_climate)
  })
  
}
runGadget(shinyApp(ui, server),viewer = dialogViewer("Select Vars", width = 600, height = 600))

#################################################
#################################################
#####----------Main function------------#########
#################################################
#################################################


genepool_predicted <- function(data_gen = genotypic_climate, y = c("Genepool.interpreted.ACID","Race.interpreted.ACID"), area = "Americas"){
  
  # ---------------------------------------------------------------- #
  # Train models
  # ---------------------------------------------------------------- #
  cat("\n>>>> Starting training process\n\n")
  
  if( length(grep("Color_", names(data_gen))) != 0 ){  
    data_gen[,grep("Color_", names(data_gen) )] <- data_gen[,grep("Color_", names(data_gen) )] %>% mutate_all(., funs(as.factor(.)))
  }
  if( length(grep("Protein_", names(data_gen) ) )!= 0 ){  
    data_gen[,grep("Protein_", names(data_gen) )] <- data_gen[,grep("Protein_",names(data_gen) )] %>% mutate_all(., funs(as.factor(.)))
  }
  
  # Function to exclude correlated variables before to model
  colinearity <- function(genepool_data,i=1){
    numeric <- genepool_data[,sapply(genepool_data, is.numeric)]
    descrCor <- cor(numeric)
    highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
    numeric <- numeric[,-highlyCorDescr]
    vec <- which(names(genepool_data) %in% names(numeric))
    genepool_data <- eval(parse(text = paste0("data.frame(", y[i], "=", "genepool_data$", y[i], ",", "numeric", ")")))
    return(genepool_data)
  }
  
  # Function to get just numeric variables before to model
  only_numeric <- function(genepool_data,i=1 ){
    genepool_data2 <- genepool_data[,sapply(genepool_data, is.numeric)]
    genepool_data <- eval(parse(text = paste0("data.frame(", y[i], "=", "genepool_data$", y[i], ",", "genepool_data2", ")")))
    return(genepool_data)
  }
  
  # Process response variable
  eval(parse(text = paste0("data_gen$", y[1], " <- as.character(data_gen$", y[1], ")")))
  eval(parse(text = paste0("data_gen$", y[1], "[which(data_gen$", y[1], " == 'N/A')] <- NA")))
  if(length(grep("Spain_Andean_I", eval(parse(text = paste0("data_gen$", y[1]))))) != 0){
    eval(parse(text = paste0("data_gen$", y[1], "[which(data_gen$", y[1], " =='Spain_Andean_I')] <- 'Andean'")))
  }
  eval(parse(text = paste0("data_gen$", y[1], " <- factor(data_gen$", y[1], ")"))) 
  
  # Apply filters
  row.names(data_gen) <- data_gen$ID
  genepool_data <- data_gen %>% 
    dplyr::filter(., Analysis == area & To.use.ACID == 1) %>% `rownames<-`(.$ID) %>%
    dplyr::select(., -ID, -Analysis, -To.use.ACID)
  
  # data_gen<- data_gen %>% dplyr::select(., -ID, -Analysis, -To.use.ACID)
  # Arrange and let just completed data for the training process
  
  genepool_data$ID <- rownames(genepool_data)
  genepool_data <- genepool_data[complete.cases(genepool_data),]
  rownames(genepool_data) <- genepool_data$ID; genepool_data$ID <- NULL
  
  # Select response variable
  if( y[1] == "Genepool.interpreted.ACID" ){
    if(assertthat::has_name( genepool_data, "Race.interpreted.ACID")){ genepool_data$Race.interpreted.ACID <- NULL}
    if(assertthat::has_name( genepool_data, "Subgroup.interpreted.ACID")){ genepool_data$Subgroup.interpreted.ACID <- NULL}
  }
  
  if(assertthat::has_name(genepool_data, "Genepool.protein")){ genepool_data$Genepool.protein[which(genepool_data$Genepool.protein == "N/A")] <- NA }
  genepool_data <- genepool_data[complete.cases(genepool_data),]
  if(assertthat::has_name(genepool_data, "Growth.habit")){genepool_data$Growth.habit <- factor(genepool_data$Growth.habit)}
  if(assertthat::has_name(genepool_data, "Seed.shape")){genepool_data$Seed.shape <- factor(genepool_data$Seed.shape)}
  if(assertthat::has_name(genepool_data, "Seed.brightness")){genepool_data$Seed.brightness <- factor(genepool_data$Seed.brightness)}
  if(assertthat::has_name(genepool_data, "Genepool.protein")){
    genepool_data$Genepool.protein <- as.character(genepool_data$Genepool.protein)
    genepool_data$Genepool.protein <- factor(genepool_data$Genepool.protein)
  }
  
  # Identify and exclude variables with low frequencies and variance close to 0
  nzv <- nearZeroVar(genepool_data)
  if(length(nzv)!=0){
    genepool_data <- genepool_data[,-nzv]
  }
  
  # Define parameters to train models
  set.seed(825); ctrol2 <- trainControl(method = "LGOCV", p = 0.8, number = 1, savePredictions = T)
  n <- nrow(genepool_data)*0.2
  cat(paste("Sample size of Testing genepool-data:",n,"\n"))
  rm(n)
  
  # In case of imbalance: ctrol2 <- trainControl(method = "LGOCV", p = 0.8, number = 1, savePredictions = T, sampling = "down")
  
  ##########################################
  # Model 1
  # Bagged Flexible Discriminant Analysis
  ##########################################
  cat("Running FDA ...\n")
  data_in <- only_numeric(genepool_data, i=1)
  eval(parse(text = paste0("FDA <- train(", y[1], " ~ ., data = data_in, method = 'bagFDA', trControl = ctrol2)"))) # FDA training
  cat("finishing FDA ...\n")
  
  ##########################################
  # Model 2
  # GLM: Logistic Regression Model
  ##########################################
  cat("Running GLM ...\n")
  vf <- colinearity(genepool_data,i=1)
  pos <- which(sapply(vf, is.factor))
  for(i in 1:length(pos)){
    vf[,pos[i]] <- make.names((vf[,pos[i]]))
  }
  eval(parse(text = paste0("glmFit1 <- train(", y[1], " ~ ., data = vf, method = 'glm', family = 'binomial', trControl = ctrol2)"))) # GLM training
  cat("finishing GLM ...\n")
  
  ##########################################
  # Model 3
  # Random Forest
  ##########################################
  cat("Running Random Forest ...\n")
  grid <- expand.grid(mtry = round(sqrt(ncol(genepool_data))))
  eval(parse(text = paste0("Rforest <- train(", y[1], " ~ ., data = genepool_data, method = 'rf', tuneGrid = grid, importance = TRUE, ntree = 2000, metric = 'Accuracy', trControl = ctrol2)"))) # RF training
  cat("finishing Rforest ...\n")
  
  ##########################################
  # Model 4
  # Support Vector Machines
  ##########################################
  cat("Running Support Vector Machine ...\n\n")
  eval(parse(text = paste0("svmFit <- train(", y[1], " ~ ., data = genepool_data, method = 'svmRadial', tuneLength = 9, trControl = ctrol2, importance = T)")))
  cat("finishing SVM ...\n")
  
  # ---------------------------------------------------------------- #
  # Save Variable importance
  # ---------------------------------------------------------------- #
  variableImportance <- caret::varImp(Rforest)$importance
  colnames(variableImportance) <- paste0(colnames(variableImportance), ".RF")
  variableImportance$Variable <- rownames(variableImportance)
  rownames(variableImportance) <- 1:nrow(variableImportance)
  variableImportance <- variableImportance[order(variableImportance$Andean, decreasing = T),]
  
  # variableImportance <- list(FDA = caret::varImp(FDA)$importance,
  #                            GLM = caret::varImp(glmFit1)$importance,
  #                            RF  = caret::varImp(Rforest)$importance
  #                            # SVM = caret::varImp(svmFit)$importance
  # )
  # variableImportance <- lapply(1:length(variableImportance), function(i){
  #   df <- variableImportance[[i]]
  #   colnames(df) <- paste0(colnames(df), ".", names(variableImportance)[i])
  #   df$Variable <- rownames(df)
  #   rownames(df) <- 1:nrow(df)
  #   return(df)
  # })
  # varsImportance <- Reduce(function(x, y) merge(x, y, by = "Variable"), variableImportance)
  # varsImportance$VariableImportance <- rowMeans(varsImportance[,-which(colnames(varsImportance)=="Variable")], na.rm = T)
  # varsImportance <- varsImportance %>% dplyr::select(Variable, VariableImportance)
  # varsImportance <- varsImportance %>% dplyr::arrange(., -VariableImportance)
  # saveRDS(object = varsImportance, file = paste0(root, "/gap_analysis_landraces/Results/classification_analysis/genepool_predictors.RDS"))
  
  # ---------------------------------------------------------------- #
  # Predict new cases
  # ---------------------------------------------------------------- #
  cat(">>>> Starting predicting process\n\n")
  
  data_gen <- data_gen %>% dplyr::filter(Analysis == "Americas")
  data_gen <- data_gen %>% dplyr::select(., -Analysis, -To.use.ACID)
  genepool_na <- data_gen # data_gen[!complete.cases(eval(parse(text = paste0("data_gen$", y[1])))),]
  rownames(genepool_na) <- genepool_na$ID; genepool_na$ID <- NULL
  if( y[1] == "Genepool.interpreted.ACID" ){
    if(assertthat::has_name( genepool_na, "Race.interpreted.ACID")){ genepool_na$Race.interpreted.ACID <- NULL}
    if(assertthat::has_name( genepool_na, "Subgroup.interpreted.ACID")){ genepool_na$Subgroup.interpreted.ACID <- NULL}
  }
  
 # genepool_na <- genepool_na[, names(genepool_data)]
 if(assertthat::has_name( genepool_na, "Genepool.protein")){genepool_na$Genepool.protein <- as.character(genepool_na$Genepool.protein)
   genepool_na$Genepool.protein[which(genepool_na$Genepool.protein == "N/A")] <- NA
  genepool_na$Genepool.protein <- factor(genepool_na$Genepool.protein) }
  
  if(assertthat::has_name( genepool_na, "Growth.habit")){genepool_na$Growth.habit[which(genepool_na$Growth.habit == "Climbing-Determinate")] <- NA
  genepool_na$Growth.habit <- factor(genepool_na$Growth.habit)  }
  
  genepool_na$ID <- rownames(genepool_na)
  genepool_na <- genepool_na[complete.cases(genepool_na[,-which(names(genepool_na) == y[1])]), ]
  rownames(genepool_na) <- genepool_na$ID; genepool_na$ID <- NULL
  
  model_type <- c("FDA", "glmFit1", "Rforest", "svmFit")
  
  predictions <- lapply(model_type, function(x){
    
    model <- eval(parse(text = x ))
    
    ifelse(model$method == "glm" | model$method == "rf", tp <- "response", tp <- "class")
    
    if(model$method == "rf"){ pred <- predict(model, newdata = genepool_na[,-which(names(genepool_na) == y[1])]) }
    if(model$method == "svmRadial"){ pred <- predict(model, newdata = genepool_na[,-which(names(genepool_na) == y[1])]) }
    if(model$method == "bagFDA"){ pred <- predict(model$finalModel, newdata = genepool_na[,names(data_in)] ,type = tp) }
    if(model$method == "glm"){
      
      vf_p <- genepool_na[,names(vf)[-which(names(vf) == y[1])]]
      
      pos <- which(sapply(vf_p, is.factor))
      if(length(pos)!=0){
      for(i in 1:length(pos)){
        vf_p[,pos[i]] <- make.names((vf_p[,pos[i]]))
                              }
      }
      g1 <- glm(factor(Genepool.interpreted.ACID) ~ ., data = vf, family = binomial(link = "logit"))
      
      pred <- predict(g1, newdata = na.omit(vf_p), type = "response")
      pred <- ifelse(pred < 0.5, "Andean", "Mesoamerican")
      pred <- as.factor(pred)
      
    }
    
    return(pred)
  })
  
  names(predictions) <- model_type
  
  accu.FDA <- mean(FDA$finalModel$oob[,1])
  
  accu.glmFit1 <- mean(apply(gd <- data.frame(glmFit1$resampledCM[,1:4]), 1, function(x){
    (x[1] + x[4]) /sum(x)
  }))
  accu.Rforest <- mean(apply(gd <- data.frame(Rforest$resampledCM[,1:4]), 1, function(x){
    (x[1] + x[4]) /sum(x)
  }))
  accu.svm <- mean(apply(gd <- data.frame(svmFit$resampledCM[,1:4]), 1, function(x){
    (x[1] + x[4]) /sum(x)
  }))
  
  accuracy <- c(accu.FDA, accu.glmFit1, accu.Rforest, accu.svm)
  names(accuracy) <- model_type
  
  #------------------------------------------------------ ^ -------------------------------
  #### predictions for  races
  #------------------------------------------------------ ^ -------------------------------
  if( !is.na( y[2])  ){
    
    cat("Starting predictions proccess to beans race...  \n")
    
    eval(parse(text = paste0("data_gen$", y[2], " <- as.character(data_gen$", y[2], ")")))
    eval(parse(text = paste0("data_gen$", y[2], "[which(data_gen$", y[2], " == 'N/A')] <- NA")))
    if(length(grep("Chile", eval(parse(text = paste0("data_gen$", y[2]))))) != 0){
      eval(parse(text = paste0("data_gen$", y[2], "[which(data_gen$", y[2], " =='Chile')] <- 'Peru'")))
    }
    eval(parse(text = paste0("data_gen$", y[2], " <- factor(data_gen$", y[2], ")"))) 
    
    genepool_data <- data_gen
    genepool_data <- genepool_data[ complete.cases(genepool_data) , ]
    
      if(assertthat::has_name(genepool_data, "Subgroup.interpreted.ACID")){genepool_data$Subgroup.interpreted.ACID <- NULL}
    
    if(assertthat::has_name(genepool_data, "Genepool.protein")){ genepool_data$Genepool.protein[which(genepool_data$Genepool.protein == "N/A")] <- NA }
    genepool_data <- genepool_data[complete.cases(genepool_data),]
    if(assertthat::has_name(genepool_data, "Growth.habit")){genepool_data$Growth.habit <- factor(genepool_data$Growth.habit)}
    if(assertthat::has_name(genepool_data, "Seed.shape")){genepool_data$Seed.shape <- factor(genepool_data$Seed.shape)}
    if(assertthat::has_name(genepool_data, "Seed.brightness")){genepool_data$Seed.brightness <- factor(genepool_data$Seed.brightness)}
    if(assertthat::has_name(genepool_data, "Genepool.protein")){
      genepool_data$Genepool.protein <- as.character(genepool_data$Genepool.protein)
      genepool_data$Genepool.protein <- factor(genepool_data$Genepool.protein)
    }
    
    # Identify and exclude variables with low frequencies and variance close to 0
    nzv <- nearZeroVar(genepool_data)
    genepool_data <- genepool_data[,-nzv]
    
    # Define parameters to train models
    set.seed(825); ctrol2 <- trainControl(method = "LGOCV", p = 0.8, number = 1, savePredictions = T)
    
    n<- nrow(genepool_data)*0.2
    cat(paste("Sample size of Testing race-data:",n,"\n"))
    rm(n)
    
    # In case of imbalance: ctrol2 <- trainControl(method = "LGOCV", p = 0.8, number = 1, savePredictions = T, sampling = "down")
    
    ##########################################
    # Model 1
    # Bagged Flexible Discriminant Analysis
    ##########################################
    cat("Running FDA ...\n")
    data_in <- only_numeric(genepool_data, i = 2)
    data_in <- eval(parse(text = paste0("data.frame(data_in,", y[1], "= genepool_data$", y[1], ")")))
    rownames(data_in) <- data_in$ID; data_in$ID <- NULL
    eval(parse(text = paste0("FDA.race <- train(", y[2], " ~ ., data = data_in, method = 'bagFDA', trControl = ctrol2)"))) # FDA training
    cat("finishing FDA ...\n")
    
    ##########################################
    # Model 2
    # MLM: Multinomial Regression Model
    ##########################################
    cat("Running multinom glm ...\n")
    vf <- colinearity(genepool_data, i = 2)
    vf <- eval( parse( text = paste0( "data.frame( vf,", y[1], " = genepool_data$", y[1],  ")") ) )
  
    vf$Race.interpreted.ACID <- relevel(genepool_data$Race.interpreted.ACID, ref = "Durango-Jalisco")
    rownames(vf) <- vf$ID; vf$ID <- NULL
    
    set.seed(1200)
    folds <- modelr::crossv_kfold(vf, k = 6)
    
    multi <- eval (parse(text = paste0("folds %>% mutate(., model = purrr::map(train, ~ nnet::multinom(", y[2], "~. , data = .)))")))
    multi <- multi %>% dplyr::mutate(., tested = purrr::map2(model,test, ~predict(.x, newdata = .y)))
    multi <- eval(parse(text = paste0("multi %>% dplyr::mutate(., cm = purrr::map2(test, tested, ~table(data.frame(.x)$", y[2], ", .y)))  %>% mutate(., accuracy = purrr::map(cm, function(x){sum(diag(x))/sum(x)}))")))
    
    # select the best model#
    multi.model <- multi[which(unlist(multi$accuracy) == max(unlist(multi$accuracy))), "model"]$model
    mean(unlist(multi$accuracy))
    
    # Variable importance
    mlm.varImp <- lapply(X = multi$model, FUN = caret::varImp)
    mlm.varImp <- do.call(cbind, mlm.varImp)
    colnames(mlm.varImp) <- paste0("fold_", 1:6)
    mlm.varImp$Overall <- apply(X = mlm.varImp, MARGIN = 1, FUN = function(x) median(x, na.rm = T))
    mlm.varImp <- mlm.varImp %>% select(Overall)
    
    ##########################################
    # Model 3
    # Random Forest
    ##########################################
    cat("Running Random Forest ...\n")
    grid <- expand.grid(mtry = round(sqrt(ncol(genepool_data))))
    rownames(genepool_data) <- genepool_data$ID; genepool_data$ID <- NULL
    eval(parse(text = paste0("Rforest.race <- train(", y[2], " ~ ., data = genepool_data, method = 'rf', tuneGrid = grid, importance = TRUE, ntree = 2000, metric = 'Accuracy', trControl = ctrol2)"))) # RF training
    cat("finishing Rforest ...\n")
    
    ##########################################
    # Model 4
    # Support Vector Machines
    ##########################################
    cat("Running Support Vector Machine ...\n\n")
    eval(parse(text = paste0("svmFit.race <- train(", y[2], " ~ ., data = genepool_data[,-which( names(genepool_data)==", 'y[1]' ,") ], method = 'svmRadial', tuneLength = 9, trControl = ctrol2, importance = T)")))
    cat("finishing SVM ...\n")
    
    # ---------------------------------------------------------------- #
    # Save Variable importance
    # ---------------------------------------------------------------- #
    variableImportance.race <- list(FDA = caret::varImp(FDA.race)$importance,
                                    MLM = mlm.varImp,
                                    RF  = caret::varImp(Rforest.race)$importance
                                    # SVM = caret::varImp(svmFit.race)$importance
    )
    
    cat(">>>> Starting predicting process for race...\n\n")
    
    # data_gen <- data_gen %>% dplyr::filter(Analysis == "Americas")
    # data_gen <- data_gen %>% dplyr::select(., -Analysis, -To.use.ACID)
    rownames(data_gen) <- data_gen$ID; data_gen$ID <- NULL
    genepool_na <- data_gen # data_gen[!complete.cases(eval(parse(text = paste0("data_gen$", y[1])))),]
    rownames(genepool_na) <- genepool_na$ID; genepool_na$ID <- NULL
    
    #genepool_na <- data_gen[!complete.cases(eval(parse(text = paste0("data_gen$", y[2])))),]
    
    df <- as.data.frame(data_gen[, y[2]])
    row.names(df) <- row.names(data_gen)
    genepool_na_race <- base::merge(df, data.frame(genepool_na, predictions$Rforest) , by = "row.names")
    row.names(genepool_na_race) <- genepool_na_race$Row.names
    genepool_na_race <-genepool_na_race %>% dplyr::select(., -Row.names, -Genepool.interpreted.ACID) 
    
    ##### VOY POR ACA
    names(genepool_na_race)[which(names(genepool_na_race) == "data_gen[, y[2]]")] <- y[2]
    names(genepool_na_race)[which(names(genepool_na_race) == "predictions.Rforest")] <- y[1]
    
    if(assertthat::has_name(genepool_na_race, "Subgroup.interpreted.ACID")){ genepool_na_race$Subgroup.interpreted.ACID <-NULL}
    if(assertthat::has_name(genepool_na_race, "Genepool.protein")){genepool_na_race$Genepool.protein <- as.character(genepool_na_race$Genepool.protein)
                                                              genepool_na_race$Genepool.protein[which(genepool_na_race$Genepool.protein == "N/A")] <- NA
                                                              genepool_na_race$Genepool.protein <- factor(genepool_na_race$Genepool.protein)
                                                              }
    
    if(assertthat::has_name(genepool_na_race,"Growth.habit") ) {genepool_na_race$Growth.habit[which(genepool_na_race$Growth.habit == "Climbing-Determinate")] <- NA
                                                           genepool_na_race$Growth.habit <- factor(genepool_na_race$Growth.habit)
    }
    
    genepool_na_race <- genepool_na_race[complete.cases(genepool_na_race[,-which(names(genepool_na_race) == y[2] & names(genepool_na_race) == y[1] )]),]#predicciones con el Rforest
    
    model_type <- c("FDA.race", "multi.model", "Rforest.race", "svmFit.race")
    
    predictions_race <- lapply(model_type, function(x){
      
      cat(paste("Predicting",x,"\n"))
      model <- eval(parse(text = x ))
      if(!is.null( model$method)) {
        
        if(model$method == "rf"){pred <- predict(model, newdata = genepool_na_race[,-which(names(genepool_na_race) == y[2])])}
        if(model$method == "svmRadial"){ pred <- predict(model, newdata = genepool_na_race[,-which(names(genepool_na_race) == y[2])])}
        if(model$method == "bagFDA"){ pred <- predict(model, newdata = genepool_na_race[, names(data_in)[-which(names(genepool_na_race) == y[2])]] ,type = "raw")}
        
      } else {
        
        vf_p <- genepool_na_race[,names(vf)[-which(names(vf) == y[2])]]
        pos <- which(sapply(vf_p, is.factor))
        if(length(pos)!=0){
          for(i in 1:length(pos)){
            vf_p[,pos[i]] <- make.names((vf_p[,pos[i]]))
          }
        }
        pred <- predict(model, newdata = na.omit(vf_p), type = "class")
        
      }
      
      return(pred)
    })
   
    names(predictions_race) <- model_type
    data_predicted_race <- data.frame(genepool_na_race, predictions_race)
    
    accu.FDA.race <- mean(FDA.race$finalModel$oob[,1])
    
    accu.multinom <- mean(unlist(multi$accuracy))
    n.lev <-length(eval(parse(text = paste0("levels(genepool_data$", y[2], ")"))))
    accu.Rforest.race <- sum(unlist(diag(matrix(Rforest.race$resampledCM[1:(n.lev*n.lev)], n.lev,n.lev,byrow = T))))/ sum(unlist(matrix(Rforest.race$resampledCM[1:(n.lev*n.lev)], n.lev, n.lev, byrow = T)))
    accu.svm.race <- sum(unlist(diag(matrix(svmFit.race$resampledCM[1:(n.lev*n.lev)],n.lev,n.lev,byrow = T))))/ sum(unlist(matrix(svmFit.race$resampledCM[1:(n.lev*n.lev)], n.lev, n.lev, byrow = T)))
    accuracy_race <- c(accu.FDA.race, accu.multinom, accu.Rforest.race, accu.svm.race)
    names(accuracy_race) <- model_type
    
  }
  
  
  cat("Saving data...\n")
 
  if( !is.null(y[1]) ){
    
      if(length(y)==2){
        cat(">>>> Process done\n")
        data_predicted_genepool <- data.frame(genepool_na, predictions)
        getmode <- function(v){
          uniqv <- unique(v)
          uniqv[which.max(tabulate(match(v, uniqv)))]
        }
        data_predicted_genepool$Genepool.prediction <- apply(X = data_predicted_genepool %>% select(FDA:svmFit), MARGIN = 1, FUN = getmode)
        data_predicted_genepool$ID <- rownames(data_predicted_genepool)
        data_predicted_genepool$ID <- as.integer(data_predicted_genepool$ID)
        genotypic_climate <- readRDS(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/BEAN-GRP-COORDINATES-CLIMATE-HUMAN-FACTORS.RDS"))
        genotypic_climate <- genotypic_climate %>% select(ID, Longitude, Latitude)
        data_predicted_genepool <- dplyr::inner_join(x = data_predicted_genepool, y = genotypic_climate, by = "ID")
        saveRDS(data_predicted_genepool, paste0(root, "/gap_analysis_landraces/Results/classification_analysis/genepool_predicted_ciat_original.RDS"))
        return(list(data_predicted_genepool = data.frame(genepool_na, predictions),
                    accuracy.genepool = accuracy,
                    variableImportance.genepool = variableImportance,
                    data_predicted_race= data.frame(genepool_na_race, predictions_race),
                    accuracy.race = accuracy_race,
                    # variableImportance.race = variableImportance.race,
                    data = data_gen))
        
      } else {
       
        return(list(data_predicted = data.frame(genepool_na, predictions),
                    models_accuracy = accuracy,
                    data = data_gen))
        
      }
   
  } else { 
    stop("ERROOOOOORRRRR")
    }
}


data_predicted_genepool$ID <- rownames(data_predicted_genepool)
data_predicted_genepool$ID <- as.numeric(data_predicted_genepool$ID)
genotypic_climate <- readRDS(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/BEAN-GRP-COORDINATES-CLIMATE-HUMAN-FACTORS.RDS"))
genotypic_climate <- genotypic_climate %>% select(ID, Longitude, Latitude)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
data_predicted_genepool$Genepool.prediction <- apply(X = data_predicted_genepool %>% select(FDA:svmFit), MARGIN = 1, FUN = getmode)
data_predicted_genepool <- dplyr::inner_join(x = data_predicted_genepool, y = genotypic_climate, by = "ID")
write.csv(x = data_predicted_genepool, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Results/classification_analysis/genepool_predicted.csv", row.names = F)
saveRDS(data_predicted_genepool, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Results/classification_analysis/genepool_predicted.RDS")

predictions <- genepool_predicted(data_gen = genotypic_climate, c("Genepool.interpreted.ACID","Race.interpreted.ACID"), area = "Americas")
df<-predictions[[2]];df
df2<-predictions[[4]];df2
predictions
saveRDS(predictions, "/home/hachicanoy/genepool_predictions.RDS")

data_gen$Genepool.predicted <- NA
data_gen$Genepool.predicted[match(rownames(predictions[[1]]), rownames(data_gen))] <- as.character(apply(X = predictions[[1]][,c("FDA", "glmFit1", "Rforest", "svmFit")], MARGIN = 1, function(x){Mode(x)}))

# Map example

shp_wld <- rgdal::readOGR(dsn = "/home/hachicanoy", layer = "all_countries")
proj4string(shp_wld) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
shp_wld$CONTINENT <- iconv(shp_wld$CONTINENT, from = "UTF-8", to = "latin1")
shp_wld <- shp_wld[shp_wld@data$CONTINENT == "North America" | shp_wld@data$CONTINENT == "South America",]

shp_wld <- fortify(shp_wld)

ggplot() + 
  geom_polygon(data = shp_wld, aes(long, lat, group = group)) +
  geom_point(data = predictions[[1]], aes(x = Longitude, y = Latitude, fill = svmFit, colour = svmFit)) +
  coord_cartesian(xlim = c(-180, 0)) + theme_bw()


###########################################################
###### MULTINOMIAL LOGISTIC REGRESSION  "nnet" package#####
###########################################################
install.packages("nnet")
library("nnet")

genepool_data <- data_gen


genepool_data <- genepool_data[complete.cases(genepool_data$Race.interpreted.ACID),]
genepool_data$Race.interpreted.lit<-factor(genepool_data$Race.interpreted.lit)
genepool_data$Race.interpreted.lit<-relevel(genepool_data$Race.interpreted.lit,ref="Durango-Jalisco")

set.seed(1200)
#genepool_data<- colinearity(genepool_data)
folds<-modelr::crossv_kfold(genepool_data,k=6)


multi<- folds %>% mutate(.,model=purrr::map(train, ~ nnet::multinom( Race.interpreted.lit~. , data=. ) ) )

multi<- multi %>%  dplyr::mutate(.,tested= purrr::map2(model,test, ~predict(.x,newdata=.y) )  )

multi<- multi %>% dplyr::mutate(., cm=purrr::map2(test,tested, ~table(data.frame(.x)$Race.interpreted.lit,.y)  )   )  %>% mutate(., accuracy=purrr::map(cm, function(x){ sum(diag(x))/sum(x)}  )  )

#select the best model#
multi.model<- multi[which(unlist(multi$accuracy)==max(unlist(multi$accuracy))),"model"]$model
##global accuracy
mean(unlist(multi$accuracy))



####mediante un FDA


genepool_data$Genepool.lit<- as.numeric(genepool_data$Genepool.lit)
#as.numeric(genepool_data$Race.interpreted.lit )


genepool_data2<-genepool_data[,sapply(genepool_data, is.numeric)]
genepool_data2<-data.frame(Race.interpreted.lit=genepool_data$Race.interpreted.lit,genepool_data2)
genepool_data2$Race.interpreted.lit<-factor(genepool_data2$Race.interpreted.lit)
genepool_data2<- genepool_data2[complete.cases(genepool_data2$Race.interpreted.lit),]

head(genepool_data2)


set.seed(825)
ctrol2<-trainControl(method="LGOCV",p=0.8,number=1,savePredictions = T)

FDA.race<-train(Race.interpreted.lit~.,data=genepool_data2,method="bagFDA",trControl = ctrol2)
mean(FDA.race$finalModel$oob[,1])



genepool_data$Genepool.lit<- factor(genepool_data$Genepool.lit)
vf<- only_numeric(genepool_data )
vf$Genepool.lit<-as.numeric(vf$Genepool.lit)
vf<-data.frame(Race.interpreted.lit=genepool_data$Race.interpreted.lit,vf)
genepool_data$Race.interpreted.lit<-factor(genepool_data$Race.interpreted.lit)
genepool_data$Genepool.lit<-factor(genepool_data$Genepool.lit)

gam<-caret::train( Race.interpreted.lit  ~.,data=vf , method="nnet", trcontrol=ctrol2,
                   seed = 1)
gam$results

hist(vf$bio_19)

hist(log(scale(vf$bio_19,center = T, scale = T)) )


######  REDUCCION DE DIMENSIONALIDAD ###########

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
root     <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9", "Windows" = "//dapadfs/Workspace_cluster_9")


suppressMessages(library(Rtsne))
library(dplyr)
suppressMessages(if(!require(corrplot)){install.packages("corrplot");library(corrplot)}else{library(corrplot)})


biophysicalVars <- readRDS(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/BEAN-GRP-COORDINATES-CLIMATE.RDS"))
names(biophysicalVars)
biophysicalVars<-biophysicalVars[,-1]

colinearity<-function(genepool_data,tol=0.75){
  
  #detectar varibles altamente correlacionadas y quitarlas de la base de datos
  numeric<-genepool_data %>% dplyr::select(.,bio_1:bio_19)
  descrCor<-cor(numeric)
  highlyCorDescr <- findCorrelation(descrCor, cutoff = tol)
  numeric  <- numeric[,-highlyCorDescr]
  genepool_data <- cbind(genepool_data %>% dplyr::select(1:(which(names(genepool_data)=="bio_1")-1)), numeric)
  
  return(genepool_data)
}
colinearity(biophysicalVars,tol=0.75)

M<-cor(colinearity(biophysicalVars,tol=0.75), use = "complete.obs")
corrplot(M)
hist(M)
biophysicalVars <-colinearity(biophysicalVars,tol=0.5)


bio_tsne1 <- Rtsne(biophysicalVars[complete.cases(biophysicalVars),] %>% unique, dims = 2, perplexity = 400, verbose = TRUE, max_iter = 2000,pca=TRUE)
bio_tsne2 <- Rtsne(biophysicalVars[complete.cases(biophysicalVars),] %>% unique, dims = 2, perplexity = 25, verbose = TRUE, max_iter = 2000,pca=TRUE)
bio_tsne3 <- Rtsne(biophysicalVars[complete.cases(biophysicalVars),] %>% unique, dims = 2, perplexity = 10, verbose = TRUE, max_iter = 2000,pca=TRUE)

par(mfrow=c(1,3))
plot(bio_tsne1$Y, pch = 20, main = "tsne for biophysical variables")
plot(bio_tsne2$Y, pch = 20, main = "tsne for biophysical variables")
plot(bio_tsne3$Y, pch = 20, main = "tsne for biophysical variables")


bio_tsne1$M
M<-cor(biophysicalVars, use = "complete.obs")
corrplot(M)
hist(M)




cancer<- read.table("C:/Users/ACMENDEZ/Desktop/cancer.txt",sep=",")

cancer.dat<-cancer[,-(1:2)]

can1<- Rtsne(cancer %>% unique, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000,pca=TRUE)
tsn<-as.data.frame(can1$Y)
tsn.clust<-tsn
cluster<-kmeans( scale( tsn ),4)
tsn.clust$kmeans<-factor(cluster$cluster)
clust.h<-stats::hclust(dist(scale(tsn) )  )
tsn.clust$hierar<-factor(cutree(clust.h,4) )
plot(clust.h)
ggplot(tsn.clust,aes_string(x="V1",y="V2",color="kmeans") )+geom_point(size=0.25)  + guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.direction = "horizontal", 
        legend.position = "bottom",
        legend.box = "horizontal") +
  scale_colour_brewer(palette = "Accent") 

###################

ciat <- gs_ls("Bean_landrace_name_table")
ciat <- gs_title("Bean_landrace_name_table")
ciat %>% gs_browse(ws = "Pvulgaris_CIATdb")
ciat <- ciat %>% gs_read(ws = "Pvulgaris_CIATdb")

names(ciat) <- c("ID", "Source", "Cleaned.by", "Accession.number", "Synonyms", "Common.names",
                 "Interpreted.name.csosa", "To.use.ACID", "Common.name.ACID",
                 "Genepool.ACID", "Genepool.literature.ACID","Race_interpreted_ACID",
                 "Race.literature.ACID", "Subgroup.interpreted.ACID", "Subgroup.literature.ACID",
                 "Reference.ACID", "TEST.vernacular", "Name.literature.vernacular",
                 "Genepool.literature.vernacular", "Race.interpreted.vernacular", "Race.literature.vernacular",
                 "Subgroup.literature.vernacular", "Reference.vernacular", "Genus", "Species", "Subspecies", "Variety",
                 "Biological.status", "Material.type", "CORE.collection", "Country", "Department", "County", "Place",
                 "Altitude", "Latitude", "Longitude", "Lat.geo", "Lon.geo", "Coord.status", "Collection.date", "Name",
                 "Name2", "Institution", "Country3", "Receipt.date", "Growth.habit", "Seed.color",
                 "Seed.shape", "Seed.brightness", "Seed.weight", "Protein", "Genepool.WEIGHT.fix",
                 "Genepool.protein", "Race.protein", "Responsible11")

ciat <- ciat %>% filter(Coord.status != "No coords") # 16038
ciat$Latitude[which(!is.na(ciat$Lat.geo) & is.na(ciat$Latitude))] <- ciat$Lat.geo[which(!is.na(ciat$Lat.geo) & is.na(ciat$Latitude))]
ciat$Longitude[which(!is.na(ciat$Lon.geo) & is.na(ciat$Longitude))] <- ciat$Lon.geo[which(!is.na(ciat$Lon.geo) & is.na(ciat$Longitude))]

# ------------------------------------ #
# Include altitude records from SRTM
# ------------------------------------ #

# Identify coordinates without altitude data
which(!is.na(ciat$Latitude) & is.na(ciat$Altitude)) %>% length
ciat %>% dplyr::filter(!is.na(Latitude) & is.na(Altitude)) %>% dplyr::select(Longitude, Latitude) %>% head


srtm <- raster::raster(paste0(OSysPath, "/data_cluster_4/observed/gridded_products/srtm/Altitude_30s/alt"))
srtm.vals <- raster::extract(x = srtm,
                             y = ciat %>% dplyr::filter(!is.na(Latitude) & is.na(Altitude)) %>% dplyr::select(Longitude, Latitude))

# Density plots before and after update altitude records
ciat %>% ggplot(aes(x = Altitude)) + geom_density() # Before
srtm.vals %>% data.frame %>% ggplot(aes(x = .)) + geom_density() # SRTM values

ciat$Altitude[which(!is.na(ciat$Latitude) & is.na(ciat$Altitude))] <- srtm.vals






rm(srtm.vals, srtm)

ciat <- ciat %>% filter(Altitude <= 3500)

biophysicalVars
ciat<-ciat[complete.cases(ciat$Genepool.literature.ACID),]

ciat <- ciat %>% filter(To.use.ACID == 1)
ciat <- ciat %>% dplyr::filter(!is.na(Longitude) & !is.na(Altitude) &
                                 !is.na(Growth.habit) & !is.na(Seed.color) &
                                 !is.na(Seed.shape) & !is.na(Seed.brightness) &
                                 !is.na(Seed.weight) & !is.na(Protein) &
                                 !is.na(Genepool.protein))


ciat.bio<-left_join(x=ciat,y=biophysicalVars,by="ID")
ciat.bio<-as.data.frame(ciat.bio)
if(all( (ciat.bio$Genepool.ACID == "Spain_Andeanean_I" )==FALSE )==FALSE ){
  ciat.bio$Genepool.ACID[which(ciat.bio$Genepool.ACID=="Spain_Andeanean_I")]<-"Andean"
  
}
ciat.bio$Genepool.ACID<-factor(ciat.bio$Genepool.ACID)
table(ciat.bio$Race.literature.ACID)
table(ciat$Race.literature.ACID)


ciat.bio <- ciat.bio %>% dplyr::select(.,ID,Race.interpreted.ACID,Seed.weight,Altitude,Latitude.x,Longitude.x,aridityIndexThornthwaite:bio_19)
#Cambiar dependiendo de la raza

ciat.bio<-ciat.bio[complete.cases(ciat.bio),]
row.names(ciat.bio)<-ciat.bio$ID

M<-cor(ciat.bio[,-1], use = "complete.obs")
corrplot(M)
hist(M)




#detectar varibles altamente correlacionadas y quitarlas de la base de datos
numeric<-ciat.bio[,sapply(ciat.bio,is.numeric)]
numeric<-numeric[,-1]
numeric<-na.omit(numeric)
descrCor<-cor(numeric)
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
numeric  <- numeric[,-highlyCorDescr]
row.names(numeric)<-row.names(ciat.bio)
numeric<-data.frame(Genepool.ACID=ciat.bio$Race.interpreted.ACID,numeric)#cambiar dependiendo de la raza

#-----RACE
levels(factor(numeric$Genepool.ACID))

numeric$Genepool.ACID[which(numeric$Genepool.ACID=="N/A")]<-NA
numeric<-na.omit(numeric)
numeric$Genepool.ACID<-factor(numeric$Genepool.ACID)

#--------end RACE
numeric<- numeric[,] %>% unique
bio_tsne3 <- Rtsne(numeric[,], dims = 2, perplexity =40, verbose = TRUE, max_iter = 1500,pca=TRUE)

plot(bio_tsne3$Y, pch = 20, main = "tsne for biophysical variables")

row.names(bio_tsne3$Y)<-row.names(numeric)
tsn.clust<-data.frame(bio_tsne3$Y,Genepool.ACID=numeric$Genepool.ACID)


ggplot(tsn.clust,aes_string(x="X1",y="X2",color="Genepool.ACID") )+geom_point(size=1.8)  + guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.direction = "horizontal", 
        legend.position = "bottom",
        legend.box = "horizontal") 
################ DBSCAN CLUSTERING

install.packages("dbscan")
install.packages("factoextra")
library(dbscan)
library(factoextra)

set.seed(123)
cl<- dbscan::dbscan(tsn.clust[,1:2],eps=1.9, MinPts=10 )
cl


fviz_cluster(cl, data = tsn.clust[,1:2], stand = FALSE,
             ellipse = F, show.clust.cent = F,
             geom = "point",palette = "jco", ggtheme = theme_classic())

###HIERARCHICAL CLUSTERING

tsn<-as.data.frame(bio_tsne3$Y)
tsn.clust<-tsn
cluster<-kmeans( scale( tsn ),4)
tsn.clust$kmeans<-factor(cluster$cluster)
clust.h<-stats::hclust(dist(scale(tsn) )  )
barplot(clust.h$height)
tsn.clust$hierar<-factor(cutree(clust.h,4) )
plot(clust.h)
ggplot(tsn.clust,aes_string(x="V1",y="V2",color="hierar") )+geom_point(size=1.85)  + guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.direction = "horizontal", 
        legend.position = "bottom",
        legend.box = "horizontal") +
  scale_colour_brewer(palette = "Accent") 


### RANDOM FOREST

ciat.tsne<- base::merge(bio_tsne3$Y,numeric,by="row.names" ,all.x=TRUE )
row.names(ciat.tsne)<- ciat.tsne$Row.names
ciat.tsne<-ciat.tsne[,-1]

set.seed(250)
trcont<- trainControl(method="LGOCV",p=0.8,number=1,savePredictions = T)
grid <- expand.grid(mtry = round((ncol(ciat.tsne)-4)/3))

##con TSNE
rforest_1<-train(Genepool.ACID~.,   data=ciat.tsne ,method="rf",tuneGrid=grid, importance=T, ntree=2000, metric="Accuracy", trControl= trcont)

accu.rforest_1<- mean(apply(gd<-data.frame(rforest_1$resampledCM[,1:4]),1,function(x){
  (x[1] + x[4]) /sum(x)
}) )

#### sin TSNE
rforest_2<-train(Genepool.ACID~.,   data=ciat.tsne[,-(1:2)] ,method="rf",tuneGrid=grid, importance=T, ntree=2000, metric="Accuracy", trControl= trcont)

accu.rforest_2<- mean(apply(gd<-data.frame(rforest_2$resampledCM[,1:4]),1,function(x){
  (x[1] + x[4]) /sum(x)
}) )

##### PRINCIPAL COMPONENTS ANALYSIS
pca<-PCA(ciat.bio[,-(1:3)],ncp=4)
View(pca$var$cos2)
df<-data.frame(pca$ind$coord[,1:2],ciat.bio$Race.interpreted.ACID)
plot(pca)
ggplot(df,aes_string(x="Dim.1",y="Dim.2",color="ciat.bio.Race.interpreted.ACID", shape="ciat.bio.Race.interpreted.ACID")) + geom_point(size=1.85)  + guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.direction = "horizontal", 
        legend.position = "bottom",
        legend.box = "horizontal") +
  scale_colour_brewer(palette = "Accent")

nrow(ciat.bio)
nrow(pca$ind$coord)

df<-data.frame(pca$ind$coord, ciat.bio$Genepool.ACID )

plot(density(df[which(df$ciat.bio.Genepool.ACID=="Mesoamerican"),2]),ylim=c(0,0.2))
lines(density(df[which(df$ciat.bio.Genepool.ACID=="Andean"),2]),col="red")


vect<-which( names(ciat.bio)%in%names(numeric) )


ciat.bio[,sapply(ciat.bio,is.character)]






####### ACP 2 #####
OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
root     <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9", "Windows" = "//dapadfs/Workspace_cluster_9")


# genep_1<- readRDS("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_datosAndres/acp/genepool_predictions.RDS")
# race_1<-readRDS("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_datosAndres/acp/predictions_race.RDS")
# beancordH<-readRDS("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_datosAndres/acp/BEAN-GRP-COORDINATES-HUMAN-FACTORS.RDS")
# beancordC<-readRDS("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_datosAndres/acp/BEAN-GRP-COORDINATES-CLIMATE.RDS")
dmodel<- readRDS("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_datosAndres/acp/data4modeling.RDS")

files<-dir(file.path("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/raster_sdm/2_5m"))
files<-files[grep("dist",files)]

for(i in 1:length(files )){
  
  eval( parse( text =  paste0('raster_',files[i],'<-raster(',"'", file.path(paste0( root,"//gap_analysis_landraces/Input_data/raster_sdm/2_5m","/",files[i],"'" ) ),')' )  ) )    
  
  eval( parse( text= paste0("dmodel <- data.frame( dmodel,", substr(files[i],1, nchar(files[i])-4 ) ,"= extract(", 'raster_',files[i], ", cbind(dmodel$Longitude,dmodel$Latitude) ,df=T)[,2] )"  )  ) ) 
  
}


dmodel<-dmodel[, -which(names(dmodel)== "Distance.to.GP1"  )]
dmodel<- dmodel[complete.cases(dmodel$Genepool.predicted),]
dmodel<- dmodel[complete.cases(dmodel$Race.predicted),]

dmodel<- dmodel %>% dplyr::select( ., Altitude:Longitude ,annualPET:dist_toGP1 )
dmodel<- na.omit(dmodel)
# all(is.na(dmodel.acp))==FALSE


dmodel.acp <- dmodel %>% dplyr::select( .,-Analysis ,-Genepool.predicted, -Race.predicted )  
M<-cor(dmodel.acp)
corrplot(M)

plot(dmodel.acp$aridityIndexThornthwaite,dmodel.acp$Physical.area)

highlyCorDescr <- findCorrelation(M, cutoff = .70)
names(dmodel.acp)[highlyCorDescr]
#dmodel.acp<- dmodel.acp [ , highlyCorDescr]


acp<- PCA( dmodel.acp , quanti.sup = which( names(dmodel.acp) %in% names(dmodel.acp)[-highlyCorDescr]  )  )


plot(acp)
names(dmodel)




df<-data.frame(acp$ind$coord[,1:2],gen.pred=dmodel$Genepool.predicted )
plot(acp)
ggplot(df,aes_string(x="Dim.1",y="Dim.2",color="gen.pred", shape="gen.pred")) + geom_point(size=1.85)  + guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.direction = "horizontal", 
        legend.position = "bottom",
        legend.box = "horizontal") +
  scale_colour_brewer(palette = "Accent")




####### CWR ACP #####

cwr<- read.csv2("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_occurrence_data/_gp1_data/GP1_points.csv",sep="|")
str(cwr)
cwr<- data.frame(Latitude=as.numeric(as.character(cwr$latitude)), Longitude=as.numeric(as.character(cwr$longitude) ) )

cwr<- cwr[ complete.cases(cwr)  ,]
cwr<- cwr[-which(cwr$Latitude==0), ]




files<-dir(file.path("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/raster_sdm/2_5m"))
#files<-files[grep("dist",files)]

for(i in 1:length(files )){
  
  tryCatch( { 
    eval( parse( text =  paste0('raster_',files[i],'<-raster(',"'", file.path(paste0( root,"//gap_analysis_landraces/Input_data/raster_sdm/2_5m","/",files[i],"'" ) ),')' )  ) )    
    
    
    
    eval( parse( text= paste0("cwr <- data.frame( cwr,", substr(files[i],1, nchar(files[i])-4 ) ,"= extract(", 'raster_',files[i], ", cbind(cwr$Longitude,cwr$Latitude) ,df=T)[,2] )"  )  ) ) 
    
    eval(parse(text= paste0("rm(", "raster_", files[i], ")"  ) )  )
    
  }, error=function(e){ cat("ERROR :",conditionMessage(e), "\n") } )  
  
  cat(paste0("procesando:"," ",i , "\n") )
}



cwr<-na.omit(cwr)



#dmodel.acp <- dmodel %>% dplyr::select( .,-Analysis ,-Genepool.predicted, -Race.predicted )  
M<-cor(cwr)
corrplot(M)

highlyCorDescr <- findCorrelation(M, cutoff = .70)
names(cwr)[highlyCorDescr]


acp<- PCA( cwr , quanti.sup = which( names(cwr) %in% names(cwr)[-highlyCorDescr]  )  )


plot(acp)
names(dmodel)




df<-data.frame(acp$ind$coord[,1:2],gen.pred=dmodel$Genepool.predicted )
plot(acp)
ggplot(df,aes_string(x="Dim.1",y="Dim.2",color="gen.pred", shape="gen.pred")) + geom_point(size=1.85)  + guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.direction = "horizontal", 
        legend.position = "bottom",
        legend.box = "horizontal") +
  scale_colour_brewer(palette = "Accent")


###### acp landraces y CWR ###

names(dmodel)

names(cwr)

cwr_landra <- dmodel %>% dplyr::select(.,names(cwr),Genepool.predicted) %>% bind_rows(.,cwr) 
cwr_landra<- data.frame(cwr_landra,stringsAsFactors=FALSE)
str(cwr_landra)

cwr_landra$Genepool.predicted<-as.character(cwr_landra$Genepool.predicted)
cwr_landra$Genepool.predicted[is.na(cwr_landra$Genepool.predicted)]  <- "CWR"
cwr_landra$Genepool.predicted<-as.factor(cwr_landra$Genepool.predicted)
cwr_landra<-na.omit(cwr_landra)




acp<-PCA(cwr_landra[,-ncol(cwr_landra)]  )


df<-data.frame(acp$ind$coord[,1:2],gen.pred=cwr_landra$Genepool.predicted )
plot(acp)
ggplot(df,aes_string(x="Dim.1",y="Dim.2",color="gen.pred", shape="gen.pred")) + geom_point(size=1.85)  + guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.direction = "horizontal", 
        legend.position = "bottom",
        legend.box = "horizontal") +
  scale_colour_brewer(palette = "Accent")


http://1.2.0.1/reg.php?ah_goal=politicas.html&ah_login=true&url=E2B8F3578D88E9BF2388F2468A984E8A8C28109A19

