# Explore and classify occurrence data
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

# Load data
genotypic_climate <- read.csv("/home/hachicanoy/bean_landraces/ciat_descriptors_climate_hfactors.csv")
rownames(genotypic_climate) <- genotypic_climate$ID

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

# ------------------------------------ #
# Main function
# ------------------------------------ #
y = "Race.interpreted.ACID"
genepool_predicted <- function(data_gen = genotypic_climate, y = "Genepool.interpreted.ACID", area = "Americas"){
  
  # ---------------------------------------------------------------- #
  # Train models
  # ---------------------------------------------------------------- #
  
  cat("\n>>>> Starting training process\n\n")
  
  # Select response variable
  if(y == "Genepool.interpreted.ACID"){
    if(assertthat::has_name(data_gen, "Race.interpreted.ACID")){data_gen$Race.interpreted.ACID <- NULL}
    if(assertthat::has_name(data_gen, "Subgroup.interpreted.ACID")){data_gen$Subgroup.interpreted.ACID <- NULL}
  } else {
    if(y == "Race.interpreted.ACID"){
      if(assertthat::has_name(data_gen, "Genepool.interpreted.ACID")){data_gen$Genepool.interpreted.ACID <- NULL}
      if(assertthat::has_name(data_gen, "Subgroup.interpreted.ACID")){data_gen$Subgroup.interpreted.ACID <- NULL}
    } else {
      if(y == "Subgroup.interpreted.ACID"){
        if(assertthat::has_name(data_gen, "Genepool.interpreted.ACID")){data_gen$Genepool.interpreted.ACID <- NULL}
        if(assertthat::has_name(data_gen, "Race.interpreted.ACID")){data_gen$Race.interpreted.ACID <- NULL}
      }
    }
  }
  
  # Process response variable
  eval(parse(text = paste0("data_gen$", y, " <- as.character(data_gen$", y, ")")))
  eval(parse(text = paste0("data_gen$", y, "[which(data_gen$", y, " == 'N/A')] <- NA")))
  if(length(grep("Spain_Andean_I", eval(parse(text = paste0("data_gen$", y))))) != 0){
    eval(parse(text = paste0("data_gen$", y, "[which(data_gen$", y, " =='Spain_Andean_I')] <- 'Andean'")))
  }
  eval(parse(text = paste0("data_gen$", y, " <- factor(data_gen$", y, ")"))) 
  
  # Apply filters
  row.names(data_gen) <- data_gen$ID
  data_gen2 <- data_gen %>%
    dplyr::filter(., Analysis == area & To.use.ACID == 1) %>%
    dplyr::select(., -ID, -Analysis, -To.use.ACID)
  
  # Arrange and let just completed data for the training process
  genepool_data <- data_gen2
  genepool_data$Genepool.protein[which(genepool_data$Genepool.protein == "N/A")] <- NA
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
  
  # Function to exclude correlated variables before to model
  colinearity <- function(genepool_data){
    
    numeric <- genepool_data[,sapply(genepool_data, is.numeric)]
    descrCor <- cor(numeric)
    highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
    numeric <- numeric[,-highlyCorDescr]
    vec <- which(names(genepool_data) %in% names(numeric))
    genepool_data <- data.frame(genepool_data[,sapply(genepool_data, function(x){!is.numeric(x)})], numeric)
    return(genepool_data)
    
  }
  
  # Function to get just numeric variables before to model
  only_numeric <- function(genepool_data){
    
    genepool_data2 <- genepool_data[,sapply(genepool_data, is.numeric)]
    genepool_data <- eval(parse(text = paste0("data.frame(", y, "=", "genepool_data$", y, ",", "genepool_data2", ")")))
    return(genepool_data)
    
  }
  
  # Define parameters to train models
  set.seed(825); ctrol2 <- trainControl(method = "LGOCV", p = 0.8, number = 1, savePredictions = T)
  # In case of imbalance: ctrol2 <- trainControl(method = "LGOCV", p = 0.8, number = 1, savePredictions = T, sampling = "down")
  
  ##########################################
  # Model 1
  # Bagged Flexible Discriminant Analysis
  ##########################################
  
  cat("Running FDA ...\n")
  data_in <- only_numeric(genepool_data)
  eval(parse(text = paste0("FDA <- train(", y, " ~ ., data = data_in, method = 'bagFDA', trControl = ctrol2)"))) # FDA training
  
  ##########################################
  # Model 2
  # GLM: Logistic Regression Model
  ##########################################
  
  cat("Running GLM ...\n")
  vf <- colinearity(genepool_data)
  pos <- which(sapply(vf, is.factor))
  for(i in 1:length(pos)){
    vf[,pos[i]] <- make.names((vf[,pos[i]]))
  }
  eval(parse(text = paste0("glmFit1 <- train(", y, " ~ ., data = vf, method = 'glm', family = 'binomial', trControl = ctrol2)"))) # GLM training
  
  ##########################################
  # Model 3
  # Random Forest
  ##########################################
  
  cat("Running Random Forest ...\n")
  grid <- expand.grid(mtry = round((ncol(genepool_data)-4)/3))
  eval(parse(text = paste0("Rforest <- train(", y, " ~ ., data = genepool_data, method = 'rf', tuneGrid = grid, importance = TRUE, ntree = 2000, metric = 'Accuracy', trControl = ctrol2)"))) # RF training
  
  ##########################################
  # Model 4
  # Support Vector Machines
  ##########################################
  
  cat("Running Support Vector Machine ...\n\n")
  eval(parse(text = paste0("svmFit <- train(", y, " ~ ., data = genepool_data, method = 'svmRadial', tuneLength = 9, trControl = ctrol2, importance = T)")))
  
  # ---------------------------------------------------------------- #
  # Predict new cases
  # ---------------------------------------------------------------- #
  
  cat(">>>> Starting predicting process\n\n")
  
  genepool_na <- data_gen[!complete.cases(eval(parse(text = paste0("data_gen$", y)))),]
  
  genepool_na <- genepool_na[, names(genepool_data)]
  genepool_na$Genepool.protein <- as.character(genepool_na$Genepool.protein)
  genepool_na$Genepool.protein[which(genepool_na$Genepool.protein == "N/A")] <- NA
  genepool_na$Genepool.protein <- factor(genepool_na$Genepool.protein)
  genepool_na$Growth.habit[which(genepool_na$Growth.habit == "Climbing-Determinate")] <- NA
  genepool_na$Growth.habit <- factor(genepool_na$Growth.habit)
  genepool_na <- genepool_na[complete.cases(genepool_na[,-which(names(genepool_na) == y)]),]
  
  model_type <- c("FDA", "glmFit1", "Rforest", "svmFit")
  
  predictions <- lapply(model_type, function(x){ 
    
    model <- eval(parse(text = x))
    
    ifelse(model$method == "glm" | model$method == "rf", tp <- "response", tp <- "class")
    if(model$method == "rf"){ pred <- predict(model, newdata = genepool_na[,-which(names(genepool_na) == y)]) }
    if(model$method == "svmRadial"){ pred <- predict(model, newdata = genepool_na[,-which(names(genepool_na) == y)]) }
    if(model$method == "bagFDA"){ pred <- predict(model$finalModel, newdata = genepool_na[,names(data_in)] ,type = tp) }
    if(model$method == "glm"){
      
      vf_p <- genepool_na[,names(vf)[-which(names(vf) == y)]]
      pos <- which(sapply(vf_p, is.factor))
      
      for(i in 1:length(pos)){
        vf_p[,pos[i]] <- make.names((vf_p[,pos[i]]))
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
  return(list(data_predicted = data.frame(genepool_na, predictions), models_accuracy = accuracy, data = data_gen))
  cat(">>>> Process done\n")
  
}

predictions <- genepool_predicted(data_gen = genotypic_climate, y = "Genepool.interpreted.ACID", area = "Americas")
df<-predictions[[2]]
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

suppressMessages(if(!require(rdrop2)){install.packages("rdrop2");library(rdrop2)}else{library(rdrop2)})
drop_auth()

