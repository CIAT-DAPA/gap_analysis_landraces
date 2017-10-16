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

# Tables of interest
ciat; biophysicalVars; humanFactors
ciat$Genepool.interpreted.ACID[which(ciat$Genepool.interpreted.ACID == "Spain_Andean_I")] <- "Andean"
ciat$Genepool.literature.ACID[which(ciat$Genepool.literature.ACID == "Spain_Andean_I")] <- "Andean"

source("descriptive_analysis4cleanedDB.R")

####base de datos 
# genotypic_climate <- read.csv("C:/Users/acmendez/Google Drive/CIAT/ciat_beans_filtered_by_altitude_by_predictors_by_americas.csv")#readRDS("C:/Users/Usuario/Google Drive/CIAT/ciatOrganizedVariables_climate.RDS")
# genotypic_climate$Race.protein[genotypic_climate$Race.protein == "N/A"] <- NA
# genotypic_climate %>% glimpse


# ==================================== #
# Classification algorithms
# ==================================== #

## Genepool as response variable
# genotypic_climate <- readRDS("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/ciatOrganizedVariables_climate.RDS")
# genotypic_climate$Race.protein[genotypic_climate$Race.protein == "N/A"] <- NA
# genotypic_climate %>% glimpse

genotypic_climate <- read.csv("C:/Users/acmendez/Google Drive/CIAT/ciat_beans_filtered_with_climate.csv") #read.csv("C:/Users/Usuario/Google Drive/CIAT/ciat_beans_filtered_by_altitude_by_predictors_by_americas.csv")
 
genotypic_climate$Race.protein[genotypic_climate$Race.protein == "N/A"] <- NA
genotypic_climate %>% glimpse


genepool_data <- genotypic_climate %>% dplyr::select(Genepool, Altitude, Latitude:bio_19)
genepool_data <- genepool_data[complete.cases(genepool_data),]; rownames(genepool_data) <- 1:nrow(genepool_data)
genepool_data$Genepool <- factor(genepool_data$Genepool)
genepool_data$Growth.habit <- factor(genepool_data$Growth.habit)
genepool_data$Seed.shape <- factor(genepool_data$Seed.shape)
genepool_data$Seed.brightness <- factor(genepool_data$Seed.brightness)
genepool_data$Race.protein <- factor(genepool_data$Race.protein)
genepool_data<-genepool_data %>% dplyr::select(.,Genepool, Altitude, Latitude,Seed.weight,Color_Black:bio_19 )
M<-cor(genepool_data %>% dplyr::select(.,bio_1:bio_19) )
corrplot(M)

#detectar varaibles con bajas freceencias y con dsd(x)=0
nzv <- nearZeroVar(genepool_data)
genepool_data<-genepool_data[,-nzv]
#detectar varibles altamente correlacionadas y quitarlas de la base de datos
# numeric<-genepool_data %>% dplyr::select(.,bio_1:bio_19)
# descrCor<-cor(numeric)
# highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
# numeric  <- numeric[,-highlyCorDescr]
# genepool_data <- cbind(genepool_data %>% select(Genepool:(which(names(genepool_data)=="bio_1")-1)), numeric)
genepool_data<-genepool_data %>% mutate(.,genepool_bin=ifelse(Genepool=="Andean",1,0))%>% mutate_at(., vars(genepool_bin),funs(factor(.))) %>% dplyr::select(.,-Genepool)




#REGRESION LOGISTICA

genepool_folds <- modelr::crossv_kfold(genepool_data, k = 5)

genepool_models <- genepool_folds %>% mutate(model = map(train, ~ glm((genepool_bin) ~ ., data =.,family=binomial(link="logit"))))
colSums(genepool_data)
m1<-genepool_models$model$`1`
summary(m1)

predicted<-predict(m1,genepool_folds$test$`1`$data,type="response")

validate<-cbind(prob=predicted,genepool=genepool_folds$test$`1`$data$genepool_bin)

hist(predicted)
optCutOff <- optimalCutoff(genepool_folds$test$`1`$data$genepool_bin, predicted,optimiseFor ="Both")[1];optCutOff 

validate<- as.data.frame(validate) %>% mutate(.,prediccion=ifelse(prob>=optCutOff,1,0))

table(validate[,2:3])


#with(  , table( category, SubCategory) )


matriz<-matrix(0,nrow(validate),2)
options(scipen=999)
for(j in 1:nrow(validate)){

  pt.corte<-validate[j,"prob"]
  
  
  
  cont.a<-nrow(validate[which(validate[,"prob"]>=pt.corte & validate[,"genepool"]==1 ),])
  cont.b<-nrow(validate[which(validate[,"prob"]>=pt.corte & validate[,"genepool"]==0 ),])
  cont.c<-nrow(validate[which(validate[,"prob"]<pt.corte & validate[,"genepool"]==1 ),])
  cont.d<-nrow(validate[which(validate[,"prob"]<pt.corte & validate[,"genepool"]==0 ),])
  
  
  
  sensi<-cont.a/(cont.a+cont.c)
  especi<-cont.d/(cont.b+cont.d)
  
  matriz[j,1]<-sensi
  matriz[j,2]<-especi
}

se<-matriz[,1]
es<-matriz[,2]
es2<-(1-es)

you<-se+es-1

mtx<-cbind(prob=validate$prob,sensi=se,especi=es,you=you)

optimcut<-mtx[which(you==max(you)),];optimcut
plot(sort(es2),sort(se),xlab="1-Especificidad",ylab="Sensibilidad",type="l",main="Curva ROC")
lines(seq(0,1,0.01),seq(0,1,0.01))  
AUC<-trapz(sort(es2),sort(se))
legend(0.5,0.5,paste("AUC=",round(AUC*100,1),"%"),cex=1.5,bty="n",inset=0.1)
q1<-AUC/(2-AUC)
q2<-2*AUC^2/(1+AUC)
se.AUC<-sqrt((AUC*(1-AUC)+(sum(validate$genepool==1)-1)*(q1-AUC^2)+(sum(validate$genepool==0)-1)*(q2-AUC^2))/(sum(validate$genepool==1)*sum(validate$genepool==0)))
ls.AUC<-AUC+1.96*(se.AUC)
li.AUC<-AUC-1.96*(se.AUC)
cat("IC 95% AUC ","<", li.AUC,";",ls.AUC,">")





set.seed(8205)
trainIndex <- createDataPartition(genepool_data$genepool_bin, p = .8, 
                                  list = FALSE
                                  )

training<-genepool_data[trainIndex,]
test<-genepool_data[-trainIndex,]

str(training)
fitcontrol<- trainControl(method = "repeatedcv",
                          
                          repeats = 5
)

set.seed(825)
glmFit1 <- train(as.factor(genepool_bin) ~., data = training, 
                 method = "glm",
                 family="binomial",
                 metric = "ROC",
                 trControl = fitcontrol
                 
                 #importance = T
                 
                 ## This last option is actually one
                 ## for gbm() that passes through
              )
glmFit1$finalModel
pred<-predict(glmFit1,test[,-ncol(test)])
caret::confusionMatrix(test[,ncol(test)],pred)
varImp(glmFit1)







set.seed(825)
ctrol<-trainControl(method="LGOCV",number=1,repeats=10,savePredictions = T)
glmFit2<-train(as.factor(genepool_bin)~., data=genepool_data,
             method="glm",
             family="binomial",
             
             trControl=ctrol
               
               )

View(glmFit2$pred)

glmFit2
varImp(glmFit2)
fitted2<-predict(glmFit2,test)

confusionMatrix(fitted2, test$genepool_bin)

#Adaptive Mixture Discriminant Analysis
set.seed(825)
ctrol2<-trainControl(method="LGOCV",p=0.8,number=10,repeats=10,savePredictions = T)
glmFit3<-train(as.factor(genepool_bin)~., data=genepool_data,
               method="amdai",
               
               
               trControl=ctrol2
               
)
glmFit3


#Bagged Flexible Discriminant Analysis
genepool_data<- genepool_data %>% mutate_at(.,vars(genepool_bin),funs(factor(.)))
set.seed(825)
ctrol2<-trainControl(method="LGOCV",p=0.8,number=5,savePredictions = T)
glmFit3<-train((genepool_bin)~., data=genepool_data,
               method="bagEarth",
               trControl=ctrol2)


glmFit3$finalModel$oob

pred<-predict(glmFit3$finalModel,newdata=test[,-ncol(test)],type="class");pred
summary(glmFit3$finalModel)
length(t(test[,ncol(test)]))
table(pred,t(test[,ncol(test)]))
caret::confusionMatrix(table(pred,t(test[,ncol(test)])))


#caret::confusionMatrix(glmFit3$pred$obs[glmFit3$pred$nprune=="41" & glmFit3$pred$Resample=="Resample1"],glmFit3$pred$pred[glmFit3$pred$nprune=="41" & glmFit3$pred$Resample=="Resample1"])

#     Extraer la base de datos de acceciones a predecir
genepool_na <- genotypic_climate[!complete.cases(genotypic_climate$Genepool),]; #rownames(genotypic_climate) <- 1:nrow(genepool_data)
row.names(genepool_na)<-genepool_na$ID
genepool_na<- genepool_na %>% dplyr::select(.,Genepool, Altitude, Latitude,Seed.weight,Color_Black:bio_19 ) # %>% Filter( function(x) sd(x)!=0,. )
genepool_na<- genepool_na[complete.cases(genepool_na[,2:ncol(genepool_na)]),]

pred<-predict(glmFit3$finalModel,newdata=genepool_na[,-1],type="class")
genepool_na<- genepool_na %>% mutate(.,Gp_FDA=pred )


set.seed(825)
ctrol2<-trainControl(method="LGOCV",p=0.8,number=3,repeats=10,savePredictions = T)

#Binary Discriminant Analysis

glmFit3<-train(as.factor(genepool_bin)~., data=genepool_data,
               method="binda",
               
               
               trControl=ctrol2
               
)
glmFit3
#Penalized Linear Discriminant Analysis


glmFit3<-train(as.factor(genepool_bin)~., data=genepool_data,
               method="PenalizedLDA",
               
               
               trControl=ctrol2
               
)
glmFit3

#####################################################################
#####################################################################
## FUNCION PARA HACER LA CLASIFICACI?N PARA TODAS LAS METODOLOGIAS###
#####################################################################
#####################################################################

# genotypic_climate <- read.csv("C:/Users/acmendez/Google Drive/CIAT/ciat_beans_filtered_with_climate.csv") #read.csv("C:/Users/Usuario/Google Drive/CIAT/ciat_beans_filtered_by_altitude_by_predictors_by_americas.csv")

# ------------------------------------ #
# Load data
# ------------------------------------ #
genotypic_climate <- read.csv("/home/hachicanoy/bean_landraces/ciat_descriptors_climate_hfactors.csv")
rownames(genotypic_climate) <- genotypic_climate$ID
#genotypic_climate <- genotypic_climate[,-which(names(genotypic_climate) == "ID")]

# ------------------------------------ #
# Shiny app for selection of variables
# ------------------------------------ #
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
# Function to predict level of analysis
# ------------------------------------ #

genepool_predicted <- function(data_gen = genotypic_climate, y = "Genepool.interpreted.ACID", area = "Americas"){
  
  ################################
  # Train models
  ################################
  
  # Select response variable
  if(y == "Genepool.interpreted.ACID"){
    data_gen$Race.interpreted.ACID <- NULL
    data_gen$Subgroup.interpreted.ACID <- NULL
  } else {
    if(y == "Race.interpreted.ACID"){
      data_gen$Genepool.interpreted.ACID <- NULL
      data_gen$Subgroup.interpreted.ACID <- NULL
    } else {
      if(y == "Subgroup.interpreted.ACID"){
        data_gen$Genepool.interpreted.ACID <- NULL
        data_gen$Race.interpreted.ACID <- NULL
      }
    }
  }
  eval(parse(text = paste0("data_gen$", y, " <- as.character(data_gen$", y, ")")))
  
  if(length(grep("Spain_Andean_I", eval(parse(text = paste0("data_gen$", y))))) != 0){
    eval(parse(text = paste0("data_gen$", y, "[which(data_gen$", y, " =='Spain_Andean_I')] <- 'Andean'")))
  }
  
  eval(parse(text = paste0("data_gen$", y, " <- as.factor(data_gen$", y, ")"))) 
  
  # Filter data
  row.names(data_gen) <- data_gen$ID
  data_gen2 <- data_gen %>%
    dplyr::filter(., Analysis == area & To.use.ACID == 1) %>%
    dplyr::select(., -ID, -Analysis, -To.use.ACID)
  
  # Let complete data for the training process
  genepool_data <- data_gen2
  genepool_data <- genepool_data[complete.cases(genepool_data),]
  
  genepool_data$Growth.habit <- factor(genepool_data$Growth.habit)
  genepool_data$Seed.shape <- factor(genepool_data$Seed.shape)
  genepool_data$Seed.brightness <- factor(genepool_data$Seed.brightness)
  genepool_data$Genepool.protein[which(genepool_data$Genepool.protein == "N/A")] <- NA
  genepool_data$Genepool.protein <- factor(genepool_data$Genepool.protein)
  
  genepool_data <- genepool_data[complete.cases(genepool_data),]
  # genepool_data<-genepool_data %>% dplyr::select(.,Genepool, Altitude, Latitude,Seed.weight,Color_Black:bio_19 )
  
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
  
  only_numeric <- function(genepool_data){
    
    genepool_data2 <- genepool_data[,sapply(genepool_data, is.numeric)]
    genepool_data <- eval(parse(text = paste0("data.frame(", y, "=", "genepool_data$", y, ",", "genepool_data2", ")")))
    return(genepool_data)
    
  }
  
  # Define parameters to train models
  set.seed(825); ctrol2 <- trainControl(method = "LGOCV", p = 0.8, number = 1, savePredictions = T)
  
  # Model 1
  # Bagged Flexible Discriminant Analysis
  
  data_in <- only_numeric(genepool_data)
  eval(parse(text = paste0("FDA <- train(", y, " ~ ., data = data_in, method = 'bagFDA', trControl = ctrol2)"))) # FDA training
  
  # Model 2
  # Logistic Regression Model
  
  vf <- colinearity(genepool_data)
  pos <- which(sapply(vf, is.factor))
  for(i in 1:length(pos)){
    vf[,pos[i]] <- make.names((vf[,pos[i]]))
  }
  eval(parse(text = paste0("glmFit1 <- train(", y, " ~ ., data = vf, method = 'glm', family = 'binomial', trControl = ctrol2)"))) # GLM training
  
  # Model 3
  # Random Forest
  
  grid <- expand.grid(mtry = round((ncol(genepool_data)-4)/3))
  eval(parse(text = paste0("Rforest <- train(", y, " ~ ., data = genepool_data, method = 'rf', tuneGrid = grid, importance = TRUE, ntree = 2000, metric = 'Accuracy', trControl = ctrol2)"))) # RF training
  
  # Model 4
  # Support Vector Machines
  eval(parse(text = paste0("svmFit <- train(", y, " ~ ., data = genepool_data, method = 'svmRadial', tuneLength = 9, trControl = ctrol2, importance = T)")))
  
  ################################
  # Predict response
  ################################
  
  genepool_na <- data_gen[!complete.cases(eval(parse(text = paste0("data_gen$", y)))),] #rownames(data_gen) <- 1:nrow(genepool_data)
  
  genepool_na <- genepool_na[, names(genepool_data)] # %>% Filter( function(x) sd(x)!=0,. )
  genepool_na$Genepool.protein[which(genepool_na$Genepool.protein == "N/A")] <- NA
  genepool_na$Genepool.protein <- factor(genepool_na$Genepool.protein)
  genepool_na <- genepool_na[complete.cases(genepool_na[,-which(names(genepool_na) == y)]),]
  
  model_type <- c("FDA", "glmFit1", "Rforest", "svmFit")
  
  predictions <- lapply(model_type, function(x){ 
    
    model <- eval(parse(text = x))
    
    ifelse(model$method == "glm"|model$method == "rf", tp <- "response", tp <- "class")
    
    if(model$method == "rf"){
      pred <- predict(model, newdata = genepool_na[,-which(names(genepool_na) == y)])
    }
    
    if(model$method == "svmRadial"){
      pred <- predict(model, newdata = genepool_na[,-which(names(genepool_na) == y)])
    }
    
    if(model$method == "bagFDA"){
      pred <- predict(model$finalModel, newdata = genepool_na[,names(data_in)] ,type = tp)
    }
    
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
  
}

predictions <- genepool_predicted(data_gen = genotypic_climate, y = "Genepool.interpreted.ACID", area = "Americas")
df<-predictions[[2]]





###########################################################
###### MULTINOMIAL LOGISTIC REGRESSION  "nnet" package#####
###########################################################
install.packages("nnet")
library("nnet")

genepool_data<-genepool_data[complete.cases(genepool_data$Race.interpreted.lit),]
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


