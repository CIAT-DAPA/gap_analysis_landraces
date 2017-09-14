
OSys <- Sys.info(); OSys <- OSys[names(OSys)=="sysname"]
if(OSys == "Linux"){ root <- "/mnt/workspace_cluster_9" } else {
  if(OSys == "Windows"){ root <- "//dapadfs/Workspace_cluster_9" }
}; rm(OSys)

# Load packages
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))
suppressMessages(library(rgdal))
suppressMessages(library(sp))
suppressMessages(library(raster))
suppressMessages(library(ncdf4))
suppressMessages(library(rasterVis))
suppressMessages(library(htmlwidgets))
suppressMessages(library(compiler))
suppressMessages(library(leaflet))
suppressMessages(library(highcharter))
suppressMessages(library(plotly))
suppressMessages(library(d3heatmap))
suppressMessages(library(cluster))
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(corrplot))
suppressMessages(library(Rtsne))
suppressMessages(library(randomForest))
suppressMessages(library(caret))
suppressMessages(library(modelr))
suppressMessages(library(broom))
suppressMessages(library(purrr))
suppressMessages(if(!require(InformationValue)){install.packages("InformationValue");library(InformationValue)}else{library(InformationValue)})
suppressMessages(if(!require(corrplot)){install.packages("corrplot");library(corrplot)}else{library(corrplot)})
suppressMessages(if(!require(caTools)){install.packages("caTools");library(caTools)}else{library(caTools)})
suppressMessages(if(!require(caret)){install.packages("caret");library(caret)}else{library(caret)})
suppressMessages(if(!require(shiny)){install.packages("shiny");library(shiny)}else{library(shiny)})
suppressMessages(if(!require(miniUI)){install.packages("miniUI");library(miniUI)}else{library(miniUI)})


## =================================================================================================================== ##
## CIAT information with climate data (with phaseolin data and race/genepool classification)
## =================================================================================================================== ##

phenotypic_data <- read_excel(path = paste0(root, "/gap_analysis_landraces/Input_data/_phenotypic_data/Bean/Blair_et_al_2009_races_subgroups_phenotypic.xlsx"), sheet = 2)

par(mfrow = c(1,3))
phenotypic_data %>% select(Meso.total, Andean.total) %>% cor(method = "pearson", use = "complete.obs") %>% corrplot.mixed()
phenotypic_data %>% select(Meso.total, Andean.total) %>% cor(method = "spearman", use = "complete.obs") %>% corrplot.mixed()
phenotypic_data %>% select(Meso.total, Andean.total) %>% cor(method = "kendall", use = "complete.obs") %>% corrplot.mixed()

par(mfrow = c(1,3))
phenotypic_data %>% select(D_J1, D_J2, G, M1, M2, NG1, NG2, P1, P2) %>% cor(method = "pearson", use = "complete.obs") %>% corrplot.mixed()
phenotypic_data %>% select(D_J1, D_J2, G, M1, M2, NG1, NG2, P1, P2) %>% cor(method = "spearman", use = "complete.obs") %>% corrplot.mixed()
phenotypic_data %>% select(D_J1, D_J2, G, M1, M2, NG1, NG2, P1, P2) %>% cor(method = "kendall", use = "complete.obs") %>% corrplot.mixed()
# phenotypic_data %>% select(D_J1, D_J2, G, M1, M2, NG1, NG2, P1, P2) %>% cor(method = "kendall", use = "complete.obs") %>% corrplot(is.corr = T, method = "ellipse", type = "upper")

par(mfrow = c(1,3))
phenotypic_data %>% select(D_J.total, G, M.total, NG.total, P.total) %>% cor(method = "pearson", use = "complete.obs") %>% corrplot.mixed()
phenotypic_data %>% select(D_J.total, G, M.total, NG.total, P.total) %>% cor(method = "spearman", use = "complete.obs") %>% corrplot.mixed()
phenotypic_data %>% select(D_J.total, G, M.total, NG.total, P.total) %>% cor(method = "kendall", use = "complete.obs") %>% corrplot.mixed()
rm(phenotypic_data)


####base de datos 
genotypic_climate <- read.csv("C:/Users/acmendez/Google Drive/CIAT/ciat_beans_filtered_by_altitude_by_predictors_by_americas.csv")#readRDS("C:/Users/Usuario/Google Drive/CIAT/ciatOrganizedVariables_climate.RDS")
genotypic_climate$Race.protein[genotypic_climate$Race.protein == "N/A"] <- NA
genotypic_climate %>% glimpse

# ==================================== #
# Univariate descriptive analysis
# ==================================== #

# Quantitative variables: histograms
test <- genotypic_climate %>% dplyr::select(Genepool,Altitude, Longitude, Latitude, Seed.weight, bio_1:bio_19) %>%
  gather(Variable, Value, -Genepool)
test <- test[complete.cases(test),] %>% as.data.frame

gg<-ggplot(data = test, aes(x = Value, alpha = .6, colour = factor(Genepool))) + # fill = Variable
  geom_density() +
  facet_wrap(~Variable, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(face = "bold")) +
  guides(alpha = F, fill = F) +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
if(!file.exists(paste0(root, "/gap_analysis_landraces/Results/density_quantitative_variables_by_genepool.png"))){
  ggsave(paste0(root, "/gap_analysis_landraces/Results/density_quantitative_variables_by_genepool.png"), plot = gg, width = 22, height = 10, units = "in"); rm(gg)
}; rm(gg)




# Quantitative variables: descriptive statistics
genotypic_climate %>% dplyr::select(Altitude, Longitude, Latitude, Seed.weight, bio_1:bio_19) %>%
  psych::describe() %>% select(mean, sd, median, min, max, range) %>% as.data.frame %>%
  round(., digits = 2) %>% write.csv(., file = paste0(root, "/gap_analysis_landraces/Results/descriptiveStats_quantitative_variables.csv"), row.names = T)

# Qualitative variables: create a table of counts
fqTable <- genotypic_climate %>% dplyr::select(Vernacular.name, Genepool, Race.interpreted, Subgroup, Growth.habit, Seed.shape, Seed.brightness, Race.protein) %>%
  gather(measure, value) %>%
  count(measure, value) %>%
  spread(measure, n) %>%
  gather(key = Variable, value = Count, Genepool:Vernacular.name)
fqTable <- fqTable[complete.cases(fqTable),]; rownames(fqTable) <- 1:nrow(fqTable); colnames(fqTable)[1] <- "Category"
fqTable <- fqTable %>% dplyr::mutate(Percentage = Count/nrow(genotypic_climate))
fqTable <- fqTable %>% as.data.frame
# Color variable
fqTable <- rbind(fqTable, data.frame(Category = genotypic_climate[,grep(pattern = "^Color_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% names %>% gsub(pattern = "Color_", replacement = "", x = .),
                                     Variable = "Color",
                                     Count = genotypic_climate[,grep(pattern = "^Color_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% as.numeric,
                                     Percentage = genotypic_climate[,grep(pattern = "^Color_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% as.numeric / nrow(genotypic_climate)))
# Protein variable
fqTable <- rbind(fqTable, data.frame(Category = genotypic_climate[,grep(pattern = "^Protein_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% names %>% gsub(pattern = "^Protein_", replacement = "", x = .),
                                     Variable = "Protein",
                                     Count = genotypic_climate[,grep(pattern = "^Protein_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% as.numeric,
                                     Percentage = genotypic_climate[,grep(pattern = "^Protein_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% as.numeric / nrow(genotypic_climate)))

# Qualitative variables: barplot per variable
gg <- fqTable %>% ggplot(aes(x =  reorder(Category, Percentage), y = Percentage*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Percentage (%)") +
  coord_flip() +
  facet_wrap(~ Variable, scales = "free") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
if(!file.exists(paste0(root, "/gap_analysis_landraces/Results/barplot_qualitative_variables.png"))){
  ggsave(paste0(root, "/gap_analysis_landraces/Results/barplot_qualitative_variables.png"), plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)
}; rm(gg)

# ==================================== #
# Multivariate descriptive analysis
# ==================================== #

# Principal Component Analysis for mixed data

# Cluster analysis

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
## FUNCION PARA HACER LA CLASIFICACIÓN PARA TODAS LAS METODOLOGIAS###
#####################################################################
#####################################################################

genotypic_climate <- read.csv("C:/Users/acmendez/Google Drive/CIAT/ciat_beans_filtered_with_climate.csv") #read.csv("C:/Users/Usuario/Google Drive/CIAT/ciat_beans_filtered_by_altitude_by_predictors_by_americas.csv")

ui<-miniPage(
  gadgetTitleBar("Shiny gadget example"),
  miniContentPanel(padding = 0,
                   checkboxGroupInput("vars","Select Vars", choices=names(genotypic_climate ),selected = names(genotypic_climate ))
  )
)

server<- function(input,output,session){
  
  observeEvent(input$done,{
    genotypic_climate <<-genotypic_climate [,input$vars]
    stopApp(genotypic_climate )
  })
  
}

runGadget(shinyApp(ui, server),viewer = dialogViewer("Select Vars", width = 600, height = 600))

genepool_predicted<- function(data_gen=genotypic_climate,y="Genepool.lit",area="Americas"){
  
  row.names(data_gen)<-data_gen$ID
  data_gen<- data_gen %>% filter(., Analysis==area) %>% select(., -ID,-Analysis)

  
  genepool_data<-data_gen
  genepool_data <- genepool_data[complete.cases(genepool_data),]
 
   
 if(length( grep("Andean_Spain_I",eval(parse(text=paste0("data_gen$",y)))))!=0){
   eval(parse(text=paste0("data_gen$",y, "[","which(data_gen$",y," =='Andean_Spain_I'",")","]","<-'Andean'")))
   
 }

  
  eval(parse(text= paste0("data_gen$",y,"<-","factor(","data_gen$",y,")") )) 

  genepool_data<-data_gen
  
  genepool_data$Growth.habit <- factor(genepool_data$Growth.habit)
  genepool_data$Seed.shape <- factor(genepool_data$Seed.shape)
  genepool_data$Seed.brightness <- factor(genepool_data$Seed.brightness)
  genepool_data$Genepool.protein[which(genepool_data$Genepool.protein=="N/A")]<-NA
  genepool_data$Genepool.protein <- factor(genepool_data$Genepool.protein)
  
  genepool_data <- genepool_data[complete.cases(genepool_data),]
  #genepool_data<-genepool_data %>% dplyr::select(.,Genepool, Altitude, Latitude,Seed.weight,Color_Black:bio_19 )
  
  
  
  #detectar varaibles con bajas freceencias y con dsd(x)=0
  nzv <- nearZeroVar(genepool_data)
  genepool_data<-genepool_data[,-nzv]

  
    colinearity<-function(genepool_data){
      
      #detectar varibles altamente correlacionadas y quitarlas de la base de datos
      numeric<-genepool_data %>% dplyr::select(.,bio_1:bio_19)
      descrCor<-cor(numeric)
      highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
      numeric  <- numeric[,-highlyCorDescr]
      genepool_data <- cbind(genepool_data %>% select(1:(which(names(genepool_data)=="bio_1")-1)), numeric)
      
      return(genepool_data)
    }
    
    
    only_numeric<- function(genepool_data){
      
      genepool_data2<-genepool_data[,sapply(genepool_data, is.numeric)]
      genepool_data<-eval(parse(text=paste0("data.frame(",y,"=","genepool_data$",y,",","genepool_data2",")")))
      return(genepool_data)
    }
    
    
    
    
    
    set.seed(825)
    ctrol2<-trainControl(method="LGOCV",p=0.8,number=1,savePredictions = T)
    #Bagged Flexible Discriminant Analysis
   
    data_in<-only_numeric(genepool_data)
   
    #FDA train
    eval(parse(text=paste0( "FDA<-train(",y,"~.,data=data_in,method='bagFDA',trControl=ctrol2)" ) ))
    
    
    folds<-
    mda(Genepool.lit~.,data=data_in)

#Clasificación por Regresion Logistica
   vf<-colinearity(genepool_data)
   pos<-which(sapply(vf, is.factor))
   for(i in 1:length(pos)){
   
     vf[,pos[i]] <- make.names((vf[,pos[i]]))
     
   }
   
 eval(parse(text = paste0("glmFit1<-train(",y,"~.,data =vf,method = 'glm', family='binomial',trControl = ctrol2)" )  ))
  
  

  #Clasificación Randon Forest
  
  grid <- expand.grid(mtry = round((ncol(genepool_data)-4)/3))
  
  eval(parse(text = paste0( "Rforest<-train(",y,"~.,data=genepool_data,method= 'rf',tuneGrid=grid, importance=TRUE,ntree=2000,metric='Accuracy', trControl=ctrol2)"  )  ))
  

     
 #svm clasificaction
  
  eval(parse(text = paste0("svmFit<-train(",y,"~.,data=genepool_data,method='svmRadial',tuneLength=9,trControl=ctrol2,importance=T)" ) ))
  
  
    genepool_na <- data_gen[!complete.cases(eval(parse(text=paste0("data_gen$",y)))),]; #rownames(data_gen) <- 1:nrow(genepool_data)
  
    genepool_na<- genepool_na %>% dplyr::select(., names(data_gen)) # %>% Filter( function(x) sd(x)!=0,. )
    genepool_na$Genepool.protein[which(genepool_na$Genepool.protein=="N/A")]<-NA
    genepool_na$Genepool.protein <- factor(genepool_na$Genepool.protein)
    genepool_na<- genepool_na[complete.cases( genepool_na %>% dplyr::select(., names(genepool_na)[-which(names(genepool_na)==y)])  ),   ]
    
    
    
    model_type<-c("FDA","glmFit1","Rforest","svmFit")

    predictions<-lapply(model_type,function(x){ 
      
      model<-eval(parse(text=x))

    ifelse(model$method=="glm" | model$method=="rf",tp<-"response",tp<-"class")

    if(model$method=="rf") {
      pred<-predict(model,newdata=genepool_na[,-which(names(genepool_na)==y)])
    }

    if(model$method=="svmRadial") {
      pred<-predict(model,newdata=genepool_na[,-which(names(genepool_na)==y)])
    }

    if(model$method=="bagFDA"  ){
      pred<-predict(model$finalModel,newdata= genepool_na[,names(data_in)] ,type=tp)

      }


    if(model$method=="glm"){

      vf_p<-genepool_na[,names(vf)[-which(names(vf)==y)] ]
      pos<-which(sapply(vf_p,is.factor))

      for(i in 1:length(pos)){
        vf_p[,pos[i]]<-make.names((vf_p[,pos[i]]  ))

      }

      g1<-glm(factor(Genepool.lit)~.,data=vf,family = binomial(link = "logit"))

      pred<- predict(g1,newdata=na.omit(vf_p),type="response")
      pred<-ifelse(pred<0.5,"Andean","Mesoamerican")
      pred<- as.factor(pred)
      }



     return(pred)
                })
    
    
   
    names(predictions)<- model_type
    
    
    accu.FDA<-mean(FDA$finalModel$oob[,1])
    
    accu.glmFit1<-mean(apply(gd<-data.frame(glmFit1$resampledCM[,1:4]),1,function(x){
    (x[1] + x[4]) /sum(x)
                      }) )
    accu.Rforest<- mean(apply(gd<-data.frame(Rforest$resampledCM[,1:4]),1,function(x){
      (x[1] + x[4]) /sum(x)
    }) )
      
    
    accu.svm<-  mean(apply(gd<-data.frame(svmFit$resampledCM[,1:4]),1,function(x){
      (x[1] + x[4]) /sum(x)
    }) )
    
    accuracy<-c(accu.FDA,accu.glmFit1,accu.Rforest,accu.svm)
    names(accuracy)<-model_type
    return(list(data_predicted=data.frame(genepool_na,predictions),models_accuracy=accuracy,data=data_gen))
    
    } #End funcion para calcular los parametros de cada


predictions<-genepool_predicted(data_gen=genotypic_climate,y="Genepool.lit",area="Americas")



