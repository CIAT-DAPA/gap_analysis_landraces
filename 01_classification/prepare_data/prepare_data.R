# Classification function
# Implemented by: H. Achicanoy, M.V. Díaz
# CIAT, 2018

suppressMessages(library(pacman))
pacman::p_load(tidyverse, caret, randomForest, ISLR, nnet, caretEnsemble, ranger, ModelMetrics, rminer)

# sampling_mthd <- c("none", "down", "up")

################################# Preparing dataset ######################################################

df<-"//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/sorghum/databases/final_base_climate_socio.csv"
df<-read.csv(df, header = T)
df %>% apply(.,2,function(x) sum(is.na(x))) %>% sort(., decreasing = T)
#df<-df[,-which(names(df) %in% c("collectioncode","Headbug","Cultivar.name","Downy.mildew....glasshouse.","Strigol.control","Downy.mildew....field.","Lysine....","Protein...." ))]

analysis<-"all" # "socioeconomic", "bioclimatic", "all"

if(analysis == "all"){
  base<-df %>% select(ensemble:elevation,Latitude, Longitude) #all variables : socio + bioclimatic
  
}else if(analysis == "bioclimatic"){
  base<-df %>% select(ensemble, Altitude:thermicityIndex, Latitude, Longitude) #only bioclimatic variables
  
}else if(analysis == "socioeconomic"){
  
  base<-df %>% select(ensemble, Accessibility, dist_h_set:elevation, Latitude, Longitude) #only socioeconomic variables
}


names(base)[1]<-"Y"

table(base$Y) #check the ocurrences number for each race.

#base<-base[which(base$Y %in% c("Bicolor", "Durra", "Guinea", "Caudatum", "Kafir")),]; 
levels(base$Y)[c(3,5,6,8,9,10,11,13,14,15)]<- NA  #remove the races with 0 ocurrences or races you want to remove
base_1<-base %>% drop_na()  #remove NAs for all dataframe

base_1$Y <- base_1$Y %>% as.character() %>% as.factor(); base_1<-base_1[complete.cases(base_1),]
base_2<-base[is.na(base$Y),];base_2<-base_2[,-1] ;base_2<-base_2[complete.cases(base_2),] #Only if you want to predict NAs races, because it has NAs for races. It is in external_df parameter into the function (external_df=base_2)


#####################################################################
## Run only when dataframe contain qualitative variables
## To convert the qualitative in dummy variables
#####################################################################

library(recipes)
rec_obj<-recipes::recipe(Y~.,data= base) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>%
  prep(data= base)

df_db<-bake(rec_obj, newdata = base)

#### Finish ###########

base<-df_db #If you run last lines

base$Y <- base$Y %>% as.character() %>% as.factor()
base_1<-base_1[complete.cases(base_1),]
base_2<-base[which(is.na(base$Y)),];base_2<-base_2[,-1] ;base_2<-base_2[complete.cases(base_2),] #Only if you want to predict NAs races. It is in external_df parameter into the function (external_df=base_2)

################################# Classification models ######################################################

source("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/scripts/01_classification/classification_function.R") #Load the classification function R script in its file location

classification<-classification_fun(df=base_1, external_df = base_2, standardize_all = T, top_variables = 5,omit_correlated = T,sampling_mthd = "none") 

#df= dataframe with all complete data (base_1) 
#external_df = NULL by default ... Also it could be base_2 if you want to predict NAs races
#standardize_all = TRUE by default ... Also, you can put it as FALSE if you don't want to normalize the numerical data.
#top_variables = 5 by default. But, if you want to see more than 5 important variables you can put as variables as you want.
#omit_correlated = T by default ... If you want to have the correlated variables you can put FALSE.
#sampling_mthd = "none" by default. But, if you want to sampling the ocurrences in order to balance the occurrences for each races, you can put: "up" or "down"

saveRDS(results,"C:/Users/ICRISAT/Desktop/charact_socio_bio.rds") #save the results of the function



