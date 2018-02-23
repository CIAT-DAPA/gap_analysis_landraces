####################################################################################################################
####################################################################################################################
####################################################################################################################
############################################################    SDM USING SDM PACKAGE    ###########################
###########################################################       CHRYSTIAN C. SOSA         ########################
###########################################################             2017                       #################
####################################################################################################################
####################################################################################################################
####################################################################################################################
library(sdm)
library(raster)
library(rgdal)
library(rJava)
library(dismo)

##########################################################
##PREVIOUS STEPS##
##########################################################

inDir<-"V:/07_Maxent"
bacK_Dir<-paste0(inDir,"/","_swd");if(!file.exists(bacK_Dir)){dir.create(bacK_Dir)}
occ_Dir<-paste0(inDir,"/","_occurrences");if(!file.exists(occ_Dir)){dir.create(occ_Dir)}
src.dir<-"V:/07_Maxent/_scripts" 
mod_Dir<-paste0(inDir,"/","_modelling");if(!file.exists(mod_Dir)){dir.create(mod_Dir)}

##########################################################
###BASELINE DIRECTORY
##########################################################

input_dir<-"V:/02_Gridded_data"
bl_dir<-paste0(input_dir,"/","baseline_2_5min_v2/average")
current_clim_dir<-paste0(bl_dir,"/","biovars");if(!file.exists(paste0(bl_dir,"/","biovars"))){dir.create(paste0(bl_dir,"/","biovars"))}

##########################################################
###SWD FILES CREATING
##########################################################

source(paste0(src.dir,"/","_swd.R"))
x<-swd_function(inDir=inDir,current_clim_dir=current_clim_dir,occ_Dir=occ_Dir,bg_create=F)
  
##########################################################
###CALLING SWD FILES
##########################################################
  
occs<-list.files(paste0(inDir,"/","_swd"),".csv")
occFile<-paste0(bacK_Dir,"/",occs[[1]])
sp_name<-sub(bacK_Dir,"",occFile)
sp_name<-sub("/","",sp_name)
sp_name<-sub(".csv","",sp_name)

##########################################################

spData <- read.csv(occFile)
spData<-spData[,-c(1:3)]
spData_presence<-spData[which(spData$status==1),]

##########################################################
###SWD TO SPATIALPOINTDATAFRAME
##########################################################

coordinates(spData_presence)<-~lon+lat
crs(spData_presence)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

##########################################################
###CHOOSE VARIABLES USING NIPALS
##########################################################

source(paste0(src.dir,"/","choose_variables.R"))

var_cho<-nipals_by_specie(spData_presence)

current_clim_layer<-lapply(paste0(current_clim_dir,"/","/",paste0(var_cho,".tif")),raster)
current_clim_layer<-stack(current_clim_layer)

spData_presence<-spData_presence[,c("status",var_cho)]

##########################################################
###FORMATTING TO SDM PACKAGE FORMAT 
##########################################################

mod_Dir<-paste0(mod_Dir);if(!file.exists(mod_Dir)){dir.create(mod_Dir)}

sp_Dir<-paste0(mod_Dir,"/",sp_name);if(!file.exists(sp_Dir)){dir.create(sp_Dir)}
sp_Dir_Original<-sp_Dir
Eval_sp_Dir<-paste0(sp_Dir,"/","evaluation");if(!file.exists(Eval_sp_Dir)){dir.create(Eval_sp_Dir)}

#setwd(dir =sp_Dir )
current_Out_Dir<-paste0(sp_Dir,"/","current");if(!file.exists(current_Out_Dir)){dir.create(current_Out_Dir)}
#set.seed(1000)
##########################################################
if(!file.exists(paste0(sp_Dir,"/","sdm.sdm"))){
d <- sdm::sdmData(train=spData_presence,
                 #predictors=as.data.frame(spData_presence[,var_cho]),
                 predictors=current_clim_layer,
                    #bg=as.data.frame(spData_pseudo@coords))
                 bg=list(n=10000,method='gRandom',remove=TRUE)
                 );gc()
}else{
  
cat("Already modelled!","\n")  
}
##########################################################
###PERFORMING 5 SDMs ALGORITHMS
##########################################################
if(!file.exists(paste0(sp_Dir,"/","sdm.sdm"))){
m2 <- sdm(status~.,data=d,methods=c('rf','brt','svm','glm','maxent'),
          replicatin='cv',
          test.percent=20,
          n=5,
          var.selection=T,
          overwrite=F,
          nc=5);gc()

write.sdm(m2,paste0(sp_Dir,"/","sdm"))


}else{
  m2<-read.sdm(paste0(sp_Dir,"/","sdm.sdm"))
  
}
##########################################################
##########################################################
m2_eval<-getEvaluation(m2,opt=4, stat=c('AUC',
                                        'COR',
                                        'Deviance',
                                        'obs.prevalence',
                                        'threshold',
                                        'sensitivity',
                                        'specificity',
                                        'TSS',
                                        'Kappa',
                                        'NMI',
                                        'phi',
                                        'ppv',
                                        'npv',
                                        'ccr',
                                        'prevalence'
                                        ))

##########################################################
#GETTING EVALUATION INFO

mInfo<-getModelInfo(m2)

##########################################################
#NULL MODEL


if(!file.exists(paste0(Eval_sp_Dir,"/","metrics.csv"))){
source(paste0(src.dir,"/","_null_model.R"))
nAUC<-nullModel_calculate(spData_presence=spData_presence,current_clim_layer=current_clim_layer);gc()
}else{
  cat("Already modelled! OMMITING NULL MODEL","\n")  
  
}
##########################################################
###WRITING RASTER FILES TO PERFORM EVALUATION ANALYSIS
#########################################################

if(!file.exists(paste0(current_Out_Dir,"/","current.img"))){
  p2m <- predict(m2,newdata=current_clim_layer,filename=paste0(current_Out_Dir,"/","current.img"),nc=5,mean=T,overwrite=T);gc()
  names(p2m)<-unique(mInfo$method)
  
  }else{
  p2m<-stack(brick(paste0(current_Out_Dir,"/","current.img")))
  names(p2m)<-unique(mInfo$method)
}
if(!file.exists(paste0(current_Out_Dir,"/","current_all.img"))){
  
p2m_all <- predict(m2,newdata=current_clim_layer,filename=paste0(current_Out_Dir,"/","current_all.img"),nc=5,mean=F,overwrite=F);gc()
names(p2m_all)<-paste0(mInfo$method,"_",mInfo$modelID)

}else{
  p2m_all<-stack(brick(paste0(current_Out_Dir,"/","current_all.img")))
  names(p2m)<-unique(mInfo$method)
  
  names(p2m_all)<-paste0(mInfo$method,"_",mInfo$modelID)
  
}



##########################################################
###PERFORMING EVALUATION METRICS
######################################################## 
if(!file.exists(paste0(Eval_sp_Dir,"/","metrics.csv"))){
source(paste0(src.dir,"/","_evaluation.R"))
evaluation<-evaluation_function(m2_eval=m2_eval,mInfo=mInfo,nAUC=nAUC,p2m=p2m,p2m_all=p2m_all,current_Out_Dir=current_Out_Dir,cAUC_INC=F);gc()

write.csv(evaluation,paste0(Eval_sp_Dir,"/","metrics.csv"),quote=F,row.names=F)

}else{
  
  evaluation<-read.csv(paste0(Eval_sp_Dir,"/","metrics.csv"),header=T)
}

if(sum(evaluation$valid_model)==0){
source(paste0(src.dir,"/","_evaluation_replicates.R"))
models_to_ensemble_MOD<-as.character(evaluation$METHOD[which(evaluation$valid_model==1)])

#if(!file.exists(paste0(Eval_sp_Dir,"/","metrics.csv"))){
  


m2_rep<-m2_rep_function(mInfo)
models_to_ensemble<-as.numeric(m2_rep$modelID[which(m2_rep$valid_model==1)])

source(paste0(src.dir,"/","00_evaluation_PCA.R"))


#rm(p2m,p2m_all)
##########################################################
###ENSAMBLING APPROACHES
######################################################## 

#CURRENT
source(paste0(src.dir,"/","_evaluation_ens.R"))
sp_Dir<-current_Out_Dir

if(!file.exists(paste0(current_Out_Dir,"/","MCAA_FINAL_THR.tif"))){
x<-eval_to_ens(models_to_ensemble,m2_eval,sp_Dir,mInfo,m2_rep,p2m_all);gc()
}else{
  cat("Baseline already projected!","\n")
}

##########################################################
###DEFINING RCPs
######################################################## 
RCPs<-c("rcp26","rcp45","rcp60","rcp85")
PERIODS<-c("2020_2049","2040_2069")
GCMS<-c("bcc_csm1_1",
        "bcc_csm1_1_m",
        "cesm1_cam5",
        "csiro_mk3_6_0",
        "fio_esm",
        "gfdl_cm3",       
        "giss_e2_r",      
        "ipsl_cm5a_lr",
        "miroc_esm", 
        "miroc_esm_chem",
        "miroc_miroc5",
        "mohc_hadgem2_es",
        "mri_cgcm3",
        "ncar_ccsm4",
        "ncc_noresm1_m",
        "nimr_hadgem2_ao"
)
  
grid_dir<-"V:/03_Future_data/downscaling_bsl_2_5min_v1"

# RCP<-RCPs[[1]]
# GCM<-GCMS[[1]]
# PERIOD<-PERIODS[[1]]

source(paste0(src.dir,"/","_projecting.R"))
source(paste0(src.dir,"/","mclapply2.R"))


##########################################################
###RUNNING FOR GCM AND PERIODS 3 cores|3 cores| 2 core
###USING 18 CORES!
######################################################## 
#rm(sp_Dir)
#sp_Dir_Original
if(!file.exists(paste0(sp_Dir_Original,"/","ensemble"))){
for(k in 1:length(RCPs)){
  
  cat("Projecting for: ",as.character(RCPs[[k]]),"\n")
  
mclapply2(1:length(GCMS),function(j){
  
  mclapply2(1:length(PERIODS),function(i){
  
    x<-projection_function(input_dir,RCP=RCPs[[k]],GCM=GCMS[[j]],PERIOD=PERIODS[[i]],var_cho,m2,out_dir,m2_rep,models_to_ensemble,m2_eval,mInfo,evaluation);gc()
    
    })
  }) 
};

}else{
  cat("Already projected!","\n")
}
##########################################################
###ENSEMBLING APPROACH PER RCP
######################################################## 
##########################################################
###RUNNING FOR GCM AND PERIODS 3 cores|3 cores| 2 core
###USING 18 CORES!
######################################################## 
if(!file.exists(paste0(sp_Dir_Original,"/","Graphics"))){
  
source(paste0(src.dir,"/","_ensemble_final.R"))

for(i in 1:length(RCPs)){
  
  cat("Projecting for: ",as.character(RCPs[[i]]),"\n")
  
  mclapply2(1:length(PERIODS),function(j){
    

x<-ensemble_RCP_function(mod_Dir,sp_name,sp_Dir_Original,RCP=RCPs[[i]],PERIOD=PERIODS[[j]],GCMS=GCMS)
  })
};rm(i)
}else{
  
  cat("Already ensembled!","\n")
}
##########################################################
###NO MIGRATION SCENARIO FOR RCP
######################################################## 
if(!file.exists(paste0(sp_Dir_Original,"/","Graphics"))){
  
source(paste0(src.dir,"/","_No_migration_Scenario.R"))

  for(k in 1:length(RCPs)){
    
    cat("Projecting for: ",as.character(RCPs[[k]]),"\n")
    
    mclapply2(1:length(GCMS),function(j){
      
      mclapply2(1:length(PERIODS),function(i){
        
       x<-No_migration_Scenario_function(mod_Dir,sp_name,sp_Dir_Original,RCP=RCPs[[k]],PERIOD=PERIODS[[i]],GCM=GCMS[[j]],models_to_ensemble_MOD,current_Out_Dir)
          
      })
    }) 
  };

source(paste0(src.dir,"/","_No_migration_Scenario_RCP.R"))


for(k in 1:length(RCPs)){
  
  cat("Projecting for: ",as.character(RCPs[[k]]),"\n")
  
  mclapply2(1:length(PERIODS),function(i){
  
x<-No_migration_RCP_function(mod_Dir,sp_name,sp_Dir_Original,RCP=RCPs[[k]],PERIOD=PERIODS[[i]],models_to_ensemble_MOD,current_Out_Dir)
  })
};rm(i) 
}else{
  
  cat("Already ensembled!","\n")
}
##########################################################
###CALCULATING STEPWISE

source(paste0(src.dir,"/","_stepwise.R"))

x<-STEPWISE_STEP_FUNCTION(sp_Dir_Original,current_Out_Dir,GCMS,PERIODS,RCPs,grid_dir,spData_presence)
  
########################################################## 
###AFFECTED AREAS BY CLIMATE CHANGE
graphics_dir<-paste0(sp_Dir_Original,"/","Graphics");if(!file.exists(graphics_dir)){dir.create(graphics_dir)}
source(paste0(src.dir,"/","_affected_areas.R"))
x<-afffected_areas_function(graphics_dir,current_Out_Dir,ens_dir,RCPs,GCMS,PERIOD,grid_dir,var_cho,sp_Dir_Original)
  
source(paste0(src.dir,"/","_rcp_ensemble_affected_areas.R"))
x<-RCP_ENSEMBLE_AFFECTED_AREAS(sp_Dir_Original,RCPs,PERIODS,current_Out_Dir)

########################################################## 
###FINAL MAPS
sp_name2<-"Brachiaria decumbens"
countries<-c("Colombia", "Ecuador", "Peru","Venezuela","Brazil","Bolivia")

source(paste0(src.dir,"/","_maps.R"))

x<-maps_function(current_Out_Dir,RCPs,GCMS,PERIODS,sp_name2,countries)
  
####

source(paste0(src.dir,"/","_maps_affected_areas.R"))

x<-maps_function_af_areas<-function(sp_Dir_Original,RCPs,GCMS,PERIODS,sp_name2,countries,current_Out_Dir) 
}else{
cat("NO VALID MODELS, FINISHING THE SCRIPT!")
}