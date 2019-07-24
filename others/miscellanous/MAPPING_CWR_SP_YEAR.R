require(countrycode);require(dismo);require(sp);require(maptools);require(ggplot2);require(ggmap); library(RgoogleMaps)
require(shapefiles)
# Load packages
library(lubridate)
suppressMessages(if(!require(dplyr)){install.packages("dplyr");library(dplyr)}else{library(dplyr)})
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
suppressMessages(if(!require(gtools)){install.packages("gtools");library(gtools)}else{library(gtools)})
suppressMessages(if(!require(googlesheets)){install.packages("googlesheets");library(googlesheets)}else{library(googlesheets)})
suppressMessages(if(!require(corrplot)){install.packages("corrplot");library(corrplot)}else{library(corrplot)})

################################

#---CALLING ISO3 AND SHAPEFILES
#data("wrld_simpl")
countrycode_data2<-countrycode_data

GAUL_SHP<-shapefile("U:/Input_data/_maps/GAUL_2014/G2014_2013_0_NO_ANT.shp")
GAUL_TABLE<-read.csv("U:/Input_data/_maps/GAUL_2014/GAUL.csv",header=T)
out_dir<-"U:/Input_data/_occurrence_data/_GBIF_data_2017_11_27"
GBIF_data<-read.csv("U:/Input_data/_occurrence_data/_GBIF_data_2017_11_27/GBIF_CLEANED_2017_11_27.csv",header=T)

#######################################
##---CALLING CIAT DATA
ciat <- gs_ls("Bean_landrace_name_table")
ciat <- gs_title("Bean_landrace_name_table")
ciat %>% gs_browse(ws = "Pvulgaris_CIATdb")
ciat <- ciat %>% gs_read(ws = "Pvulgaris_CIATdb")
nrow(ciat) # 37987 (old and original), 23831 (new one with vernacular names)



names(ciat) <- c("ID", "Source", "Cleaned.by", "Accession.number", "Synonyms", "Common.names",
                 "Interpreted.name.csosa", "AC.ACID", "To.use.ACID", "Common.name.ACID",
                 "Genepool.interpreted.ACID", "Genepool.literature.ACID","Race.interpreted.ACID",
                 "Race.literature.ACID", "Subgroup.interpreted.ACID", "Subgroup.literature.ACID",
                 "Reference.ACID", "TEST.vernacular", "Name.literature.vernacular",
                 "Genepool.literature.vernacular", "Race.interpreted.vernacular", "Race.literature.vernacular",
                 "Subgroup.literature.vernacular", "Reference.vernacular", "Genus", "Species", "Subspecies", "Variety",
                 "Biological.status", "Material.type", "CORE.collection", "Country", "Department", "County", "Place",
                 "Altitude", "Latitude", "Longitude", "Lat.geo", "Lon.geo", "coord.status", "Collection.date", "Name",
                 "Name2", "Institution", "Country3", "Receipt.date", "Growth.habit", "Seed.color",
                 "Seed.shape", "Seed.brightness", "Seed.weight", "Protein", "Genepool.WEIGHT.fix",
                 "Genepool.protein", "Race.protein", "Responsible11")

# ------------------------------------ #
# Update coordinates
# ------------------------------------ #

# Replace empty spaces with georreferenced coordinates
ciat <- ciat %>% filter(coord.status != "No coords") # 16038
ciat$Latitude[which(!is.na(ciat$Lat.geo) & is.na(ciat$Latitude))] <- ciat$Lat.geo[which(!is.na(ciat$Lat.geo) & is.na(ciat$Latitude))]
ciat$Longitude[which(!is.na(ciat$Lon.geo) & is.na(ciat$Longitude))] <- ciat$Lon.geo[which(!is.na(ciat$Lon.geo) & is.na(ciat$Longitude))]

ciat2<-ciat

coordinates(ciat2) = c('Longitude', 'Latitude')
proj4string(ciat2) = crs(GAUL_SHP)
ovr<-over(ciat2,GAUL_SHP)
ciat3<-as.data.frame(ciat2)

ciat3$OVR<-NA
ciat3$ISO3_OVR<-NA
ciat3$CONTINENT<-NA
ciat3$OVR<-ovr$ADM0_CODE
ciat3<-ciat3[which(!is.na(ciat3$OVR)),]
for(i in 1:nrow(GAUL_TABLE)){
  cat(i,"\n")
  ciat3$ISO3_OVR[which(ciat3$OVR==GAUL_TABLE$GAUL.1[[i]])]<-as.character(GAUL_TABLE$ISO3[[i]])
  
}
#######

for(i in 1:nrow(countrycode_data2)){
  cat(i,"\n")
  ciat3$CONTINENT[which(ciat3$ISO3_OVR==countrycode_data2$iso3c[[i]])] <-countrycode_data2$continent[[i]]
}
ciat4<-ciat3[which(ciat3$CONTINENT=="Americas"),]
ciat4$Collection.date[which(ciat4$Collection.date==0)]<-NA

ciat4$year<-NA

for(i in 1:nrow(ciat4)){
  cat(i,"\n")
  #as.Date(gsub("-","/",ciat4$Collection.date[[1]]),"modern")
  if(length(grep("-",ciat4$Collection.date[[i]]))>0){
    ciat4$Collection.date[[i]]<-as.character(as.Date(dmy(as.character(ciat4$Collection.date[[i]]))))
  
}else{
  ciat4$Collection.date[[i]]<-as.character(as.Date(as.numeric(ciat4$Collection.date[[i]]), origin = "1899-12-30"))
    }
}

ciat4$year<-year(ciat4$Collection.date)
######################################
###--CALLING GRIN DATA
OSys <- Sys.info(); OSys <- OSys[names(OSys)=="sysname"]
if(OSys == "Linux"){ root <- "/mnt/workspace_cluster_9" } else {
  if(OSys == "Windows"){ root <- "//dapadfs/Workspace_cluster_9" }
}; rm(OSys)


GRIN<- readRDS(paste0(root, "/gap_analysis_landraces/Results/_occurrences_datasets/GRINda_all.RDS"))
GRIN<-GRIN[which(!is.na(GRIN$Longitude)),]
#
GRIN2<-GRIN

coordinates(GRIN2) = c('Longitude', 'Latitude')
proj4string(GRIN2) = crs(GAUL_SHP)
ovr2<-over(GRIN2,GAUL_SHP)
GRIN3<-as.data.frame(GRIN2)

GRIN3$OVR<-NA
GRIN3$ISO3_OVR<-NA
GRIN3$CONTINENT<-NA
GRIN3$OVR<-ovr2$ADM0_CODE
GRIN3<-GRIN3[which(!is.na(GRIN3$OVR)),]
##

for(i in 1:nrow(GAUL_TABLE)){
  cat(i,"\n")
  GRIN3$ISO3_OVR[which(GRIN3$OVR==GAUL_TABLE$GAUL.1[[i]])]<-as.character(GAUL_TABLE$ISO3[[i]])
  
}
#######

for(i in 1:nrow(countrycode_data2)){
  cat(i,"\n")
  GRIN3$CONTINENT[which(GRIN3$ISO3_OVR==countrycode_data2$iso3c[[i]])] <-countrycode_data2$continent[[i]]
}
GRIN4<-GRIN3[which(GRIN3$CONTINENT=="Americas"),]
GRIN4$year<-NA
GRIN4$year<-year(GRIN4$Source.Date)
############################################
##--GBIF--#
GBIF_data$CONTINENT_CODE<-NA
GBIF_data<-GBIF_data[which(GBIF_data$hasGeospatialIssues==FALSE),]


for(i in 1:nrow(countrycode_data2)){
  cat(i,"\n")
  GBIF_data$CONTINENT_CODE[which(GBIF_data$ISO3==countrycode_data2$iso3c[[i]])] <-countrycode_data2$continent[[i]]
}


#GBIF_data2<-GBIF_data
GBIF_data2<-GBIF_data[which(GBIF_data$CONTINENT_CODE=="Americas"),]


#############################


GBIF_FINAL<-as.data.frame(cbind(as.character(GBIF_data2$gbifID),GBIF_data2$decimalLongitude,GBIF_data2$decimalLatitude,as.character(GBIF_data2$ISO3),GBIF_data2$year))
GBIF_FINAL$SOURCE<-NA;GBIF_FINAL$SOURCE<-"GBIF"
colnames(GBIF_FINAL)<-c("ID","LONGITUDE","LATITUDE","ISO3","YEAR","SOURCE")

GBIF_FINAL_OPT<-GBIF_FINAL[which(!is.na(GBIF_FINAL$YEAR)),]

GBIF_FINAL_OPT2<-GBIF_FINAL_OPT
GBIF_FINAL_OPT2$YEAR<-as.numeric(as.character(GBIF_FINAL_OPT2$YEAR))
GBIF_FINAL_OPT2<-  GBIF_FINAL_OPT2[which(GBIF_FINAL_OPT2$YEAR<=2000),]
CIAT_FINAL<-as.data.frame(cbind(as.character(ciat4$ID),ciat4$Longitude,ciat4$Latitude,as.character(ciat4$ISO3_OVR),ciat4$year))
CIAT_FINAL$SOURCE<-NA;CIAT_FINAL$SOURCE<-"CIAT"
colnames(CIAT_FINAL)<-c("ID","LONGITUDE","LATITUDE","ISO3","YEAR","SOURCE")

USDA_FINAL<-as.data.frame(cbind(as.character(GRIN4$Accession),GRIN4$Longitude,GRIN4$Latitude,as.character(GRIN4$ISO3_OVR),GRIN4$year))
USDA_FINAL$SOURCE<-NA;USDA_FINAL$SOURCE<-"USDA"
colnames(USDA_FINAL)<-c("ID","LONGITUDE","LATITUDE","ISO3","YEAR","SOURCE")


data_final<-as.data.frame(rbind(GBIF_FINAL,CIAT_FINAL,USDA_FINAL))
data_final$YEAR<-as.numeric(as.character(data_final$YEAR))
#data_final<-data_final[which(!is.na(data_final$YEAR)),]
data_final$LATITUDE<-as.numeric(as.character(data_final$LATITUDE))
data_final$LONGITUDE<-as.numeric(as.character(data_final$LONGITUDE))
data_final2<-data_final

data_final<-data_final[which(!is.na(data_final$YEAR)),]
########
data_final3<-as.data.frame(rbind(GBIF_FINAL_OPT,CIAT_FINAL,USDA_FINAL))
data_final3$YEAR<-as.numeric(as.character(data_final3$YEAR))
#data_final<-data_final[which(!is.na(data_final$YEAR)),]
data_final3$LATITUDE<-as.numeric(as.character(data_final3$LATITUDE))
data_final3$LONGITUDE<-as.numeric(as.character(data_final3$LONGITUDE))
########
data_final4<-as.data.frame(rbind(GBIF_FINAL_OPT2,CIAT_FINAL,USDA_FINAL))
data_final4$YEAR<-as.numeric(as.character(data_final4$YEAR))
#data_final<-data_final[which(!is.na(data_final$YEAR)),]
data_final4$LATITUDE<-as.numeric(as.character(data_final4$LATITUDE))
data_final4$LONGITUDE<-as.numeric(as.character(data_final4$LONGITUDE))

  
years<-c(1950,1960,1970,1980,1990,2000)
####
for(i in 1:length(years)){
x<-data_final[data_final$YEAR<=years[[i]],]



if(nrow(x)>1 & !is.null(nrow(x))){
  
  
  xmin<-min(x$LONGITUDE,na.rm=T)
  xmax<-max(x$LONGITUDE,na.rm=T)
  
  ymin<-min(x$LATITUDE,na.rm=T)
  ymax<-max(x$LATITUDE,na.rm=T)
  
}else if(is.null(nrow(x))){
  
  xmin<-min(x$LONGITUDE,na.rm=T)-5
  xmax<-max(x$LONGITUDE,na.rm=T)+5
  
  ymin<-min(x$LATITUDE,na.rm=T)-5
  ymax<-max(x$LATITUDE,na.rm=T)+5 
}else if(nrow(x)==1){
  
  xmin<-min(x$LONGITUDE,na.rm=T)-5
  xmax<-max(x$LONGITUDE,na.rm=T)+5
  
  ymin<-min(x$LATITUDE,na.rm=T)-5
  ymax<-max(x$LATITUDE,na.rm=T)+5 
}  else if(nrow(x)==1){
  xmin<-min(x$LONGITUDE,na.rm=T)-5
  xmax<-max(x$LONGITUDE,na.rm=T)+5
  
  ymin<-min(x$LATITUDE,na.rm=T)-5
  ymax<-max(x$LATITUDE,na.rm=T)+5 
  
}


MaxZoom2<-MaxZoom(c(ymin,ymax),c(xmin,xmax))

# if(MaxZoom2<3){
#   MaxZoom2=3
# }

center <- rev(sapply(as.data.frame(x[,c("LONGITUDE","LATITUDE")]), mean,na.rm=T))
center<-rev(center)

ss<-get_map(location=center, zoom = MaxZoom2, maptype='road', source='google',crop=TRUE,color='bw')


#(expression("Tx by Yr\nOlder (\u2265 18 Years)")
map1 <- ggmap(ss, extent='panel')#, base_layer=ggplot(data=temp_dt2,aes(x=long, y=lat)))
# map1<-map1+geom_point(color="black",size=0.5)
map1<-map1+geom_point(aes(x=LONGITUDE, y=LATITUDE,col=SOURCE),size=1.2,stroke = 1,data=x, show.legend = TRUE,shape=17)#Alt + 9733  
map1<-map1+labs(x ="Longitude", y="Latitude")
map1<-map1+ggtitle(paste0("<=",years[[i]]," | ",nrow(x)," records"))
map1<-map1+ theme(axis.text=element_text(size=10),axis.title=element_text(size=10),legend.position="bottom",legend.title=element_blank())#+
  #scale_fill_manual(name="Taxon occurrences",values="black") # no title) 
 # scale_colour_manual(name="Taxon occurrences",values="black")

#map1<-map1+scale_color_manual(values = c("Occurrences" = 'black')) + 
#  scale_shape_manual(values = c(taxon3 = 17))

# ggsave(map1,)
if(length(unique(na.omit(x$CONTINENT_CODE)))==1){
  
  ggsave(paste0(out_dir,"/AMERICAS/","AM_",years[[i]],"_",Sys.Date(),".pdf"),units="in",width=4,height=4.8,scale=2,dpi=600)
  
}else{
  
  ggsave(paste0(out_dir,"/AMERICAS/",years[[i]],"_",Sys.Date(),".pdf"),units="in",width=4,height=4.8,scale=2,dpi=600)
  
    }
}
#########
#############################

#LOAD USDA and CIAT data
# 
# 
# mask<-raster("U:/Input_data/mask_wb_c_ant_AMERICAS.tif")
# mask2<-mask
# mask2[]<-1:ncell(mask2)
# mask2[which(is.na(mask[]))]<-NA
# 
# data_final2<-data_final
# 
# coordinates(data_final2)<-~LONGITUDE+LATITUDE
# crs(data_final2)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# data_final$COORD<-NA
# data_final$COORD<-raster::extract(mask2,data_final2)
# data_final$unique<-NA
# sum<-as.data.frame(table(data_final$COORD))
# sum<-sum[order(sum$Freq,decreasing = T),]
# sum2<-sum
# sum2<-sum2[which(sum2$Freq==1),]
# for(i in 1:nrow(sum2)){
#   
#   
#   data_final$unique[which(data_final$COORD==sum2$Var1[[i]])]<-1
#   
#   
# }
write.csv(data_final,paste0("U:/Input_data/_occurrence_data/_GBIF_data_2017_11_27/AMERICAS/DATA_CLEANED_ONLY_YEAR.csv"),row.names=F,quote=F,na="")
write.csv(data_final2,paste0("U:/Input_data/_occurrence_data/_GBIF_data_2017_11_27/AMERICAS/DATA_CLEANED_ALL_YEAR.csv"),row.names=F,quote=F,na="")
write.csv(data_final3,paste0("U:/Input_data/_occurrence_data/_GBIF_data_2017_11_27/AMERICAS/DATA_CLEANED_GBIF_YEAR.csv"),row.names=F,quote=F,na="")
write.csv(data_final4,paste0("U:/Input_data/_occurrence_data/_GBIF_data_2017_11_27/AMERICAS/DATA_CLEANED_GBIF_YEAR2000.csv"),row.names=F,quote=F,na="")

###########

##############
dat_fin_list<-list(data_final,data_final2,data_final3,data_final4)
labs<-c("Using only year","Year and not year available","GBIF with year","GBIF records before 2000")
for(i in 1:length(dat_fin_list)){
x<-dat_fin_list[[i]]



if(nrow(x)>1 & !is.null(nrow(x))){
  
  
  xmin<-min(x$LONGITUDE,na.rm=T)
  xmax<-max(x$LONGITUDE,na.rm=T)
  
  ymin<-min(x$LATITUDE,na.rm=T)
  ymax<-max(x$LATITUDE,na.rm=T)
  
}else if(is.null(nrow(x))){
  
  xmin<-min(x$LONGITUDE,na.rm=T)-5
  xmax<-max(x$LONGITUDE,na.rm=T)+5
  
  ymin<-min(x$LATITUDE,na.rm=T)-5
  ymax<-max(x$LATITUDE,na.rm=T)+5 
}else if(nrow(x)==1){
  
  xmin<-min(x$LONGITUDE,na.rm=T)-5
  xmax<-max(x$LONGITUDE,na.rm=T)+5
  
  ymin<-min(x$LATITUDE,na.rm=T)-5
  ymax<-max(x$LATITUDE,na.rm=T)+5 
}  else if(nrow(x)==1){
  xmin<-min(x$LONGITUDE,na.rm=T)-5
  xmax<-max(x$LONGITUDE,na.rm=T)+5
  
  ymin<-min(x$LATITUDE,na.rm=T)-5
  ymax<-max(x$LATITUDE,na.rm=T)+5 
  
}


MaxZoom2<-MaxZoom(c(ymin,ymax),c(xmin,xmax))

# if(MaxZoom2<3){
#   MaxZoom2=3
# }

center <- rev(sapply(as.data.frame(x[,c("LONGITUDE","LATITUDE")]), mean,na.rm=T))
center<-rev(center)

ss<-get_map(location=center, zoom = MaxZoom2, maptype='road', source='google',crop=TRUE,color='bw')


#(expression("Tx by Yr\nOlder (\u2265 18 Years)")
map1 <- ggmap(ss, extent='panel')#, base_layer=ggplot(data=temp_dt2,aes(x=long, y=lat)))
# map1<-map1+geom_point(color="black",size=0.5)
map1<-map1+geom_point(aes(x=LONGITUDE, y=LATITUDE,col=SOURCE),size=1.2,stroke = 1,data=x, show.legend = TRUE,shape=17)#Alt + 9733  
map1<-map1+labs(x ="Longitude", y="Latitude")
map1<-map1+ggtitle(paste0(labs[[i]]," | ",nrow(x)," records"))
map1<-map1+ theme(axis.text=element_text(size=10),axis.title=element_text(size=10),legend.position="bottom",legend.title=element_blank())#+

ggsave(paste0(out_dir,"/AMERICAS/",labs[[i]],"_",Sys.Date(),".pdf"),units="in",width=4,height=4.8,scale=2,dpi=600)
}