require(countrycode);require(dismo);require(sp);require(maptools)
data("wrld_simpl")
countrycode_data2<-countrycode_data
GAUL_TABLE<-read.csv('D:/BU/ADMIN/GAUL_2014/gaul_2014/GAUL.csv')
GAUL_SHP<-shapefile("D:/BU/ADMIN/GAUL_2014/gaul_2014/G2014_2013_0.shp")

GBIF<-read.csv('D:/GBIF_PHASEOLUS/GBIF_2017_11_27.csv',header=T,sep=",");gc()

GBIF$ISO3<-NA
GBIF$ISO3_MATCH<-NA

library(sp)
coordinates(GBIF) <- ~decimalLongitude+decimalLatitude
crs(GBIF) <- crs(GAUL_SHP)

ovr <- over(GBIF, GAUL_SHP);gc()
ovr$ISO3_GAUL<-NA




  
  for(j in 1:nrow(countrycode_data2)){
    cat(j,"\n")
  GBIF$ISO3[which(GBIF$countryCode==countrycode_data2$iso2c[[j]])]<-countrycode_data2$iso3c[[j]]
  }


for(j in 1:nrow(GAUL_TABLE)){
  cat(j,"\n")
  ovr$ISO3_GAUL[which(ovr$ADM0_CODE==GAUL_TABLE$GAUL.1[[j]])]<-as.character(GAUL_TABLE$ISO3[[j]])
}

GBIF$ISO3_GAUL<-ovr$ISO3_GAUL


GBIF2<-GBIF[which(!is.na(GBIF$ISO3_GAUL)),]

for(i in 1:nrow(GBIF2)){
  
  cat(i," of ",nrow(GBIF2),"\n")
 
  if(is.na(GBIF2$ISO3[[i]]) & !is.na(GBIF2$ISO3_GAUL[[i]])){
    GBIF2$ISO3_MATCH[[i]]<-1
    
  }else if(GBIF2$ISO3[[i]]==GBIF2$ISO3_GAUL[[i]]){
   
   GBIF2$ISO3_MATCH[[i]]<-1
 } else{
   GBIF2$ISO3_MATCH[[i]]<-0
   
 }
}

GBIF3<-GBIF2
GBIF3<-GBIF3[which(GBIF3$ISO3_MATCH==1),]
plot(wrld_simpl);points(GBIF3,pch=16,col="red")


write.csv(GBIF3,'D:/GBIF_PHASEOLUS/GBIF_CLEANED_2017_11_27.csv',row.names = F,na = "")
