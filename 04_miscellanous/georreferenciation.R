############################# Georreferenciation ####################################
## Different ways to georreference accessions with missing coordinates data #########
## 1) GEOLocate: Function based on the GEOLocate software. Translate textual locality descriptions 
##    associated with biodiversity collections data into geographic coordinates. Rios N, (2018), Yale University.
## 2) Harvesine: To calculate the Harvesine distance between two pair of coordinates (How different are they).
##    Just to compare a traditional georreferenciation method and a new one.
## 3) Geocode: There are two packages which use geocode services based on Google Maps API: dismo and ggmap.
##    Dismo allows to calculate the incertainty of the results
## NOTE: The most accurate and used is GEOLocate (https://www.geo-locate.org/web/default.html)
#####################################################################################

## By: María Victoria Diaz L.
## CIAT (2019)

################################################## GEOLocate #########################################################################

library(RJSONIO)
library(RCurl)

setwd("D:/OneDrive - CGIAR/Desktop/taro_data")

input_csv="D:/OneDrive - CGIAR/Desktop/taro_data/Taro_2019_LatLong_20190619_togeoref.csv"
raw_output="out.csv"
first_final_output="out_first.csv"

OPTIONS="&doduncert=true&dopoly=false&displacepoly=false"
In= read.csv(input_csv)

numRuns = 0
recordCounter = 0
for (k in 1:nrow(In)){
  print(k)
  Sys.sleep(3) #be nice and pause a few seconds between requests	
  Country=In[k,]$Country; Country = stringr::str_to_title(Country)
  Locality=In[k,]$Locality; Locality = stringr::str_to_title(Locality)
  StateProvince=In[k,]$StateProvince; StateProvince = stringr::str_to_title(StateProvince)
  County=In[k,]$County; County = stringr::str_to_title(County)

  q=paste("http://geo-locate.org/webservices/geolocatesvcv2/glcwrap.aspx?country=",Country,"&locality=",Locality,"&state=",StateProvince,"&county=",County,OPTIONS, sep='')
  q=gsub(' ','%20',q)
  
  tryCatch({
    JSONresponse = basicTextGatherer()
    curlPerform(url = q, writefunction = JSONresponse$update)
    RecNum = k
    glc = fromJSON(JSONresponse$value())
    numresults = glc$numResults
    if (numresults > 0){ 
      for (i in 1:numresults) {
        Rank  = i
        Longitude = glc$resultSet$features[[i]]$geometry$coordinates[1]
        Latitude = glc$resultSet$features[[i]]$geometry$coordinates[2]
        Precision = glc$resultSet$features[[i]]$properties$precision
        Score = glc$resultSet$features[[i]]$properties$score
        Parsepattern = glc$resultSet$features[[i]]$properties$parsePattern
        Uncert = glc$resultSet$features[[i]]$properties$uncertaintyRadiusMeters
        Poly = glc$resultSet$features[[i]]$properties$uncertaintyPolygon
        #if a polygon is present reformat coordinates to geolocate format-a comma delimited array
        if ("coordinates"%in%names(Poly)){
          sPoly = ''
          for (v in 1:length(Poly$coordinates[[1]][])){
            vLon=format(Poly$coordinates[[1]][[v]][1])
            vLat=format(Poly$coordinates[[1]][[v]][2])
            sPoly  = paste(sPoly,vLat, vLon, sep=',')
          }
          # Strip the leading commas
          sPoly=sub("^,+", "", sPoly)
          Poly=sPoly
        }
        df = data.frame(RecNum,Rank,Latitude,Longitude,Precision,Score,Parsepattern,Uncert,Poly,replace(In[k,], is.na(In[k,]),""))
        recordCounter = recordCounter + 1
        if (recordCounter==1)
          write.table(x=df, file=raw_output, append=FALSE, row.names=FALSE, col.names=TRUE,  quote=TRUE, sep=',', qmethod="double") else
            write.table(x=df, file=raw_output, append=TRUE,  row.names=FALSE, col.names=FALSE, quote=TRUE, sep=',', qmethod="double")
      }
    } else {
      glcRank  = 1
      glcLongitude = NA
      glcLatitude = NA
      glcPrecision = NA
      glcScore = NA
      glcParsepattern = NA
      glcUncert = NA
      glcPoly = NA
      df = data.frame(glcRecNum,glcRank,glcLatitude,glcLongitude,glcPrecision,glcScore,glcParsepattern,glcUncert,glcPoly,replace(glcIn[k,], is.na(glcIn[k,]),""))
      recordCounter = recordCounter + 1
      if (recordCounter==1)
        write.table(x=df, file=raw_output, append=FALSE, row.names=FALSE, col.names=TRUE,  quote=TRUE, sep=',') else
          write.table(x=df, file=raw_output, append=TRUE,  row.names=FALSE, col.names=FALSE, quote=TRUE, sep=',')	
    }
  },error = function(err) 
  {
    Rank  = 0
    Longitude = NA
    Latitude = NA
    Precision = "ERROR GETTING JSON"
    Score = 0
    Parsepattern = NA
    Uncert = NA
    Poly = NA
    df = data.frame(RecNum,Rank,Latitude,Longitude,Precision,Score,Parsepattern,Uncert,Poly,replace(In[k,], is.na(In[k,]),""))
    recordCounter = recordCounter + 1
    if (recordCounter==1)
      write.table(x=df, file=raw_output, append=FALSE, row.names=FALSE, col.names=TRUE,  quote=TRUE, sep=',') else
        write.table(x=df, file=raw_output, append=TRUE,  row.names=FALSE, col.names=FALSE, quote=TRUE, sep=',')  
    
  })
}

Filtered=read.csv(raw_output)
Filtered=Filtered[Filtered$Rank==1,]
write.table(x=Filtered, file=first_final_output, append=FALSE, row.names=FALSE, col.names=TRUE,  quote=TRUE, sep=',')

#freeing resources
rm(list=ls())


########################################### Haversine Distance #############################################


base<-read.csv("D:/OneDrive - CGIAR/Desktop/taro_data/Taro_2019_LatLong_20190619_togeoref.csv")

library(pracma)
library(geosphere)
library(microbenchmark)
library(data.table)

#x<-data.frame();distc<-c()
#dtHaversine<-c(); haversine<-c(); geo<-c()
dt1 <- copy(base); dt2 <- copy(base); dt3 <- copy(base)
dt1$dist<-NA; dt2$dist<-NA; dt3$dist<-NA

for(i in 1:nrow(base)){

  #distc<-haversine(original,geolocate , R = 6371.0)
  x<-data.frame(longitude=base$longitude[i],latitude=base$latitude[i],Long=base$Longitude[i],Lat=base$Latitude[i] )
  original<-c(x$longitude,x$latitude)
  geolocate<-c(x$Long, x$Lat)
  #distc[i]<-geosphere::distHaversine(original,geolocate, r=6378137) #The shortest distance between two points (i.e., the 'great-circle-distance' or 'as the crow flies')
 
dt2$dist[i] = distHaversine(matrix(original, ncol = 2),matrix(geolocate, ncol = 2))/1000
    
dt3$dist[i] = distGeo(matrix(original, ncol = 2),  matrix(geolocate, ncol = 2))/1000 #Esta casi no se usa
    
}

base<-data.frame(base,Harve = dt2$dist, Geo = dt3$dist)
base<-base[,-5]
write.csv(base, "C:/Users/mvdiaz/Desktop/coord_verification.csv")



########################################### Geocode ########################################################


base<-read.csv("D:/OneDrive - CGIAR/Desktop/taro_data/Book11.csv")
base<- base[, "address"]; base<-as.data.frame(base)


## 1 ##

library(sp)
library(raster)
library(XML)
library(dismo)

uncertainty <- 10000 # in meters (= 10km)


for(j in 1:nrow(base)){
  #for(j in 778:nrow(unique.Geodata)){
  Sys.sleep(0.5) # wait 0.5 seconds
  b <- dismo::geocode(base$address[j])
  print(j)
  b2 <- subset(b, b$uncertainty == min(b$uncertainty)) # select record with least uncertainty
  if(dim(b2)[1] == 0){
    base[j, 'LatitudeDD'] <- base[j, 'LatitudeDD'] #; print(1)
    base[j, 'LongitudeDD'] <- base[j, 'LongitudeDD'] #; print('a')  
  } else {
    b2 <- b2[1,] # sometimes 2 same minimum values i.e. geocode('Austria,Niederoesterreich,Weinviertel,Katzelsdorf')
    if(b2[, 'uncertainty'] < uncertainty) {
      base[j, 'LatitudeDD'] <- b2[, 'latitude']
      base[j, 'LongitudeDD'] <- b2[, 'longitude']
      base[j, 'coordUncertaintyM'] <- b2[, 'uncertainty']
    } else {
      base[j, 'LatitudeDD'] <- base[j, 'LatitudeDD']
      base[j, 'LongitudeDD'] <- base[j, 'LongitudeDD']
    }
  }
}



## 2 ##

library(ggmap)

geocodi_base<-lapply(1:length(base), function(i){
  
  tryCatch({
    cat(i, "\n")
    result <- ggmap::geocode(as.character(base[i,1]), output = "latlona", source = "google")
    H=cbind(result[[1]],result[[2]],result[[3]])
    return(H)
    rm(result)
    Sys.sleep(3)
    
  },error = function(e) {
    print(paste0("Error ",base[i,1]))
    message = e
    status = FALSE
    
  }, finally = {
    
    print(paste0("End ",base[i,1]))
    
  })
  
})
bas<-do.call(rbind, geocodi_base)
bas<-as.data.frame(bas)
write.csv(bas, "C:/Users/mvdiaz/Desktop/ggmap.csv")




