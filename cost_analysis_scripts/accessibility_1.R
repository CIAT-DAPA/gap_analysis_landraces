### Author: Andres Camilo Mendez Alzate
### Script to process all of the raster files considered importants to make an accessibility analysis.


# upload world roads rasters from openstreetmaps to merge in a single file

require(raster)
am<-raster("Z:/Input_data/_maps/_openstreetmaps/shapefiles/rasters/AM_R.tif")
af<-raster("Z:/Input_data/_maps/_openstreetmaps/shapefiles/rasters/AF_R.tif")
eu<-raster("Z:/Input_data/_maps/_openstreetmaps/shapefiles/rasters/EUR_R.tif")
oc<-raster("Z:/Input_data/_maps/_openstreetmaps/shapefiles/rasters/OCE_R.tif")

ot<- raster("")

plot(eu)
plot(oc)
plot(af)
plot(am)
# If one or more raster has problems whit the value of 0, then...
ncell(altitude)
eu[which(eu[]==0)]<-NA
oc[which(oc[]==0)]<-NA
af[which(af[]==0)]<-NA
am[which(am[]==0)]<-NA

# Stack all raster files 
wlrd<-stack(am,af,eu,oc)

# Merge  stacked raster files
sum_wrld <- mosaic(am,af,eu,oc, fun = min )
plot(sum_wrld)
unique(sum_wrld)
# Save world's roads rasters 
writeRaster(sum_wrld,"Z:/Input_data/_maps/_openstreetmaps/world_roads.tif", overwrite = TRUE )

# Reclassifiying worl's roads raster into base speed (mins per kilometers)

wrld_rec <- sum_wrld

c <- data.frame(hcode = 1:13, spped = c(0.5,1,6,6,6,0.5,1,6,6,1,1,6,6))
for(i in 1:nrow(c)){
  
  wrld_rec[ which(wrld_rec[] == c[i,1] )] <- c[i,2]
  
}
unique(wrld_rec[])
writeRaster(wrld_rec,"Z:/Input_data/_maps/_openstreetmaps/world_roads_reclass.tif", overwrite = TRUE )


###### very important 

highway = 'motorway' OR highway = 'primary' OR highway = 'secondary' OR highway = 'tertiary' OR highway = 'track' OR highway = 'motorway_link' OR highway = 'primary_link' OR highway = 'secondary_link' OR highway = 'tertiary_link' OR highway = 'trunk' OR highway = 'trunk_link' OR highway = 'road' 

#####################

# Upload the mask to fit or correct any raster
# Is important the fact that any raster must have the same extent and resolution equal to the mask
wmask <- raster("Z:/Input_data/mask_wb_c_ant.tif")


####### import all necesary raster to create the friction surface.

#  Import Altidude raster
altitude<- raster("Z:/Input_data/_maps/_altitude_and_slope/Altitude.tif")
extent(altitude)

# Compute the slope raster from altitude raster
slope<-terrain(altitude, opt = "slope", unit = "tangent")

# verify if the extent and resolution are equal to mask
extent(wmask)
extent(slope)

writeRaster(slope, "Z:/Input_data/_maps/_altitude_and_slope/Slope_tangent.tif" , format = "GTiff" , overwrite = T )


# Import world road network raster (there is tow raster the first come from Groads-1997, and the second one come form Openstreetmaps-2016 (use this))

roads <- raster("Z:/Input_data/_maps/_Groads/groads_fclass3.tif") # raster from Groads
roads <- raster("Z:/Input_data/_maps/_openstreetmaps/world_roads_reclass.tif") # raster from Openstreetmaps (Use this)
extent(roads)

# Import rail network reclassified
rails <- raster("Z:/Input_data/_maps/_railroads/railroads_raster_reclasiffied/railroads_raster_reclass.tif")
extent(rails)

# Import Land cover raster reclassified
land_co <- raster("Z:/Input_data/_maps/land_Cover/cover_reclassified.tif")
# If the extend of one or more raster are different then you would think to do a raster crop. For this purpose you must use a wmask.

land_co <- raster::crop(land_co, extent(wmask) ) 

# Import Navigable Rivers reclassified raster

n.rivers <- raster("Z:/Input_data/_friction_surface/datasets/Rivers_sf.tif")
extent(n.rivers)

# Rasterize the presence coordinates for beans accessions and save them

beans <- read.csv('Z:/Input_data/_maps/finalCoords.csv')
beans <- data.frame(beans$Longitude , beans$Latitude)
beans_s <- SpatialPoints(beans, proj4string = CRS(proj4string(land_co)) )

beans_r <- rasterize(beans_s, wmask  )
beans_r[!is.na(beans_r)] <- 1


writeRaster(beans_r, "Z:/Input_data/_maps/_beans_presence_rasters/presence_raster.tif" , format = "GTiff" , overwrite = T )


# Build the walk speed raster using the Tobler's walking formula or  van Wagtendonk and Benedict formula

spl_w <- 0.6 * exp(3.5 * abs(slope + 0.05))    #Tobler's walking     
spl_w2 <- 5 * exp(-3 * slope)  #  van Wagtendonk and Benedict

#dividir sobre 12
spl_f <- spl_w/12
hist(spl_f[])

writeRaster(spl_w,"Z:/Input_data/_maps/_walk_speed/pace_factor.tif",overwrite = T)
writeRaster(spl_w2,"Z:/Input_data/_maps/_walk_speed/pace_factor2.tif",overwrite = T)

# The walk speed is calculated  multiplying the land cover reclassify by the walk speed factor

walk_spd <- land_co * spl_w
  
writeRaster(walk_spd,"Z:/Input_data/_maps/_walk_speed/walk_spd.tif",overwrite = T)

# Merge all raster files to create the fricctions surface

merge_spd <- raster::merge(roads, rails, n.rivers ,walk_spd)
       
#merge_spd2<- min(stack(roads, rails, walk_spd),na.rm=T)

writeRaster(merge_spd, "Z:/Input_data/_friction_surface/frsurface_5.tif" , format = "GTiff" , overwrite = T )

# Reclasify the world roads raster


w_roads <- raster("Z:/Input_data/_maps/_openstreetmaps/world_roads.tif")

# At Last, the cost distance (accessibility) analysis was carried out in ArcMap software.

x1<- sample(1:1000,15)
x2<- sample(1:1000,70)


## Comparision between both walk's speed formulas Tolber and Von Wagtendonk
xdf <- seq(-60,60,1)


plot(xdf, (5 * exp(-3 * (tan(xdf*pi/180) ))), type="b", main = "Slope speed factor", ylab = "Pace formula  (Min/Km)" , xlab = "Slope" )
lines(xdf, (0.6 * exp(3.5 * abs(tan(xdf*pi/180)+0.05 ))) , col = "red", type = "b" )


### Fix some errors in openstreetmaps roads rasters
url<- "Z:/Input_data/_maps/_openstreetmaps/shapefiles/rasters/AM_C_R.tif"
aust <- raster(url)
aust[which(aust[] == 0)] <- NA
plot(aust)
writeRaster(aust, "url" , format = "GTiff" , overwrite = T )                             




require(sf)

wmask <- raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/mask_wb_c_ant.tif")
sp <- read_sf("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_maps/_openstreetmaps/shapefiles/american_roads.shp")

r <- raster()

mrast <- rasterize(sp, r, fiel="highway", mask = wmask)


