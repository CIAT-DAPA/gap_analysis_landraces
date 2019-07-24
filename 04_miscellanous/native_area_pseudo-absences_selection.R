suppressMessages(library(raster))

# Preprocess ELU map
# Load ELU map
elu <- raster::raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/ecosystems/globalelu/World_ELU_2015.tif")
# Load mask map
msk <- raster::raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask/mask_world.tif")
# Define same resolution for mask and ELU maps
elu5 <- raster::resample(elu, msk, method = "ngb")
# Save resampled ELU map
raster::writeRaster(elu5, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/ecosystems/globalelu/World_ELU_2015_5km.tif")
rm(msk, elu, elu5)

# Creating composite native area mask using occurrences per species
# Load occurrences
occ <- raster::shapefile("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/potato/lvl_1/ajanhuiri/americas/occurrences/Occ.shp")
occ <- raster::shapefile("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/potato/lvl_1/tuberosum_andigenum/americas/occurrences/Occ.shp")
# Load ELU map 5 km
elu <- raster::raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/ecosystems/globalelu/World_ELU_2015_5km.tif")

elu_vals <- raster::extract(x = elu, y = occ@coords)
sort(table(elu_vals))
elu_vals2 <- sort(unique(elu_vals))

# Native area for crop
nac <- raster::shapefile("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/potato/native_area/native_area_tuberosum.shp")
elu_crpd <- raster::crop(elu, raster::extent(nac))
elu_crpd <- raster::mask(x = elu_crpd, mask = nac)
elu_crpd[!(elu_crpd[] %in% elu_vals2)] <- NA
raster::writeRaster(elu_crpd, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/potato/native_area/native_area_tuberosum_chilotanum_elus.tif")

elu_crpd2 <- elu_crpd
elu_crpd2[!is.na(elu_crpd2[])] <- 1
raster::writeRaster(elu_crpd2, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/potato/native_area/native_area_tuberosum_chilotanum.tif")

ajan <- raster::raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/potato/native_area/native_area_ajanhuiri.tif")
sum(!is.na(ajan[]))
