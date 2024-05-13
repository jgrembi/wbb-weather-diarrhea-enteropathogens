#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Construct surface water variables for analysis

# Variables: 
# - distance_from_*_surface_water: distance (in meters) from any, seasonal, or ephemeral surface water
# - prop_detected_*_* + n_detected_*_N: number/proportion of pixels within N meter radius covered by any, seasonal, or ephemeral surface water
# Input File Directories:
# - data/occurrence.tif
# - data/transition.tif

# Output Files
# - data/washb-bangladesh-distance_from_surface_water.RDS
# Data Source: https://global-surface-water.appspot.com/
#######################################

rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
library(raster)
library(geosphere)
library(pracma)
library(matrixStats)

d_path = readRDS(paste0(clean_washb_path_box, clean_bdata_pathogen_box)) %>%
  filter(!is.na(qgpslong), !is.na(date))
ll_path = d_path %>% 
  dplyr::select(qgpslong, qgpslat)

d_diarr = readRDS(paste0(clean_washb_path_box, clean_bdata_diarr_box)) %>%
  filter(!is.na(qgpslong), !is.na(date))
ll_diarr = d_diarr %>% 
  dplyr::select(qgpslong, qgpslat)

ll = rbind(ll_path, ll_diarr) %>% 
  distinct()


any_surface_water_raster = raster(paste0(box_data_path, "occurrence.tif"))
any_surface_water_raster[any_surface_water_raster[] == 0] = NA 
any_surface_water_raster[any_surface_water_raster[] == 255] = NA
any_surface_water = rasterToPoints(any_surface_water_raster)
any_surface_water = SpatialPoints(any_surface_water[, 1:2], proj4string=CRS('+proj=longlat +datum=WGS84'))

permanent_water_raster = seasonal_surface_water_raster = ephemeral_surface_water_raster = raster(paste0(box_data_path, "transition.tif"))

permanent_water_raster[!(permanent_water_raster[] %in% c(1,2))] = NA 
permanent_water = rasterToPoints(permanent_water_raster)
permanent_water = SpatialPoints(permanent_water[, 1:2], proj4string=CRS('+proj=longlat +datum=WGS84'))

seasonal_surface_water_raster[!(seasonal_surface_water_raster[] %in% c(4, 5, 8, 10))] = NA 
seasonal_surface_water = rasterToPoints(seasonal_surface_water_raster)
seasonal_surface_water = SpatialPoints(seasonal_surface_water[, 1:2], proj4string=CRS('+proj=longlat +datum=WGS84'))

ephemeral_surface_water_raster[!(ephemeral_surface_water_raster[] %in% c(9, 10))] = NA 
ephemeral_surface_water = rasterToPoints(ephemeral_surface_water_raster)
ephemeral_surface_water = SpatialPoints(ephemeral_surface_water[, 1:2], proj4string=CRS('+proj=longlat +datum=WGS84'))



search_radius = function(surface_water_type, surface_water_raster, ll_sppoints, radius_size) {
  sppoints = SpatialPoints(ll, proj4string=CRS('+proj=longlat +datum=WGS84'))
  
  n_any_surface_water = extract(surface_water_raster, ll_sppoints, buffer = radius_size)
  
  n_detected_surface_water = sapply(n_any_surface_water, function(x) sum(!is.na(x)))

  prop_detected_surface_water = sapply(n_any_surface_water, function(x) mean(!is.na(x)))
  
  detected_sf_tbl = data.frame(n_detected_surface_water = n_detected_surface_water, 
                               prop_detected_surface_water = prop_detected_surface_water)
  
  colnames(detected_sf_tbl) = c(paste0("n_detected_", surface_water_type, "_", radius_size),
                                paste0("prop_detected_", surface_water_type, "_", radius_size))
  return(detected_sf_tbl)
}

find_distance_from_water = function(ll_block){
  ll_block = ll_block %>% dplyr::select(-g)
  ll_sppoints = SpatialPoints(ll_block, proj4string=CRS('+proj=longlat +datum=WGS84'))
  
  distance_from_any_surface_water = distm(x = ll_sppoints, y = any_surface_water, fun = distHaversine) %>% rowMins()
  detected_any_surface_water = lapply(c(250, 500, 750), 
                                      function(size) search_radius("any_surface_water", any_surface_water_raster, ll_sppoints, size)) %>% bind_cols()
  
  distance_from_permanent_water = distm(x = ll_sppoints, y = permanent_water, fun = distHaversine) %>% rowMins()
  detected_permanent_water = lapply(c(250, 500, 750), 
                                    function(size) search_radius("permanent_water", permanent_water_raster, ll_sppoints, size)) %>% bind_cols()
  
  distance_from_seasonal_surface_water = distm(x = ll_sppoints, y = seasonal_surface_water, fun = distHaversine) %>% rowMins()
  detected_seasonal_surface_water = lapply(c(250, 500, 750), 
                                           function(size) search_radius("seasonal_surface_water", seasonal_surface_water_raster, ll_sppoints, size)) %>% bind_cols()
  
  distance_from_ephemeral_surface_water = distm(x = ll_sppoints, y = ephemeral_surface_water, fun = distHaversine) %>% rowMins()
  detected_ephemeral_surface_water = lapply(c(250, 500, 750), 
                                            function(size) search_radius("ephemeral_surface_water", ephemeral_surface_water_raster, ll_sppoints, size)) %>% bind_cols()
  
  return(ll_block %>% mutate(distance_from_any_surface_water = distance_from_any_surface_water,
                             distance_from_permanent_water = distance_from_permanent_water,
                             distance_from_seasonal_surface_water = distance_from_seasonal_surface_water,
                             distance_from_ephemeral_surface_water = distance_from_ephemeral_surface_water) %>% 
           bind_cols(detected_any_surface_water, detected_permanent_water, 
                     detected_seasonal_surface_water, detected_ephemeral_surface_water))
}

ll_blocks = ll %>% mutate(g = (1:nrow(ll)) %% 500) %>% 
  group_by(g) %>% 
  group_split()

distance_from_water = lapply(ll_blocks, find_distance_from_water) %>% bind_rows()

saveRDS(distance_from_water, paste0(box_data_path, "washb-bangladesh-distance_from_surface_water.RDS"))

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()