#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Construct humidity (vapor pressure deficit) variable for analysis

# extract terraclimate raster data to
# each lat/long coordinate
# Variables:  vapor pressue deficit (kPa)
# Input File Directories:
# - data/vpd-data
# Output Files
# - data/washb-bangladesh-vpd.RDS
# Data Source: http://www.climatologylab.org/terraclimate.html
#######################################

rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
library(geosphere)

d_path = readRDS(paste0(clean_washb_path_box, clean_bdata_pathogen_box)) %>%
  filter(!is.na(qgpslong), !is.na(date))
ll_path = d_path %>% 
  dplyr::select(qgpslong, qgpslat, date)

d_diarr = readRDS(paste0(clean_washb_path_box, clean_bdata_diarr_box)) %>%
  filter(!is.na(qgpslong), !is.na(date))
ll_diarr = d_diarr %>% 
  dplyr::select(qgpslong, qgpslat, date)

ll = rbind(ll_path, ll_diarr) %>% 
  distinct()

#-----------------------------------------------------
# define function to read in data from TerraClimate
#-----------------------------------------------------
process_terraclimate = function(var, coords, year){
  filename <- paste0(here::here(), "/data/", var, "-data/TerraClimate_", var, "_",
                     year, ".nc")
  
  x = as.numeric(coords)
  
  nc <- nc_open(filename)
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  
  flat = match(abs(lat - x[2]) < 1/48, 1)
  latindex = which(flat %in% 1)
  lat_match = lat[latindex]
  
  flon = match(abs(lon - x[1]) < 1/48, 1)
  lonindex = which(flon %in% 1)
  long_match = lon[lonindex]
  start <- c(lonindex, latindex, 1)
  count <- c(1, 1, -1)
  
  distance = distm(c(long_match, lat_match), x, fun = distHaversine)
  
  # read in the full period of record using aggregated files
  data <- as.numeric(ncvar_get(nc, varid = var,start = start, count))
  data = data %>% append(distance)
  return(data)
}

compile_terraclimate = function(latlong, var, year){
  #latlong = ll # temp, to speed up computations for testing 
  print(year)
  raw = apply(latlong,1, function(x) process_terraclimate(
    var = var, coords = x, year = year))
  raw_df = as.data.frame(raw)
  
  generate_terraclimate_table = function(month){
    tbl = as.data.frame(cbind(latlong[1], latlong[2], month, year, t(raw_df[month, ]), t(raw_df[13, ])))
    colnames(tbl) = c("qgpslong", "qgpslat", "month", "year", var, "distance_from_source")
    return(tbl)
  }

  df = lapply(1:12, generate_terraclimate_table) %>% bind_rows()
  
  return (df)
}

#-----------------------------------------------------
# read in data from TerraClimate
#-----------------------------------------------------
vpd = lapply(c(2012, 2013, 2014, 2015, 2016),
             function(x) compile_terraclimate(latlong = ll, var = "vpd", year = x)) %>%
  bind_rows()

saveRDS(vpd, paste0(box_data_path, "washb-bangladesh-vpd.RDS"))
