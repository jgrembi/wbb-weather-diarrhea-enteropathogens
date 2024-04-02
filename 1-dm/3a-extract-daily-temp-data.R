#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# extract daily temperatures from processed rasters
#######################################

rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
## Use the librarian package to install and load the following libraries only used in this script
shelf(geosphere, raster)

d_diarr = readRDS(paste0(clean_washb_path_box, clean_bdata_diarr_box)) 
ll = d_diarr %>% dplyr::select(qgpslong, qgpslat) 

ll_distinct = ll %>% dplyr::select(qgpslong, qgpslat) %>% distinct() %>% filter(!is.na(qgpslong), !is.na(qgpslat))
ll_sf = sf::st_as_sf(ll_distinct, coords=c("qgpslong","qgpslat"), crs="+proj=longlat +datum=WGS84 +no_defs")

temp_rasters_dir = paste0(box_data_path, "fldas_temperature_data/") 

temp_rasters_files = list.files(temp_rasters_dir)
temp_rasters_files = temp_rasters_files[str_detect(temp_rasters_files, ".tif")]

extract_fldas_data = function(filename){
  file_path = paste0(temp_rasters_dir, "/", filename)
  temp_raster = raster(file_path, varname = "Tair_f_tavg")
  extracted_temp_vals = extract(temp_raster, ll_sf, df = T, cellnumbers = T)
  colnames(extracted_temp_vals)[3] = "temp"
  
  date = str_extract_all(filename, "[[:digit:]]+")[[1]][2]
  extracted_temp_vals$date = as.Date(glue::glue("{str_sub(date, 1, 4)}-{str_sub(date, 5, 6)}-{str_sub(date, 7, 8)}"))
  
  ll_distinct %>% bind_cols(extracted_temp_vals) %>% dplyr::select(-ID)
}

tic() ## takes 2070 sec/ 34 min to run  
all_daily_fldas_data = lapply(temp_rasters_files, extract_fldas_data) %>% bind_rows()
toc()

saveRDS(all_daily_fldas_data, paste0(box_data_path, "daily_temperatures_fldas.RDS"))

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()