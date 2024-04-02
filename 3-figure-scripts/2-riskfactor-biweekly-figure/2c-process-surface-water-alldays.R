#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# process surface water distance data 
# to calculate weekly averages. 
# this will be used in the figure showing risk 
# factors over time. 
#######################################

rm(list=ls())

## configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

# create date sequence for plot  ----------------------
master_biweek <- readRDS(paste0(data_dir, "biweekly_plot_data/master_biweek_dates.RDS")) %>% filter(biweek_date <= as.Date("2016-01-21"))
date_seq = as.list(master_biweek$biweek_date)

# read in temperature data ----------------------
df_surface_water_original = readRDS(paste0(box_data_path, "washb-bangladesh-distance_from_surface_water.RDS"))

# merge cluster ids into the temperature data ----------------------

## Read in diarrhea data 
df_diar = readRDS(paste0(data_dir, "biweekly_plot_data/d_diar_biweek.RDS"))

clusterids = df_diar %>%
  filter(intervention == 0) %>% # Keep only clusters in the control arm
  dplyr::select(clusterid, qgpslat, qgpslong, biweek) %>% 
  distinct()

df_surface_water_merged = left_join(df_surface_water_original, clusterids, by = c("qgpslat", "qgpslong"))

################################################################
# Check that the number of unique coordinates that have #
# been assigned to a cluster is equal to the number of unique 
# coordinates in the merged df with a cluster
################################################################

# Check for equality 
all_control = clusterids %>% select(qgpslat, qgpslong) %>% distinct()
all_merged = df_surface_water_merged %>% select(qgpslat, qgpslong) %>% distinct()
missing_coords = all_control %>% anti_join(all_merged,  by = c("qgpslong", "qgpslat"))

assert_that(nrow(missing_coords) == 0, msg = glue::glue("{nrow(missing_coords)} coordinates were not matched with a cluster id"))

# function to get mean temp within lag period ----------------------
find_avg_surface_water = function(data, date, surface_water_var){
  filter_date = data %>% filter(biweek == date) 
  
  while (nrow(filter_date) < 10) {
    date = data %>% filter(biweek > date) %>% pull(biweek) %>% min()
    filter_date = data %>% filter(biweek == date)
  }
  
  # get mean and CI
  estimate = washb_mean(filter_date[[surface_water_var]], id=filter_date$clusterid, print = F) %>% 
    as.data.frame()
  
  return(data.frame(mean = estimate$Mean,
                    lb = estimate$`Lower 95%CI`,
                    ub = estimate$`Upper 95%CI`))
}

generate_surface_water_df = function(surface_water_var){
  mean_surface_water_df = lapply(date_seq, 
                                 function(x) find_avg_surface_water(data = df_surface_water_merged, 
                                                                    date = x, 
                                                                    surface_water_var = surface_water_var)) %>% 
    bind_rows()
  
  
  mean_surface_water_savedata = data.frame(
    date = master_biweek$biweek_date,
    mean_surface_water = mean_surface_water_df$mean,
    mean_surface_water_lb = mean_surface_water_df$lb,
    mean_surface_water_ub = mean_surface_water_df$ub
  )
  
  colnames(mean_surface_water_savedata) = c("date", glue::glue("{surface_water_var}"), 
                                            glue::glue("{surface_water_var}_lb"), 
                                            glue::glue("{surface_water_var}_ub"))
  
  saveRDS(mean_surface_water_savedata, 
          file = paste0(data_dir, glue::glue("biweekly_plot_data/mean_{surface_water_var}_time_for_plot.RDS")))

}

surface_water_vars = c("prop_detected_any_surface_water_250",
                       "prop_detected_seasonal_surface_water_250",
                       "prop_detected_ephemeral_surface_water_250",
                       "prop_detected_permanent_water_250",
                       "distance_from_any_surface_water")

lapply(surface_water_vars, generate_surface_water_df)


#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()