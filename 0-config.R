#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# configure data directories
# source base functions
# load libraries
#######################################

#--------------------------------------------
# Install (if not already installed) and load in libraries
#--------------------------------------------
install.packages("Librarian")
library(Librarian)

shelf(here, dplyr, ggcorrplot, tidyr, reshape2, ggplot2,
      grid, gridExtra, ben-arnold/washb, lubridate, stringr,
      ncdf4, boxr, assertthat, mgcv, gamm4, DHARMa, purrr,
      viridis, caret, wrapr, pgirmess, tidyverse, kableExtra,
      rmarkdown, tictoc, doParallel, doRNG, parallel, foreach,
      magrittr, broom, lme4, rcartocolor, RColorBrewer, MetBrewer,
      cowplot, patchwork)

# source base functions  
source(paste0(here::here(), "/0-utils/0-base-functions.R"))

#--------------------------------------------
# Define repository paths
#--------------------------------------------
fig_dir = paste0(here::here(),"/4-figures/")
tab_dir = paste0(here::here(),"/6-tables/")
data_dir = paste0(here::here(), "/data/")
results_path = paste0(here::here(), "/results/")

gam_check_template_rmd = paste0(here::here(), "/3-figure-scripts/model_fit_output.Rmd")
rainy_seasons_path = paste0(results_path, "season_definitions.csv")


#--------------------------------------------
# Define (locally-mounted) Box directories
#--------------------------------------------
# path to box on each person's local
if(Sys.getenv("HOME") == "/Users/jadebc"){
  local_root_path = "/Users/jadebc/Library/CloudStorage/Box-Box/Jade Benjamin-Chung's Externally Shareable Files/"
} else if(Sys.getenv("HOME") == "/Users/arushapatil"){
  local_root_path = "/Users/arushapatil/Library/CloudStorage/Box-Box/"
} else if(Sys.getenv("HOME") == "/Users/gabriellabh"){
  local_root_path = "/Users/gabriellabh/Library/CloudStorage/Box-Box/"
} else if(Sys.getenv("HOME") == "/Users/annanguyen") {
  local_root_path = "/Users/annanguyen/Box Sync/"        
} else if(Sys.getenv("HOME") == "/Users/JGrembi") {
  local_root_path = "/Users/JGrembi/Library/CloudStorage/Box-Box/"  
} else {
  local_root_path = ""
}


local_box_path = "WBB-mapping-Stanford/"

#--------------------------------------------
# Define Sherlock (computing cluster) paths
#--------------------------------------------
if(Sys.getenv("LMOD_SYSHOST") == "sherlock"){
  sherlock_path = here::here()
  # data_path = paste0(sherlock_path, "data/")
  # results_path = paste0(sherlock_path, "results/")
  local_root_path = sherlock_path
  sherlock_results_dir = "/home/groups/jadebc/wbb-mapping-data/results/"
}
#--------------------------------------------
# Define data paths for Box
#--------------------------------------------
clean_bdata_raster_path_box = "wbb_mapping_bangladesh_data_raster.RDS"
box_data_path = paste0(local_root_path, local_box_path, "wbb-weather-diarrhea-pathogens-data/")

## Clean outcome data
clean_washb_path_box = paste0(local_root_path, local_box_path, "clean-washb/")
clean_bdata_diarr_box = "wbb_mapping_bangladesh_data_diarr.RDS"
clean_bdata_pathogen_box = "wbb_mapping_bangladesh_pathogen_data.RDS"

## Merged with risk factors
clean_pathogen_merged_box = "washb-bangladesh-merged-path.RDS"
clean_diarr_merged_box = "washb-bangladesh-merged-diarr.RDS"

## Offset GPS coords to protect PII
if (Sys.getenv("LMOD_SYSHOST") == "sherlock") {
  clean_offset_data_dir = "/home/groups/jadebc/wbb-mapping-data/cleaned_offset_data/"
} else {
  clean_offset_data_dir = paste0(box_data_path, "cleaned_offset_data/") # this is now used only for the offset data
}
clean_pathogen_merged_offset = "washb-bangladesh-merged-path_offset.RDS"
clean_diarr_merged_offset = "washb-bangladesh-merged-diarr_offset.RDS"


## Other paths
offset_results_path = paste0(local_root_path, local_box_path, "results/")
box_intermediate_path = paste0(local_root_path, local_box_path, "figures-categorical-risk-factors/intermediate-RDS-files/")


#--------------------------------------------
# Define messages that indicate model was fit successfully 
#--------------------------------------------
model_pass = c("Non-spatial gam fit successfully, no spatial autocorrelation detected so no spatial gam fit",
               "Spatial gam fit successfully")
model_fail = c("Adjusted model not run because no covariates were associated with the outcome at P< 0.1",
               "Adjusted/interaction model could not be run due to data sparsity",
               "Non-spatial GAM failed to converge")

#--------------------------------------------
# define list of variables we will evaluate 
#--------------------------------------------
test.vars = risk_factors = c("temp_monthavg_0weeklag_C", "temp_monthavg_1weeklag_C","temp_monthavg_2weeklag_C", "temp_monthavg_3weeklag_C",
                             "temp_monthmin_0weeklag_C", "temp_monthmin_1weeklag_C", "temp_monthmin_2weeklag_C", "temp_monthmin_3weeklag_C",  
                             "temp_monthmax_0weeklag_C", "temp_monthmax_1weeklag_C", "temp_monthmax_2weeklag_C", "temp_monthmax_3weeklag_C",
                             "temp_weekavg_0weeklag_C", "temp_weekavg_1weeklag_C","temp_weekavg_2weeklag_C", "temp_weekavg_3weeklag_C",
                             "temp_weekmin_0weeklag_C", "temp_weekmin_1weeklag_C", "temp_weekmin_2weeklag_C", "temp_weekmin_3weeklag_C",  
                             "temp_weekmax_0weeklag_C", "temp_weekmax_1weeklag_C", "temp_weekmax_2weeklag_C", "temp_weekmax_3weeklag_C",
                             "temp_weekavg_0weeklag_median", "temp_weekavg_1weeklag_median","temp_weekavg_2weeklag_median", "temp_weekavg_3weeklag_median",
                             "temp_weekmin_0weeklag_median", "temp_weekmin_1weeklag_median","temp_weekmin_2weeklag_median", "temp_weekmin_3weeklag_median",
                             "temp_weekmax_0weeklag_median", "temp_weekmax_1weeklag_median","temp_weekmax_2weeklag_median", "temp_weekmax_3weeklag_median",
                             "ppt_week_sum_0weeklag_C", "ppt_week_sum_1weeklag_C", "ppt_week_sum_2weeklag_C", "ppt_week_sum_3weeklag_C",
                             "ppt_week_sum_0weeklag_median", "ppt_week_sum_1weeklag_median", "ppt_week_sum_2weeklag_median", "ppt_week_sum_3weeklag_median",
                             "ppt_week_sum_0weeklag_75", "ppt_week_sum_1weeklag_75", "ppt_week_sum_2weeklag_75", "ppt_week_sum_3weeklag_75", 
                             "ppt_week_sum_0weeklag_90", "ppt_week_sum_1weeklag_90", "ppt_week_sum_2weeklag_90", "ppt_week_sum_3weeklag_90",
                             "heavyrain_0weeklag", "heavyrain_1weeklag", "heavyrain_2weeklag","heavyrain_3weeklag",
                             "heavyrain_0weeklag_90", "heavyrain_1weeklag_90", "heavyrain_2weeklag_90","heavyrain_3weeklag_90",
                             "vpd_C",
                             "distance_from_any_surface_water_tertile",
                             "distance_from_seasonal_surface_water_tertile",     
                             "distance_from_ephemeral_surface_water_tertile",    
                             "prop_detected_any_surface_water_250_median","prop_detected_any_surface_water_500_median", "prop_detected_any_surface_water_750_median", 
                             "prop_detected_seasonal_surface_water_250_median","prop_detected_seasonal_surface_water_500_median", "prop_detected_seasonal_surface_water_750_median", 
                             "prop_detected_ephemeral_surface_water_250_median","prop_detected_ephemeral_surface_water_500_median", "prop_detected_ephemeral_surface_water_750_median")

interaction.terms = c(#"ppt_60daysum_0weeklag","ppt_60daysum_1weeklag","ppt_60daysum_2weeklag","ppt_60daysum_3weeklag",
                      #"ppt_30daysum_0weeklag","ppt_30daysum_1weeklag","ppt_30daysum_2weeklag","ppt_30daysum_3weeklag",
                      "temp_weekavg_0weeklag_C",           
                      "temp_weekavg_1weeklag_C",                
                      "temp_weekavg_2weeklag_C",                  
                      "temp_weekavg_3weeklag_C", 
                      # "temp_weekavg_0weeklag_median",           
                      # "temp_weekavg_1weeklag_median",                
                      # "temp_weekavg_2weeklag_median",                  
                      # "temp_weekavg_3weeklag_median", 
                      "temp_monthavg_0weeklag_C", 
                      "temp_monthavg_1weeklag_C",
                      "temp_monthavg_2weeklag_C", 
                      "temp_monthavg_3weeklag_C")
                      # "temp_monthavg_0weeklag_median", 
                      # "temp_monthavg_1weeklag_median",
                      # "temp_monthavg_2weeklag_median", 
                      # "temp_monthavg_3weeklag_median")
              
