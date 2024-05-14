#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# For all categorical risk factors, make table for age category interactions   
# for all diarrhea models (no age interaction was evaluated for pathogens due to 
#  the narrow age range of the sample frame)

#######################################

rm(list=ls())


# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/0-utils/2-categorical-rf-tables-functions.R"))

# Define filepath to model results from age-interaction models
# file_path = paste0(offset_results_path, "gam_outputs/diarrhea-adjusted-interaction-age-0/")
# Build paths to files in Sherlock to create data-frame
if(Sys.getenv("LMOD_SYSHOST")=="sherlock"){
  # Build path to subfolders in GROUP HOME
  file_path <- paste0(sherlock_results_dir, "gam_outputs/diarrhea-adjusted-interaction-age-0/")
} else {
  file_path <- paste0(offset_results_path, "gam_outputs/diarrhea-adjusted-interaction-age-0/")
}

risk_factors_categorical = c(
  "heavyrain_1weeklag",
  "heavyrain_2weeklag",
  "heavyrain_3weeklag",
  "heavyrain_1weeklag_90",
  "heavyrain_2weeklag_90",
  "heavyrain_3weeklag_90",
  "distance_from_any_surface_water_tertile",
  "distance_from_seasonal_surface_water_tertile",     
  "distance_from_ephemeral_surface_water_tertile", 
  "prop_detected_any_surface_water_250_median",
  "prop_detected_seasonal_surface_water_250_median",
  "prop_detected_ephemeral_surface_water_250_median", 
  "prop_detected_any_surface_water_500_median",
  "prop_detected_seasonal_surface_water_500_median",
  "prop_detected_ephemeral_surface_water_500_median",
  "prop_detected_any_surface_water_750_median",
  "prop_detected_seasonal_surface_water_750_median",
  "prop_detected_ephemeral_surface_water_750_median",
  "ppt_week_sum_1weeklag_median",
  "ppt_week_sum_2weeklag_median",
  "ppt_week_sum_3weeklag_median",
  "ppt_week_sum_1weeklag_75",
  "ppt_week_sum_2weeklag_75",
  "ppt_week_sum_3weeklag_75",
  "ppt_week_sum_1weeklag_90",
  "ppt_week_sum_2weeklag_90",
  "ppt_week_sum_3weeklag_90")

risk_factor_categorical_label = c(
  "Heavy Rain, 1 Week Lag",
  "Heavy Rain, 2 Week Lag",
  "Heavy Rain, 3 Week Lag",
  "Heavy Rain, 1 Week Lag - 90th percentile",
  "Heavy Rain, 2 Week Lag - 90th percentile",
  "Heavy Rain, 3 Week Lag - 90th percentile",
  "Distance Tertile From Any Surface Water",
  "Distance Tertile From Seasonal Surface Water",
  "Distance Tertile From Ephemeral Surface Water",
  "Proportion of Area within 250m Radius of Household with Any Surface Water",
  "Proportion of Area within 250m Radius of Household with Seasonal Surface Water",
  "Proportion of Area within 250m Radius of Household with Ephemeral Surface Water",
  "Proportion of Area within 500m Radius of Household with Any Surface Water",
  "Proportion of Area within 500m Radius of Household with Seasonal Surface Water",
  "Proportion of Area within 500m Radius of Household with Ephemeral Surface Water",
  "Proportion of Area within 750m Radius of Household with Any Surface Water",
  "Proportion of Area within 750m Radius of Household with Seasonal Surface Water",
  "Proportion of Area within 750m Radius of Household with Ephemeral Surface Water",
  "Weekly Sum of Precipitation above Median, 1 week lag",
  "Weekly Sum of Precipitation above Median, 2 week lag",
  "Weekly Sum of Precipitation above Median, 3 week lag",
  "Weekly Sum of Precipitation above 75th percentile, 1 week lag",
  "Weekly Sum of Precipitation above 75th percentile, 2 week lag",
  "Weekly Sum of Precipitation above 75th percentile, 3 week lag",
  "Weekly Sum of Precipitation above 90th percentile, 1 week lag",
  "Weekly Sum of Precipitation above 90th percentile, 2 week lag",
  "Weekly Sum of Precipitation above 90th percentile, 3 week lag")


cat_interaction_results <- map(risk_factors_categorical, \(x) make_interaction_table(file_path = file_path, 
                                                                                          file_name = paste0("gam_diar7d_0_", x, "_by_age_cat.RDS"), 
                                                                                          outcome = "diar7d", 
                                                                                          risk_factor1 = x, 
                                                                                          risk_factor2 = "age_cat") %>% 
                                 rename(RF = risk_factor1)) %>%
  list_rbind()

pretty_labels <- data.frame(RF = risk_factors_categorical, label = risk_factor_categorical_label)
cat_interaction_results <- cat_interaction_results %>%
  left_join(pretty_labels)

saveRDS(cat_interaction_results, file = paste0(data_dir, "corrected_PR_tables_age_interaction_diarrhea.RDS"))  

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()