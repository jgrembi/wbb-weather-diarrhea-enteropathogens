#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Script to obtain data reported in manuscript results section on surface water
# Diarrhea (control arm) and pathogen-specific adjusted models
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/0-utils/1-plot-functions.R"))

# define list of temperature variable names
risk_factors = c("distance_from_any_surface_water_tertile",
                 "distance_from_seasonal_surface_water_tertile",     
                 "distance_from_ephemeral_surface_water_tertile",    
                 "prop_detected_any_surface_water_250_median","prop_detected_any_surface_water_500_median", "prop_detected_any_surface_water_750_median", 
                 "prop_detected_seasonal_surface_water_250_median","prop_detected_seasonal_surface_water_500_median", "prop_detected_seasonal_surface_water_750_median", 
                 "prop_detected_ephemeral_surface_water_250_median","prop_detected_ephemeral_surface_water_500_median", "prop_detected_ephemeral_surface_water_750_median")



##############################################################################
#-----------------------------------------------------------------------------
# Distance from surface water ranges across study area 
#-----------------------------------------------------------------------------
##############################################################################
d_diarrhea <- readRDS(paste0(clean_offset_data_dir, clean_diarr_merged_offset)) %>%
  filter(intervention == 0)

## any surface water 
summary(d_diarrhea$distance_from_any_surface_water)

## ephemeral surface water
summary(d_diarrhea$distance_from_ephemeral_surface_water)

## seasonal surface water
summary(d_diarrhea$distance_from_seasonal_surface_water)


##############################################################################
#-----------------------------------------------------------------------------
# Show tertile cutoffs for distance to any surface water
#-----------------------------------------------------------------------------
##############################################################################
surface_water_distances = readRDS(paste0(box_data_path,"washb-bangladesh-distance_from_surface_water.RDS"))

#tertile cutoffs
round(quantile(surface_water_distances$distance_from_any_surface_water, 
               probs = c(0.33,0.66), 
               na.rm = T), 0)



# read in analysis (gamm) results --------------------------------------------------
prev_table = readRDS(paste0(data_dir, "corrected_PR_tables_nointeraction.RDS")) %>%
  filter(risk_factor %in% risk_factors)

#------------------------------------------------------
##Show estimates for diarrhea prevalence at precipitation above the median
#------------------------------------------------------
## unadjusted
prev_table %>% 
  filter(outcome_name == "diar7d", 
         risk_factor == "distance_from_any_surface_water_tertile", 
         Group == "Unadj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)

## adjusted
prev_table %>% 
  filter(outcome_name == "diar7d", 
         risk_factor == "distance_from_any_surface_water_tertile", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)


##############################################################################
#-----------------------------------------------------------------------------
# Pathogens, adjusted models
#-----------------------------------------------------------------------------
##############################################################################

#------------------------------------------------------
##Show estimates for pathogen prevalence at surface water tertiles 
## compared to the nearest tertile
#------------------------------------------------------
prev_table %>% 
  filter(outcome_name == "pos_Aeromonas", 
         risk_factor == "distance_from_any_surface_water_tertile", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)


prev_table %>% 
  filter(outcome_name == "pos_virus", 
         risk_factor == "distance_from_ephemeral_surface_water_tertile", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)

prev_table %>% 
  filter(outcome_name == "pos_virus", 
         risk_factor == "distance_from_seasonal_surface_water_tertile", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)






##############################################################################
#-----------------------------------------------------------------------------
# Proportion of land that contained any surface water ranges across study area 
#-----------------------------------------------------------------------------
##############################################################################

## any surface water 250m
summary(d_diarrhea$prop_detected_any_surface_water_250) * 100

## any surface water 500m
summary(d_diarrhea$prop_detected_any_surface_water_500) * 100

## any surface water 750m
summary(d_diarrhea$prop_detected_any_surface_water_750) * 100


#------------------------------------------------------
##Show estimates for pathogen and diarrhea prevalence at proportion of  above the median
#------------------------------------------------------

prev_table %>% 
  filter(outcome_name == "diar7d", 
         risk_factor == "prop_detected_any_surface_water_500_median", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)


prev_table %>% 
  filter(outcome_name == "pos_Adenovirus40_41", 
         risk_factor == "prop_detected_ephemeral_surface_water_750_median", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)

prev_table %>% 
  filter(outcome_name == "pos_Adenovirus40_41", 
         risk_factor == "prop_detected_ephemeral_surface_water_750_median", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)

prev_table %>% 
  filter(outcome_name == "pos_Norovirus", 
         risk_factor == "prop_detected_ephemeral_surface_water_750_median", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)

prev_table %>% 
  filter(outcome_name == "pos_E.bieneusi", 
         risk_factor == "prop_detected_any_surface_water_750_median", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)

prev_table %>% 
  filter(outcome_name == "pos_Shigella_EIEC", 
         risk_factor == "prop_detected_ephemeral_surface_water_750_median", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)


#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()