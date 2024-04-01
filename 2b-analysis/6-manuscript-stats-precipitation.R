#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Script to obtain data reported in manuscript results section on precipitation 
# Diarrhea (control arm) and pathogen-specific adjusted models
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/0-utils/1-plot-functions.R"))

# define list of temperature variable names
risk_factors = c("ppt_week_sum_0weeklag_median",
                 "ppt_week_sum_1weeklag_median",
                 "ppt_week_sum_2weeklag_median",
                 "ppt_week_sum_3weeklag_median",
                 "heavyrain_0weeklag",
                 "heavyrain_1weeklag", 
                 "heavyrain_2weeklag", 
                 "heavyrain_3weeklag")


##############################################################################
#-----------------------------------------------------------------------------
# Precipitation ranges across study area for duration of study period
#-----------------------------------------------------------------------------
##############################################################################
ppt_all_pixels = readRDS(paste0(box_data_path, "ppt_cutoffs.RDS"))

####################################
### All seasons
####################################
## weekly median ppt
ppt_all_pixels$median_7d_cutoff

## weekly min ppt
ppt_all_pixels$min_7d_sum

## weekly max ppt
ppt_all_pixels$max_7d_sum

####################################
### Dry season
####################################
## weekly median ppt
ppt_all_pixels$median_7d_dry

## weekly min ppt
ppt_all_pixels$min_7d_dry

## weekly max ppt
ppt_all_pixels$max_7d_dry

####################################
### Rainy season
####################################
## weekly median ppt
ppt_all_pixels$median_7d_wet

## weekly min ppt
ppt_all_pixels$min_7d_wet

## weekly max ppt
ppt_all_pixels$max_7d_wet

# read in analysis (gamm) results --------------------------------------------------
prev_table = readRDS(paste0(tab_dir, "corrected_PR_tables.RDS")) %>%
  filter(risk_factor %in% risk_factors)

#------------------------------------------------------
##Show estimates for diarrhea prevalence at precipitation above the median
#------------------------------------------------------
## unadjusted
prev_table %>% 
  filter(outcome_name == "diar7d", 
         risk_factor == "ppt_week_sum_2weeklag_median", 
         Group == "Unadj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)
## adjusted
prev_table %>% 
  filter(outcome_name == "diar7d", 
         risk_factor == "ppt_week_sum_2weeklag_median", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)


##############################################################################
#-----------------------------------------------------------------------------
# Pathogens, adjusted models
#-----------------------------------------------------------------------------
##############################################################################

#------------------------------------------------------
##Show estimates for parasite prevalence at precipitation above the median
#------------------------------------------------------
prev_table %>% 
  filter(outcome_name == "pos_Cryptosporidium", 
         risk_factor == "ppt_week_sum_3weeklag_median", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)


prev_table %>% 
  filter(outcome_name == "pos_Giardia", 
         risk_factor == "ppt_week_sum_2weeklag_median", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)

#------------------------------------------------------
##Show estimates for virus prevalence at precipitation above the median
#------------------------------------------------------

prev_table %>% 
  filter(outcome_name == "pos_Adenovirus40_41", 
         risk_factor == "ppt_week_sum_1weeklag_median", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)

prev_table %>% 
  filter(outcome_name == "pos_Sapovirus", 
         risk_factor == "ppt_week_sum_1weeklag_median", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)

prev_table %>% 
  filter(outcome_name == "pos_Norovirus", 
         risk_factor == "ppt_week_sum_1weeklag_median", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)


#------------------------------------------------------
##Show estimates for diarrhea prevalence during heavy rainfall events
#------------------------------------------------------
prev_table %>% 
  filter(outcome_name == "diar7d", 
         risk_factor == "heavyrain_2weeklag", 
         Group == "Adj.") %>%
  select(outcome_name, risk_factor, PR, PR.Lower, PR.Upper)


#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()
