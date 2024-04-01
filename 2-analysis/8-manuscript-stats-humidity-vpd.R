#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Script to obtain data reported in manuscript results section on humidity/VPD
# Diarrhea (control arm) and pathogen-specific adjusted models
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/0-utils/1-plot-functions.R"))

# define list of temperature variable names
risk_factor = c("vpd_C")


#------------------------------------------------------
## Get summary statistics for manuscript
#------------------------------------------------------
terraclimate_vpd = readRDS(paste0(box_data_path, "washb-bangladesh-vpd.RDS"))
summary(terraclimate_vpd$vpd)


# read in analysis (gamm) results --------------------------------------------------
# define results path in Box 
diar0_results_dir <- paste0(offset_results_path, "gam_outputs/diarrhea-adjusted-0/")
pathogen_results_dir <- paste0(offset_results_path, "gam_outputs/pathogens-adjusted/")


##############################################################################
#-----------------------------------------------------------------------------
# Diarrhea, adjusted models
#-----------------------------------------------------------------------------
##############################################################################
set.seed(22242)
diar_data <- prep_gam_plot(results_directory = diar0_results_dir, 
                                yname = "diar7d_0_", 
                                risk_factor = risk_factor) %>% 
  mutate(outcome = "Diarrhea")

#------------------------------------------------------
##Show estimates for diarrhea prevalence at VPD 0.56 and 1.50
#------------------------------------------------------
## at VPD 0.56
diar_data %>% 
  filter(risk_factor_value >= 0.56 & risk_factor_value <=0.57) %>% 
  summarise(prev = mean(fit, na.rm = T),
            lb = mean(lwrS,na.rm = T),
            ub = mean(uprS, na.rm = T))
## at VPD 1.50
diar_data %>% 
  filter(risk_factor_value >= 1.50 & risk_factor_value <=1.51) %>% 
  summarise(prev = mean(fit, na.rm = T),
            lb = mean(lwrS,na.rm = T),
            ub = mean(uprS, na.rm = T))


##############################################################################
#-----------------------------------------------------------------------------
# Pathogens, adjusted models
#-----------------------------------------------------------------------------
##############################################################################
# pathogen outcomes
path_outcomes <- c(
  "pos_Adenovirus40_41_",
  # "pos_aEPEC_",
  "pos_Aeromonas_",
  "pos_B_fragilis_",
  "pos_C_difficile_",
  "pos_Campylobacter_",
  "pos_Cryptosporidium_",
  "pos_E_bieneusi_",
  "pos_EAEC_",
  "pos_EPEC_any_",
  "pos_ETEC_any_",
  "pos_Giardia_",
  # "pos_LT_ETEC_",
  "pos_Norovirus_",
  "pos_parasite_",
  "pos_Plesiomonas_",
  "pos_Sapovirus_",
  "pos_Shigella_EIEC_",
  # "pos_ST_ETEC_",
  "pos_STEC_",
  # "pos_tEPEC_",
  "pos_virus_"
)

set.seed(22242)
pathogen_data <- map_dfr(path_outcomes, function(outcome) {
  plot_pathogen_y_list <- prep_gam_plot(results_directory = pathogen_results_dir, 
                                        yname = outcome, 
                                        risk_factor = risk_factor) %>% 
    mutate(outcome = outcome)
})

#------------------------------------------------------
##Show estimates for ETEC prevalence at VPD 0.66 and 1.55
#------------------------------------------------------
## at VPD 0.56
pathogen_data %>% 
  filter(outcome == "pos_ETEC_any_" & risk_factor_value >= 0.65 & risk_factor_value <=0.67) %>% 
  summarise(prev = mean(fit, na.rm = T),
            lb = mean(lwrS,na.rm = T),
            ub = mean(uprS, na.rm = T))
## at VPD 1.50
pathogen_data %>% 
  filter(outcome == "pos_ETEC_any_" & risk_factor_value >= 1.55 & risk_factor_value <=1.561) %>% 
  summarise(prev = mean(fit, na.rm = T),
            lb = mean(lwrS,na.rm = T),
            ub = mean(uprS, na.rm = T))

#------------------------------------------------------
##Show estimates for Shigella/EIEC prevalence at VPD 0.66 and 1.55
#------------------------------------------------------
## at VPD 0.56
pathogen_data %>% 
  filter(outcome == "pos_Shigella_EIEC_" & risk_factor_value >= 0.65 & risk_factor_value <=0.67) %>% 
  summarise(prev = mean(fit, na.rm = T),
            lb = mean(lwrS,na.rm = T),
            ub = mean(uprS, na.rm = T))
## at VPD 1.50
pathogen_data %>% 
  filter(outcome == "pos_Shigella_EIEC_" & risk_factor_value >= 1.55 & risk_factor_value <=1.56) %>% 
  summarise(prev = mean(fit, na.rm = T),
            lb = mean(lwrS,na.rm = T),
            ub = mean(uprS, na.rm = T))

#------------------------------------------------------
##Show estimates for Sapovirus prevalence at VPD 0.66 and 1.55
#------------------------------------------------------
## at VPD 0.56
pathogen_data %>% 
  filter(outcome == "pos_Sapovirus_" & risk_factor_value >= 0.65 & risk_factor_value <=0.67) %>% 
  summarise(prev = mean(fit, na.rm = T),
            lb = mean(lwrS,na.rm = T),
            ub = mean(uprS, na.rm = T))
## at VPD 1.50
pathogen_data %>% 
  filter(outcome == "pos_Sapovirus_" & risk_factor_value >= 1.55 & risk_factor_value <=1.56) %>% 
  summarise(prev = mean(fit, na.rm = T),
            lb = mean(lwrS,na.rm = T),
            ub = mean(uprS, na.rm = T))

#------------------------------------------------------
##Show estimates for Any Virus prevalence at VPD 0.66 and 1.55
#------------------------------------------------------
## at VPD 0.56
pathogen_data %>% 
  filter(outcome == "pos_virus_" & risk_factor_value >= 0.65 & risk_factor_value <=0.67) %>% 
  summarise(prev = mean(fit, na.rm = T),
            lb = mean(lwrS,na.rm = T),
            ub = mean(uprS, na.rm = T))
## at VPD 1.50
pathogen_data %>% 
  filter(outcome == "pos_virus_" & risk_factor_value >= 1.55 & risk_factor_value <=1.56) %>% 
  summarise(prev = mean(fit, na.rm = T),
            lb = mean(lwrS,na.rm = T),
            ub = mean(uprS, na.rm = T))