#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Script to obtain data reported in manuscript results section on temperature 
# Diarrhea (control arm) and pathogen-specific adjusted models
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/0-utils/1-plot-functions.R"))

# define list of temperature variable names
temp_vars <- list("temp_weekavg_1weeklag_C",
                  "temp_weekavg_2weeklag_C")
            
##############################################################################
#-----------------------------------------------------------------------------
# Temp ranges across study area for duration of study period
#-----------------------------------------------------------------------------
##############################################################################
temp_all_pixels = readRDS(paste0(data_dir, "fldas_temp_cutoffs.RDS"))

## weekly average temps
temp_all_pixels$avgtemp_7_days_range_inC

## weekly min temps
temp_all_pixels$absmintemp_7_days_range_inC

## weekly max temps
temp_all_pixels$absmaxtemp_7_days_range_inC

##############################################################################
#-----------------------------------------------------------------------------
# Diarrhea, control arm, unadjusted and adjusted models
#-----------------------------------------------------------------------------
##############################################################################

# define results path in Box 
diar0_unadj_results_dir <- paste0(offset_results_path, "gam_outputs/diarrhea-unadjusted-0/")
diar0_adj_results_dir <- paste0(offset_results_path, "gam_outputs/diarrhea-adjusted-0/")


#------------------------------------------------------
## Read in model results and generate predicted data using original data and model fits, including 95% Cis 
#------------------------------------------------------

unadj_diar_data <- lapply(temp_vars, function(x) {
  prep_gam_plot(results_directory = diar0_unadj_results_dir, 
                yname = "diar7d_0_", 
                risk_factor = x)}) %>%
  bind_rows() %>% 
  mutate(Model = "Unadjusted")

adj_diar_data <- lapply(temp_vars, function(x) {
  prep_gam_plot(results_directory = diar0_adj_results_dir, 
                yname = "diar7d_0_", 
                risk_factor = x)}) %>%
  bind_rows() %>%
  mutate(Model = "Adjusted")

diar_data <- bind_rows(unadj_diar_data, adj_diar_data) 

#------------------------------------------------------
## These are the stats reported in the 'Temperature' results section
#------------------------------------------------------

## 1-week lag
##Obtain estimates for diarrhea prevalence at 15 degrees C 
diar_data %>% 
  group_by(Model) %>%
  filter(risk_factor=="temp_weekavg_1weeklag_C" & risk_factor_value>=14.8 & risk_factor_value <=15.4) %>% 
  summarise(prev = mean(fit, na.rm = T),
            lb = mean(lwrS,na.rm = T),
            ub = mean(uprS, na.rm = T))

##Obtain estimates for diarrhea prevalence at 30 degrees C 
diar_data %>% 
  group_by(Model) %>%
  filter(risk_factor=="temp_weekavg_1weeklag_C" & risk_factor_value>=29.8 & risk_factor_value<=30.4) %>% 
  summarise(prev = mean(fit),
            lb = mean(lwrS),
            ub = mean(uprS))

##############################################################################
#-----------------------------------------------------------------------------
# Pathogens, adjusted models
#-----------------------------------------------------------------------------
##############################################################################

# define results path in Box 
pathogen_results_dir <- paste0(offset_results_path, "gam_outputs/pathogens-adjusted/")

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
  # "pos_tEPEC_")
  "pos_virus_"
)

#------------------------------------------------------
## Read in model results and generate predicted data using original data and model fits, including 95% Cis 
#------------------------------------------------------
set.seed(22242)
pathogen_list <- list()

for(i in 1:length(path_outcomes)){
  
  print(path_outcomes[i])
  pathogen_y_list <- lapply(temp_vars, function(x) 
    prep_gam_plot(results_directory = pathogen_results_dir, 
                  yname = path_outcomes[i], 
                  risk_factor = x)) 
  pathogen_list[[i]] <- pathogen_y_list %>% bind_rows() %>% 
    mutate(outcome = path_outcomes[i])
}

pathogen_data <- bind_rows(pathogen_list)

## Summarize range of weekly average temps, including all lag periods
## These are different within the pathogen cohort, which covered a shorter period of time
pathogen_data %>% 
  filter(risk_factor %in% c("temp_weekavg_0weeklag_C", "temp_weekavg_1weeklag_C", "temp_weekavg_2weeklag_C", "temp_weekavg_3weeklag_C")) %>% 
  summarise(min = min(risk_factor_value, na.rm = T),
            max = max(risk_factor_value, na.rm = T))
## As we can see, we do not have data lower than 19 degrees C, 
##  so we can not provide model predictions for prevalence at 15 degree C as we did for diarrhea

## STEC ##
##Obtain estimates for STEC prevalence at 19 degrees C 
pathogen_data %>% 
  filter(!is.na(pos_STEC)) %>%
  filter(risk_factor=="temp_weekavg_1weeklag_C" & risk_factor_value>=18.6 & risk_factor_value<=19.4) %>% 
  summarise(prev = mean(fit),
            lb = mean(lwrS),
            ub = mean(uprS))

##Obtain estimates for STEC prevalence at 32 degrees C 
pathogen_data %>% 
  filter(!is.na(pos_STEC)) %>%
  filter(risk_factor=="temp_weekavg_1weeklag_C" & risk_factor_value>=31.6 & risk_factor_value<=32.4) %>% 
  summarise(prev = mean(fit),
            lb = mean(lwrS),
            ub = mean(uprS))
## EAEC ##
##Obtain estimates for ETEC prevalence at 19 degrees C 
pathogen_data %>% 
  filter(!is.na(pos_ETEC_any)) %>%
  filter(risk_factor=="temp_weekavg_1weeklag_C" & risk_factor_value>=18.6 & risk_factor_value<=19.4) %>% 
  summarise(prev = mean(fit),
            lb = mean(lwrS),
            ub = mean(uprS))

##Obtain estimates for ETEC prevalence at 32 degrees C 
pathogen_data %>% 
  filter(!is.na(pos_ETEC_any)) %>%
  filter(risk_factor=="temp_weekavg_1weeklag_C" & risk_factor_value>=31.6 & risk_factor_value<=32.4) %>% 
  summarise(prev = mean(fit),
            lb = mean(lwrS),
            ub = mean(uprS))


## Parasites and viruses

##Obtain estimates for Cryptosporidium prevalence at 19 degrees C 
pathogen_data %>% 
  filter(!is.na(pos_Cryptosporidium)) %>%
  filter(risk_factor=="temp_weekavg_2weeklag_C" & risk_factor_value>=18.6 & risk_factor_value<=19.4) %>% 
  summarise(prev = mean(fit),
            lb = mean(lwrS),
            ub = mean(uprS))

##Obtain estimates for Cryptosporidium prevalence at 30 degrees C 
pathogen_data %>% 
  filter(!is.na(pos_Cryptosporidium)) %>%
  filter(risk_factor=="temp_weekavg_2weeklag_C" & risk_factor_value>=29.6 & risk_factor_value<=30.4) %>% 
  summarise(prev = mean(fit),
            lb = mean(lwrS),
            ub = mean(uprS))



##Obtain estimates for E.bieneusi prevalence at 19 degrees C 
pathogen_data %>% 
  filter(!is.na(pos_E.bieneusi)) %>%
  filter(risk_factor=="temp_weekavg_2weeklag_C" & risk_factor_value>=18.6 & risk_factor_value<=19.4) %>% 
  summarise(prev = mean(fit),
            lb = mean(lwrS),
            ub = mean(uprS))

##Obtain estimates for E.bieneusi prevalence at 25 degrees C 
pathogen_data %>% 
  filter(!is.na(pos_E.bieneusi)) %>%
  filter(risk_factor=="temp_weekavg_2weeklag_C" & risk_factor_value>=25.3 & risk_factor_value<=25.7) %>% 
  summarise(prev = mean(fit),
            lb = mean(lwrS),
            ub = mean(uprS))


##Obtain estimates for E.bieneusi prevalence at 32 degrees C 
pathogen_data %>% 
  filter(!is.na(pos_E.bieneusi)) %>%
  filter(risk_factor=="temp_weekavg_2weeklag_C" & risk_factor_value>=31.6 & risk_factor_value<=32.4) %>% 
  summarise(prev = mean(fit),
            lb = mean(lwrS),
            ub = mean(uprS))



##Obtain estimates for Sapovirus prevalence at 19 degrees C 
pathogen_data %>% 
  filter(!is.na(pos_Sapovirus)) %>%
  filter(risk_factor=="temp_weekavg_1weeklag_C" & risk_factor_value>=18.6 & risk_factor_value<=19.4) %>% 
  summarise(prev = mean(fit),
            lb = mean(lwrS),
            ub = mean(uprS))

##Obtain estimates for Sapovirus prevalence at 30 degrees C 
pathogen_data %>% 
  filter(!is.na(pos_Sapovirus)) %>%
  filter(risk_factor=="temp_weekavg_1weeklag_C" & risk_factor_value>=29.6 & risk_factor_value<=30.4) %>% 
  summarise(prev = mean(fit),
            lb = mean(lwrS),
            ub = mean(uprS))

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()
