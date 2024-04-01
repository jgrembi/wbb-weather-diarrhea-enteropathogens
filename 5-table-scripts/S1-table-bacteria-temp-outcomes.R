################################################################################
# WASH Benefits 
# spatial environmental risk factor analysis
#
# For use on Sherlock virtual machine (stored in 3-figure-scripts/intermediate-RDS-files).
# Generates intermediate RDS tables with PR and confidence intervals.
# 
# This script makes the supplemental table showing bacterial pathogen associations with temperature
#
################################################################################

rm(list=ls())
source(here::here("0-config.R"))
source(paste0(here::here(), "/0-utils/1-plot-functions.R"))
# define list of temperature variable names
temp_vars <- list("temp_weekavg_1weeklag_C")
# "temp_weekavg_2weeklag_C",
# "temp_weekavg_3weeklag_C",
# "temp_weekmax_1weeklag_C",
# "temp_weekmin_1weeklag_C"#,
# "temp_monthavg_0monthlag_C",
# "temp_monthmax_0monthlag_C",
# "temp_monthmin_0monthlag_C"
# )

# read in results --------------------------------------------------
# define results path in Box 
pathogen_results_dir <- paste0(offset_results_path, "gam_outputs/pathogens-adjusted/")

# bacterial pathogen outcomes
path_outcomes <- c(
  # "pos_aEPEC_",
  "pos_Aeromonas_",
  "pos_B_fragilis_",
  "pos_C_difficile_",
  "pos_Campylobacter_",
  "pos_EAEC_",
  "pos_ETEC_any_",
  "pos_EPEC_any_",
  # "pos_LT_ETEC_",
  "pos_Plesiomonas_",
  "pos_Shigella_EIEC_",
  # "pos_ST_ETEC_",
  "pos_STEC_")
  # "pos_tEPEC_")


#------------------------------------------------------
## Read in model results and generate predicted data using original data and model fits, 
## including simultaneous 95% CIs 
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


bacteria_temp_table_19 <- pathogen_data %>%
  filter(risk_factor == "temp_weekavg_1weeklag_C" & risk_factor_value>=18.6 & risk_factor_value<=19.4) %>%
  group_by(outcome) %>%
  summarise(prev = format(round(mean(fit), 1), nsmall = 1),
            lb = format(round(mean(lwrS), 1), nsmall = 1),
            ub = format(round(mean(uprS), 1), nsmall = 1),
            prev_CI_19 = paste0(prev, " (", lb, ", ", ub, ")"))

# bacteria_temp_table_32 <- pathogen_data %>%
#   filter(risk_factor == "temp_weekavg_1weeklag_C" & risk_factor_value>=31.6 & risk_factor_value<=32.4) %>%
#   group_by(outcome) %>%
#   summarise(prev = format(round(mean(fit), 1), nsmall = 1),
#             lb = format(round(mean(lwrS), 1), nsmall = 1),
#             ub = format(round(mean(uprS), 1), nsmall = 1),
#             prev_CI_32 = paste0(prev, " (", lb, ", ", ub, ")"))

bacteria_temp_table_30 <- pathogen_data %>%
  filter(risk_factor == "temp_weekavg_1weeklag_C" & risk_factor_value>=29.6 & risk_factor_value<=30.4) %>%
  group_by(outcome) %>%
  summarise(prev = format(round(mean(fit), 1), nsmall = 1),
            lb = format(round(mean(lwrS), 1), nsmall = 1),
            ub = format(round(mean(uprS), 1), nsmall = 1),
            prev_CI_30 = paste0(prev, " (", lb, ", ", ub, ")"))

bacteria_temp_table <- bacteria_temp_table_19 %>%
  select(outcome, prev_CI_19) %>%
  left_join(bacteria_temp_table_30 %>%
              select(outcome,prev_CI_30)) %>%
  mutate(outcome_clean = clean_pathogen_names(outcome)) %>%
  select(outcome_clean,prev_CI_19, prev_CI_30) %>%
  rename(Bacteria = outcome_clean, `Prevalence (95% CI) at 19 C` = prev_CI_19, `Prevalence (95% CI) at 30 C` = prev_CI_30)

write.csv(bacteria_temp_table, paste0(tab_dir, "S1-Table-bacteria-prevalence-temp-with-95CI.csv"))
