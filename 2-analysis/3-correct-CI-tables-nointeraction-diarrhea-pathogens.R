#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# for models that did not converge properly
# due to sparsity, rerunning with glm 
# (all use binary risk factors)
#######################################
rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/0-utils/2-categorical-rf-tables-functions.R"))


## Get list of models that did not converge properly

prev_table <- readRDS(paste0(data_dir, "intermediate_RDS_files/old_PR_tables.RDS"))

failed <- prev_table %>%
  filter(SE < 0.02)

# load results objects --------------------------------------------------------
# the analysis will use the same dataset that was already processed for analysis for 
# each risk factor/outcome (e.g. covariates already pre-screened, missing observations removed, etc.)
# Diarrhea - unadjusted models
diar_prop_seas_sf_750 = readRDS(paste0(offset_results_path,"gam_outputs/diarrhea-unadjusted-0/gam_diar7d_0_prop_detected_seasonal_surface_water_750_median.RDS"))
diar_prop_eph_sf_750 = readRDS(paste0(offset_results_path,"gam_outputs/diarrhea-unadjusted-0/gam_diar7d_0_prop_detected_ephemeral_surface_water_750_median.RDS"))
diar_prop_any_sf_750 = readRDS(paste0(offset_results_path,"gam_outputs/diarrhea-unadjusted-0/gam_diar7d_0_prop_detected_any_surface_water_750_median.RDS"))
diar_prop_any_sf_500 = readRDS(paste0(offset_results_path,"gam_outputs/diarrhea-unadjusted-0/gam_diar7d_0_prop_detected_any_surface_water_500_median.RDS"))

diar_temp_weekmax_1weeklag = readRDS(paste0(offset_results_path, "gam_outputs/diarrhea-unadjusted-0/gam_diar7d_0_temp_weekmax_1weeklag_median.RDS"))
diar_temp_weekmax_2weeklag = readRDS(paste0(offset_results_path, "gam_outputs/diarrhea-unadjusted-0/gam_diar7d_0_temp_weekmax_2weeklag_median.RDS"))
diar_temp_weekmax_3weeklag = readRDS(paste0(offset_results_path, "gam_outputs/diarrhea-unadjusted-0/gam_diar7d_0_temp_weekmax_3weeklag_median.RDS"))

diar_temp_weekavg_1weeklag = readRDS(paste0(offset_results_path, "gam_outputs/diarrhea-unadjusted-0/gam_diar7d_0_temp_weekavg_1weeklag_median.RDS"))
diar_temp_weekmin_1weeklag = readRDS(paste0(offset_results_path, "gam_outputs/diarrhea-unadjusted-0/gam_diar7d_0_temp_weekmin_1weeklag_median.RDS"))
diar_temp_weekmin_2weeklag = readRDS(paste0(offset_results_path, "gam_outputs/diarrhea-unadjusted-0/gam_diar7d_0_temp_weekmin_2weeklag_median.RDS"))


######################################################################
## Diarrhea - unadjusted
######################################################################
#diar_prop_seas_sf_750 
glm_diar_prop_seas_sf_750 = fit_clustered_glm(
  y = diar_prop_seas_sf_750$y,
  a = diar_prop_seas_sf_750$a,
  data = diar_prop_seas_sf_750$model_input_data,
  family = diar_prop_seas_sf_750$gam_fit$gam$family,
  idvar = "dataid"
)

#diar_prop_eph_sf_750 
glm_diar_prop_eph_sf_750 = fit_clustered_glm(
  y = diar_prop_eph_sf_750$y,
  a = diar_prop_eph_sf_750$a,
  data = diar_prop_eph_sf_750$model_input_data,
  family = diar_prop_eph_sf_750$gam_fit$gam$family,
  idvar = "dataid"
)

#diar_prop_any_sf_750 
glm_diar_prop_any_sf_750 = fit_clustered_glm(
  y = diar_prop_any_sf_750$y,
  a = diar_prop_any_sf_750$a,
  data = diar_prop_any_sf_750$model_input_data,
  family = diar_prop_any_sf_750$gam_fit$gam$family,
  idvar = "dataid"
)

# diar_prop_any_sf_500 
glm_diar_prop_any_sf_500 = fit_clustered_glm(
  y = diar_prop_any_sf_500$y,
  a = diar_prop_any_sf_500$a,
  data = diar_prop_any_sf_500$model_input_data,
  family = diar_prop_any_sf_500$gam_fit$gam$family,
  idvar = "dataid"
)

#diar_temp_weekmax_1wk
glm_diar_temp_weekmax_1wk = fit_clustered_glm(
  y = diar_temp_weekmax_1weeklag$y,
  a = diar_temp_weekmax_1weeklag$a,
  data = diar_temp_weekmax_1weeklag$model_input_data,
  family = diar_temp_weekmax_1weeklag$gam_fit$gam$family,
  idvar = "dataid"
)

#diar_temp_weekmax_2wk
glm_diar_temp_weekmax_2wk = fit_clustered_glm(
  y = diar_temp_weekmax_2weeklag$y,
  a = diar_temp_weekmax_2weeklag$a,
  data = diar_temp_weekmax_2weeklag$model_input_data,
  family = diar_temp_weekmax_2weeklag$gam_fit$gam$family,
  idvar = "dataid"
)

#diar_temp_weekmax_3wk
glm_diar_temp_weekmax_3wk = fit_clustered_glm(
  y = diar_temp_weekmax_3weeklag$y,
  a = diar_temp_weekmax_3weeklag$a,
  data = diar_temp_weekmax_3weeklag$model_input_data,
  family = diar_temp_weekmax_3weeklag$gam_fit$gam$family,
  idvar = "dataid"
)

#diar_temp_weekavg_1wk
glm_diar_temp_weekavg_1wk = fit_clustered_glm(
  y = diar_temp_weekavg_1weeklag$y,
  a = diar_temp_weekavg_1weeklag$a,
  data = diar_temp_weekavg_1weeklag$model_input_data,
  family = diar_temp_weekavg_1weeklag$gam_fit$gam$family,
  idvar = "dataid"
)

#diar_temp_weekmin_1wk
glm_diar_temp_weekmin_1wk = fit_clustered_glm(
  y = diar_temp_weekmin_1weeklag$y,
  a = diar_temp_weekmin_1weeklag$a,
  data = diar_temp_weekmin_1weeklag$model_input_data,
  family = diar_temp_weekmin_1weeklag$gam_fit$gam$family,
  idvar = "dataid"
)

#diar_temp_weekmin_2wk
glm_diar_temp_weekmin_2wk = fit_clustered_glm(
  y = diar_temp_weekmin_2weeklag$y,
  a = diar_temp_weekmin_2weeklag$a,
  data = diar_temp_weekmin_2weeklag$model_input_data,
  family = diar_temp_weekmin_2weeklag$gam_fit$gam$family,
  idvar = "dataid"
)


# combine results ---------------------------------------
corrected_data <- bind_rows(glm_diar_prop_seas_sf_750,
                            glm_diar_prop_eph_sf_750,
                            glm_diar_prop_any_sf_750,
                            glm_diar_prop_any_sf_500,
                            glm_diar_temp_weekmax_1wk,
                            glm_diar_temp_weekmax_2wk,
                            glm_diar_temp_weekmax_3wk,
                            glm_diar_temp_weekavg_1wk,
                            glm_diar_temp_weekmin_1wk,
                            glm_diar_temp_weekmin_2wk) %>%
  rename(outcome_name = Outcome, risk_factor = RF) %>%
  mutate(RF_Type = case_when(
    # grepl("heavyrain", risk_factor) ~ "heavy rain",
    grepl("prop_detected", risk_factor) ~ "higher than median proportion",
    # grepl("ppt_week_sum", risk_factor) ~ "above median",
    grepl("temp_week", risk_factor) ~ "above median",
    RF_Type == "close" ~ "Close vs. Far",
    RF_Type == "medium" ~ "Medium vs. Far",
    TRUE ~ RF_Type
  )) %>%
  select(-estimate_se) %>%
  rename(PR.Lower = PR_lower,
         PR.Upper = PR_upper)

output = failed %>%
  select(-(`PR.Lower`:PR), -SE) %>%
  left_join(corrected_data %>%
              select(-Model))

saveRDS(output, paste0(data_dir, "intermediate_RDS_files/corrected_models_nointeraction.RDS"))


updated_table = prev_table %>%
  filter(SE > 0.02) %>%
 bind_rows(output)

saveRDS(updated_table, paste0(data_dir, "corrected_PR_tables_nointeraction.RDS"))

