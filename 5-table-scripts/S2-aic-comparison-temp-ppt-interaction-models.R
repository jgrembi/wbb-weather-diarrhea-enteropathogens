#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Script to assess the AIC/BIC of models that include interaction term 
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

# read in diarrhea results --------------------------------------------------
adjusted_directory <- paste0(offset_results_path, "gam_outputs/diarrhea-adjusted-0")
adjusted_files <- list.files(adjusted_directory, full.names = T) 

adjusted_temp_files <- adjusted_files[grepl("temp", adjusted_files) & grepl("_C", adjusted_files) & grepl("avg", adjusted_files)]


no_interaction_directory <- paste0(offset_results_path, "gam_outputs/diarrhea-adjusted-nointeraction-ppt-temp-0")
no_interaction_files <- list.files(no_interaction_directory, full.names = T,  pattern = ".RDS")

interaction_directory <-  paste0(offset_results_path, "gam_outputs/diarrhea-adjusted-interaction-ppt-0")
interaction_files <- list.files(interaction_directory, full.names = T,  pattern = ".RDS")


get_aic <- function(file) {
  print(file)
  model_res <- readRDS(file)
  
  if (!is.null(model_res$interaction_term)) {
    interaction_term = model_res$interaction_term
  } else if (grepl("_and_", file)) {
    interaction_term = gsub(".RDS", "", str_split_1(str_split_1(file, "/")[11], "_and_")[2])
  } else {
    interaction_term = NA
  }
  if (!any(is.na(model_res$spatial_gam_fit))) {
    fit <- model_res$spatial_gam_fit
    fit_type = "spatial"
  } else {
    fit <- model_res$gam_fit
    fit_type = "non-spatial"
  }
  
  if(model_res$message %in% c("Adjusted/interaction model could not be run due to data sparsity", 
                              "Adjusted model not run because no covariates were associated with the outcome at P< 0.1", 
                              "Non-spatial GAM failed to converge")) {
    aic = bic = NA
  } else {
    aic = AIC(fit$mer)
    bic = BIC(fit$mer)
  }
  w_included = c(model_res$w_included)
  
  out <- data.frame(outcome = as.character(model_res$y),
                    risk_factor = as.character(model_res$a),
                    interaction_term = interaction_term,
                    fit_type = fit_type,
                    aic = aic,
                    bic = bic,
                    message = model_res$message) %>%
    mutate(w_included = list(w_included))
  
  return(out)
}


## get aic for adjusted temp models
temp_adj <- map(adjusted_temp_files, \(x) get_aic(file = x)) %>%
  list_rbind() %>%
  mutate(model = "temp + A")

## get aic for temp models adjusted for ppt 
ppt_temp <- map(no_interaction_files, \(x) get_aic(file = x)) %>%
  list_rbind() %>%
  mutate(model = "temp + ppt + A")

## get aic for temp models adjusted for ppt & including an interaction term
ppt_temp_interaction <- map(interaction_files, \(x) get_aic(file = x)) %>%
  list_rbind() %>%
  mutate(model = "temp + ppt + temp*ppt + A")


res = bind_rows(temp_adj, ppt_temp) %>%
  bind_rows(ppt_temp_interaction) %>%
  mutate(model = factor(model, levels = c("temp + A", "temp + ppt + A", "temp + ppt + temp*ppt + A")),
         aic = round(aic, 2),
         bic = round(bic, 2),
         formula = paste0(" ~ ", risk_factor,
                          ifelse(model == "temp + ppt + A", paste0(" + ", interaction_term),
                                 ifelse(model == "temp + ppt + temp*ppt + A", paste0(" + ", interaction_term, " + ", risk_factor, "*", interaction_term), "")))) %>%
  arrange(risk_factor, formula)

table_out <- res %>%
  select(risk_factor, formula, fit_type, aic, bic)

## now load pathogen data
paths_adj_directory <- paste0(offset_results_path, "gam_outputs/pathogens-adjusted")
paths_adj_files <- list.files(paths_adj_directory, full.names = T, pattern = ".RDS") 
paths_adj_temp_files <- paths_adj_files[grepl("temp", paths_adj_files) & grepl("avg", paths_adj_files) & grepl("_C", paths_adj_files)]

## get aic for adjusted temp models
paths_temp_adj <- map(paths_adj_temp_files, \(x) get_aic(file = x)) %>%
  list_rbind() %>%
  mutate(model = "temp + A")


paths_no_interaction_directory <- paste0(offset_results_path, "gam_outputs/pathogens-adjusted-nointeraction-ppt-temp")
paths_no_interaction_files <- list.files(paths_no_interaction_directory, full.names = T,  pattern = ".RDS")
## get aic for temp models adjusted for ppt 
paths_ppt_temp <- map(paths_no_interaction_files, \(x) get_aic(file = x)) %>%
  list_rbind() %>%
  mutate(model = "temp + ppt + A")


paths_interaction_directory <-  paste0(offset_results_path, "gam_outputs/pathogens-adjusted-interaction-ppt")
paths_interaction_files <- list.files(paths_interaction_directory, full.names = T,  pattern = ".RDS")
## get aic for temp models adjusted for ppt & including an interaction term
paths_ppt_temp_interaction <- map(paths_interaction_files, \(x) get_aic(file = x)) %>%
  list_rbind() %>%
  mutate(model = "temp + ppt + temp*ppt + A")


drop_ecoli <- c("pos_aEPEC", "pos_tEPEC", "pos_ST_ETEC", "pos_LT_ETEC")
path_res = bind_rows(paths_temp_adj, paths_ppt_temp) %>%
  bind_rows(paths_ppt_temp_interaction) %>%
  mutate(model = factor(model, levels = c("temp + A", "temp + ppt + A", "temp + ppt + temp*ppt + A")),
         aic = round(aic, 2),
         bic = round(bic, 2),
        formula = paste0(" ~ ", risk_factor,
                        ifelse(model == "temp + ppt + A", paste0(" + ", interaction_term),
                              ifelse(model == "temp + ppt + temp*ppt + A", paste0(" + ", interaction_term, " + ", risk_factor, "*", interaction_term), "")))) %>%
  arrange(risk_factor, formula) %>%
  filter(!outcome %in% drop_ecoli) %>%
  mutate(outcome = gsub("_any", "", outcome))



risk_factor_levels <- c( "temp_weekavg_0weeklag_C",           
                         "temp_weekavg_1weeklag_C",                
                         "temp_weekavg_2weeklag_C",                  
                         "temp_weekavg_3weeklag_C", 
                         "temp_monthavg_0weeklag_C", 
                         "temp_monthavg_1weeklag_C",
                         "temp_monthavg_2weeklag_C", 
                         "temp_monthavg_3weeklag_C")

all_table_out <- bind_rows(path_res, res) %>%
  filter(!grepl("_median", risk_factor)) %>%
  mutate(outcome = factor(gsub("pos_", "", outcome), levels = c("diar7d", "Adenovirus40_41", "Norovirus", "Sapovirus", "virus",
                                                               "Cryptosporidium","E.bieneusi", "Giardia", "parasite",
                                                               "Aeromonas", "B.fragilis","Campylobacter", "C.difficile","Plesiomonas","Shigella_EIEC",
                                                               "EAEC","EPEC","ETEC","STEC"))) %>%
  select(risk_factor, interaction_term, formula, model, aic, bic, outcome) %>%
  pivot_wider(names_from = outcome, values_from = c("aic", "bic"), names_vary = "slowest") %>%
  mutate(risk_factor = factor(risk_factor, levels = risk_factor_levels)) %>%
  arrange(risk_factor, formula)


col_names <- union(gsub("pos_", "", unique(path_res$outcome)), unique(res$outcome))
col_order <- c("diar7d", "Adenovirus40_41", "Norovirus", "Sapovirus", "virus",
               "Cryptosporidium","E.bieneusi", "Giardia", "parasite",
               "Aeromonas", "B.fragilis","Campylobacter", "C.difficile","Plesiomonas",
               "Shigella_EIEC","EAEC","EPEC","ETEC","STEC")


aic_table <- all_table_out %>%
  select(!contains("bic")) %>%
  mutate_at(vars(contains("aic")), function(x) formatC(as.numeric(x), format = 'f', flag='0', digits = 2)) %>%
  rename_at(vars(contains("aic")), function(x) gsub("aic_", "", x)) %>%
  mutate(risk_factor = factor(risk_factor, levels = risk_factor_levels),
         interaction_term = paste0("   ", interaction_term)) %>%
  select(risk_factor, interaction_term, model, formula, all_of(col_order)) %>%
  mutate(formula = as.character(paste0("Y", formula))) %>%
  rename("7-day Diarrhea" = diar7d, "Adenovirus 40/41" = Adenovirus40_41, "Any Virus" = virus,
         "Any Parasite" = parasite, "Shigella/EIEC" = Shigella_EIEC,
         "B. fragilis" = "B.fragilis", "C. difficile" = "C.difficile",
         "E. bieneusi" = "E.bieneusi") %>%
  rename("Temperature variable" = risk_factor, "Precipitation variable" = interaction_term)

write.csv(x = aic_table, 
          file = paste0(here::here(), "/6-tables/aic-all-models-ppt-temp-interaction-results.csv"),
          row.names = FALSE)

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()