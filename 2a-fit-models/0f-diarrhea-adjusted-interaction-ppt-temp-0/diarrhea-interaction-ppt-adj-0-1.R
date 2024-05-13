#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Effect modification evaluation of temp on precipitation for diarrhea outcome
#######################################

rm(list=ls())

## configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

d_diar = readRDS(paste0(clean_offset_data_dir, clean_diarr_merged_offset))

d_diar = d_diar %>% filter(intervention==0)

analysis = "diarrhea-adjusted-interaction-ppt-0"

# make table of outcome and risk factors
diar_tbl = data.frame(outcome = rep("diar7d", length(risk_factors))) %>% 
  arrange(outcome) %>% 
  mutate(risk_factor = risk_factors)

tbl_adj <- map_dfr(1:dim(diar_tbl)[1], function(x) {
  outcome = diar_tbl[x, "outcome"]
  risk_factor = diar_tbl[x, "risk_factor"]
  w = get_covariate_list(outcome_cat = "diarrhea", risk_factor = risk_factor)
  out = data.frame(outcome = outcome, risk_factor = risk_factor) %>%
    mutate(w = list(w))
  return(out)
})

# ppt_EM = expand_grid(risk_factor = c(#"ppt_week_sum_0weeklag_C", "ppt_week_sum_1weeklag_C", 
#                                      #"ppt_week_sum_2weeklag_C", "ppt_week_sum_3weeklag_C",
#                                      "heavyrain_0weeklag", "heavyrain_1weeklag", 
#                                      "heavyrain_2weeklag","heavyrain_3weeklag",
#                                      "heavyrain_0weeklag_90", "heavyrain_1weeklag_90", 
#                                      "heavyrain_2weeklag_90","heavyrain_3weeklag_90",
#                                      "ppt_week_sum_0weeklag_median",       
#                                      "ppt_week_sum_1weeklag_median",     
#                                      "ppt_week_sum_2weeklag_median",   
#                                      "ppt_week_sum_3weeklag_median",
#                                      "ppt_week_sum_0weeklag_75",       
#                                      "ppt_week_sum_1weeklag_75",     
#                                      "ppt_week_sum_2weeklag_75",   
#                                      "ppt_week_sum_3weeklag_75",
#                                      "ppt_week_sum_0weeklag_90",       
#                                      "ppt_week_sum_1weeklag_90",     
#                                      "ppt_week_sum_2weeklag_90",   
#                                      "ppt_week_sum_3weeklag_90"),
#                      interaction_term = c(interaction.terms)) 

ppt_EM <- expand_grid(risk_factor = c("temp_weekavg_0weeklag_C",           
                                      "temp_weekavg_1weeklag_C",                
                                      "temp_weekavg_2weeklag_C",                  
                                      "temp_weekavg_3weeklag_C",
                                      "temp_monthavg_0weeklag_C", 
                                      "temp_monthavg_1weeklag_C",
                                      "temp_monthavg_2weeklag_C", 
                                      "temp_monthavg_3weeklag_C"),
                      interaction_term = c("heavyrain_0weeklag", "heavyrain_1weeklag", 
                                           "heavyrain_2weeklag","heavyrain_3weeklag",
                                           "heavyrain_0weeklag_90", "heavyrain_1weeklag_90",
                                           "heavyrain_2weeklag_90","heavyrain_3weeklag_90",
                                           "ppt_week_sum_0weeklag_median",
                                           "ppt_week_sum_1weeklag_median",
                                           "ppt_week_sum_2weeklag_median",
                                           "ppt_week_sum_3weeklag_median",
                                           "ppt_week_sum_0weeklag_75",
                                           "ppt_week_sum_1weeklag_75",
                                           "ppt_week_sum_2weeklag_75",
                                           "ppt_week_sum_3weeklag_75",
                                           "ppt_week_sum_0weeklag_90",
                                           "ppt_week_sum_1weeklag_90",
                                           "ppt_week_sum_2weeklag_90",
                                           "ppt_week_sum_3weeklag_90"))


EM_table = tbl_adj %>%
  left_join(ppt_EM, by = "risk_factor") %>%
  filter(!is.na(interaction_term)) %>%  # we're only running interaction analysis, so remove any rows without an interaction term
  ## These 0 week lags were intended for pathogen-specific analyses only. No need to run them for diarrhea!
  filter(!grepl("0weeklag", risk_factor), !grepl("0weeklag", interaction_term)) %>%
  filter(!grepl("weeklag", risk_factor) | 
          (grepl("weeklag", risk_factor) & !grepl("weeklag", interaction_term)) |
          (grepl("1weeklag", risk_factor) & grepl("1weeklag", interaction_term)) | 
          (grepl("2weeklag", risk_factor) & grepl("2weeklag", interaction_term)) | 
          (grepl("3weeklag", risk_factor) & grepl("3weeklag", interaction_term)))

print(EM_table)

## function to fit model, save results, run check script and produce fit checking output file.
fit_diarrhea_gam = function(diarrhea_tbl_row) {
  print("Fitting GAM")
  # diarrhea_tbl_row = diarrhea_tbl_adj[1,]
  x <- fit_gam_interaction(df = d_diar,
                           y = as.character(diarrhea_tbl_row[["outcome"]]),
                           a = as.character(diarrhea_tbl_row[["risk_factor"]]),
                           w = unlist(diarrhea_tbl_row[["w"]]),
                           interaction_term = as.character(diarrhea_tbl_row[["interaction_term"]]),
                           family = "poisson")
  print("Saving results")
  saveRDS(x, paste0(sherlock_results_dir,
                    "gam_outputs/", analysis, "/gam_",
                    str_replace(diarrhea_tbl_row[["outcome"]], "\\.", "_"),
                    "_0_",
                    diarrhea_tbl_row[["risk_factor"]],
                    ## 2 new lines inserted here
                    "_by_", 
                    diarrhea_tbl_row[["interaction_term"]],
                    ".RDS"))
  # if(x$message %in% model_pass) {
  #   print("Running check")
  #   check_gam_fit(outcome = str_replace(diarrhea_tbl_row[["outcome"]], "\\.", "_"), 
  #                 risk_factor = diarrhea_tbl_row[["risk_factor"]],
  #                 # 1 new line inserted here
  #                 interaction_term = diarrhea_tbl_row[["interaction_term"]],
  #                 output_type = "pdf",
  #                 analysis = analysis,
  #                 plot = "all", ## changed this from "only risk factor" to "all" so that all categories of interaction are plotted for factor interaction terms
  #                 # added interaction terms to filenames in lines 93-94 and 96-97
  #                 results_dir = sherlock_results_dir)
  #   gam_check_file_old_name = paste0(sherlock_results_dir, "gam_check/", analysis,"/gam_check_", diarrhea_tbl_row[["outcome"]], "_", diarrhea_tbl_row[["risk_factor"]], "_by_", diarrhea_tbl_row[["interaction_term"]], ".pdf")
  #   gam_check_file_new_name = paste0(sherlock_results_dir, "gam_check/", analysis,"/gam_check_", diarrhea_tbl_row[["outcome"]],"_0_", diarrhea_tbl_row[["risk_factor"]], "_by_", diarrhea_tbl_row[["interaction_term"]], ".pdf")
  #   file.rename(from = gam_check_file_old_name, to = gam_check_file_new_name)
  # } else {
  #   print("Skipping gam check because model not successful")
  # }
  # gam_outputs_file_old_name = paste0(sherlock_results_dir, "gam_outputs/", analysis, "/gam_", str_replace(diarrhea_tbl_row[["outcome"]], "\\.", "_"), "_", diarrhea_tbl_row[["risk_factor"]], "_by_", diarrhea_tbl_row[["interaction_term"]], ".RDS")
  # gam_outputs_file_new_name = paste0(sherlock_results_dir,"gam_outputs/", analysis, "/gam_", str_replace(diarrhea_tbl_row[["outcome"]], "\\.", "_"),"_0_", diarrhea_tbl_row[["risk_factor"]], "_by_", diarrhea_tbl_row[["interaction_term"]], ".RDS")
  # file.rename(from = gam_outputs_file_old_name, to = gam_outputs_file_new_name)
}


nb_row=nrow(EM_table)
print(nb_row)
missing <- vector(mode='list', length=1)

try_fit_diarrhea_gam <- function(diarrhea_tbl_row) {
  out <- tryCatch(
    {
      fit_diarrhea_gam(diarrhea_tbl_row = diarrhea_tbl_row)
    },
    error=function(cond) {
      message("There was an error.")
      message(cond)
      return(NA)
    },
    finally = {
      message("Trycatch is finished.")
    }
  )
  return(out)
}

registerDoParallel(detectCores())
foreach(i=1:nb_row) %dopar% {
  print(EM_table[i,])
  if (EM_table[i,2] %in% names(d_diar)){
    try_fit_diarrhea_gam(diarrhea_tbl_row = EM_table[i,])
  } else {
    print("Error: missing risk factor")
  }
}
stopImplicitCluster()
