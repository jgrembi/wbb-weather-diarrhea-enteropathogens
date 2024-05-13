#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Effect modification evaluation for age_cat vs diarrhea
#######################################

rm(list=ls())

## configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

d_diar = readRDS(paste0(clean_offset_data_dir, clean_diarr_merged_offset))

d_diar = d_diar %>% filter(intervention==0)

## Child age categories for diarrhea dataset include <1.5yr (this is actually 6mo - 1.4yr) and 1.5-5yr
analysis = "diarrhea-adjusted-interaction-age-0"

# make table of outcome and risk factors
diar_tbl = data.frame(outcome = rep("diar7d", length(risk_factors))) %>% 
  arrange(outcome) %>% 
  mutate(risk_factor = risk_factors)

tbl_adj <- map_dfr(1:dim(diar_tbl)[1], function(x) {
  outcome = diar_tbl[x, "outcome"]
  risk_factor = diar_tbl[x, "risk_factor"]
  w = get_covariate_list(outcome_cat = "diarrhea", risk_factor = risk_factor)
  out = data.frame(outcome = outcome, risk_factor = risk_factor) %>%
    mutate(interaction_term = "age_cat",
           w = list(w[w != "aged_C"]))
  return(out)
}) %>%
  filter(!grepl("0weeklag", risk_factor))

# tbl_adj <- tbl_adj %>%
  # filter(grepl("temp", risk_factor))

## function to fit model, save results, run check script and produce fit checking output file.
fit_diarrhea_gam = function(diarrhea_tbl_row) {
  print("Fitting GAM")
  # diarrhea_tbl_row = diarrhea_tbl_adj[1,]
  x <- fit_gam_interaction(df = d_diar,
               y = as.character(diarrhea_tbl_row[["outcome"]]),
               a = as.character(diarrhea_tbl_row[["risk_factor"]]),
               w = NULL,
               interaction_term = as.character(diarrhea_tbl_row[["interaction_term"]]),
               family = "poisson")
  print("Saving results")
  saveRDS(x, paste0(sherlock_results_dir,
                    "gam_outputs/", analysis, "/gam_",
                    str_replace(diarrhea_tbl_row[["outcome"]], "\\.", "_"),
                    "_0_",
                    diarrhea_tbl_row[["risk_factor"]],
                    "_by_", diarrhea_tbl_row[["interaction_term"]],
                    ".RDS"))
  # if(x$message %in% model_pass) {
  #   print("Running check")
  #   check_gam_fit(outcome = str_replace(diarrhea_tbl_row[["outcome"]], "\\.", "_"), 
  #                 risk_factor = diarrhea_tbl_row[["risk_factor"]],
  #                 interaction_term = diarrhea_tbl_row[["interaction_term"]], 
  #                 output_type = "pdf", 
  #                 analysis = analysis,
  #                 plot = "all",
  #                 results_dir = sherlock_results_dir)
  #   gam_check_file_old_name = paste0(sherlock_results_dir, "gam_check/", analysis,"/gam_check_", diarrhea_tbl_row[["outcome"]], "_", diarrhea_tbl_row[["risk_factor"]], "_by_", diarrhea_tbl_row[["interaction_term"]], ".pdf")
  #   gam_check_file_new_name = paste0(sherlock_results_dir, "gam_check/", analysis,"/gam_check_", diarrhea_tbl_row[["outcome"]],"_0_", diarrhea_tbl_row[["risk_factor"]], "_by_", diarrhea_tbl_row[["interaction_term"]], ".pdf")
  #   file.rename(from = gam_check_file_old_name, to = gam_check_file_new_name)
  # } else {
  #   print("Skipping gam check because model not successful")
  # }
  # gam_outputs_file_old_name = paste0(sherlock_results_dir, "gam_outputs/", analysis, "/gam_", str_replace(diarrhea_tbl_row[["outcome"]], "\\.", "_"), "_", diarrhea_tbl_row[["risk_factor"]], "_by_", diarrhea_tbl_row[["interaction_term"]], ".RDS")
  # gam_outputs_file_new_name = paste0(sherlock_results_dir, "gam_outputs/", analysis, "/gam_", str_replace(diarrhea_tbl_row[["outcome"]], "\\.", "_"),"_0_", diarrhea_tbl_row[["risk_factor"]], "_by_", diarrhea_tbl_row[["interaction_term"]], ".RDS")
  # file.rename(from = gam_outputs_file_old_name, to = gam_outputs_file_new_name)
}

nb_row=nrow(tbl_adj)
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
foreach(i=31:nb_row) %dopar% {
# for (i in (1:20)){
  print(tbl_adj[i,])
  if (tbl_adj[i,2] %in% names(d_diar)){
    try_fit_diarrhea_gam(diarrhea_tbl_row = tbl_adj[i,])
  } else {
    print("Error: missing risk factor")
  }
}

stopImplicitCluster()

