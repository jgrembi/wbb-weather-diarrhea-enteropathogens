#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# GAM analysis for diarrhea outcomes - adjusted
#######################################

rm(list=ls())


# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

d_diarrhea = readRDS(paste0(clean_offset_data_dir, clean_diarr_merged_offset))

#filter for intervention = 0
d_diarrhea = d_diarrhea %>% filter(intervention==0)

analysis = "diarrhea-adjusted-0"

diarrhea_outcomes = c("diar7d")

#make table with one line for each outcome-risk factor pair
diarrhea_tbl = data.frame(outcome = rep(diarrhea_outcomes, length(risk_factors))) %>% 
  arrange(outcome) %>% 
  mutate(risk_factor = rep(risk_factors, length(diarrhea_outcomes))) %>%
  filter(!grepl("0weeklag", risk_factor))

#make table with one line for each outcome-risk factor pair and include list of adjustment covariates as another column
diarrhea_tbl_adj <- map_dfr(1:dim(diarrhea_tbl)[1], function(x) {
  outcome = diarrhea_outcomes
  risk_factor = diarrhea_tbl[x, "risk_factor"]
  w = get_covariate_list(outcome_cat = outcome, risk_factor = risk_factor)
  out = data.frame(outcome = outcome, risk_factor = risk_factor) %>%
    mutate(w = list(w))
  return(out)
})


## function to fit model, save results, run check script and produce fit checking output file.
fit_diarrhea_gam = function(diarrhea_tbl_row) {
  print("Fitting GAM")
  x <- fit_gam(df = d_diarrhea,
               y = as.character(diarrhea_tbl_row[["outcome"]]),
               a = as.character(diarrhea_tbl_row[["risk_factor"]]),
               w = unlist(diarrhea_tbl_row[["w"]]),
               family = "poisson")
  print("Saving results")
  saveRDS(x, paste0(sherlock_results_dir,
                    "gam_outputs/", analysis, "/gam_",
                    str_replace(diarrhea_tbl_row[["outcome"]], "\\.", "_"),
                    "_0_",
                    diarrhea_tbl_row[["risk_factor"]],
                    ".RDS"))
  # if(x$message %in% model_pass) {
  #   print("Running check")
  #   check_gam_fit(outcome = str_replace(diarrhea_tbl_row[["outcome"]], "\\.", "_"),
  #                 risk_factor = diarrhea_tbl_row[["risk_factor"]],
  #                 output_type = "pdf",
  #                 analysis = analysis,
  #                 plot = "only risk factor",
  #                 results_dir = sherlock_results_dir)
  #   gam_check_file_old_name = paste0(sherlock_results_dir, "/gam_check/", analysis,"/gam_check_", diarrhea_tbl_row[["outcome"]], "_", diarrhea_tbl_row[["risk_factor"]], ".pdf")
  #   gam_check_file_new_name = paste0(sherlock_results_dir, "gam_check/", analysis,"/gam_check_", diarrhea_tbl_row[["outcome"]],"_0_", diarrhea_tbl_row[["risk_factor"]], ".pdf")
  #   file.rename(from = gam_check_file_old_name, to = gam_check_file_new_name)
  # } else {
  #   print("Skipping gam check because model not successful")
  # }
}


nb_row=nrow(diarrhea_tbl_adj)
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
foreach(i=21:40) %dopar% {
# for (i in (25:nb_row)){
  print(diarrhea_tbl_adj[i,])
  if (diarrhea_tbl_adj[i,2] %in% names(d_diarrhea)){
    try_fit_diarrhea_gam(diarrhea_tbl_row = diarrhea_tbl_adj[i,])
  } else {
    print("Error: missing risk factor")
  }
}

stopImplicitCluster()


