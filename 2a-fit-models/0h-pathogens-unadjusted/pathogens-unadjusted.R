#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# GAM analysis for pathogens outcomes - unadjusted
#######################################
rm(list=ls())


# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

d_pathogens = readRDS(paste0(clean_offset_data_dir, clean_pathogen_merged_offset))

analysis = "pathogens-unadjusted"

pathogens_outcomes = c("pos_virus","pos_parasite","pos_EAEC","pos_ST_ETEC","pos_ETEC_any","pos_LT_ETEC","pos_EPEC_any","pos_aEPEC","pos_tEPEC","pos_STEC","pos_Aeromonas","pos_B.fragilis","pos_Campylobacter","pos_C.difficile","pos_Plesiomonas","pos_Shigella_EIEC","pos_Adenovirus40_41","pos_Norovirus","pos_Sapovirus","pos_Cryptosporidium","pos_E.bieneusi","pos_Giardia")
pathogens_tbl = data.frame(outcome = rep(pathogens_outcomes, length(risk_factors))) %>% 
  arrange(outcome) %>% 
  mutate(risk_factor = rep(risk_factors, length(pathogens_outcomes)))

## This is for a special rerun to do the temp-month variables -- can delete for running regular script for analysis
# pathogens_tbl = pathogens_tbl %>% filter(grepl("temp", risk_factor), grepl("_median", risk_factor))

## function to fit model, save results, run check script and produce fit checking output file.
fit_pathogens_gam = function(pathogens_tbl_row) {
  print("Fitting GAM")
  x <- fit_gam(df = d_pathogens,
               y = as.character(pathogens_tbl_row[["outcome"]]),
               a = as.character(pathogens_tbl_row[["risk_factor"]]),
               w = NULL,
               random_intercept="clusterid",
               family = "poisson")
  print("Saving results")
  saveRDS(x, paste0(sherlock_results_dir,
                    "gam_outputs/", analysis, "/gam_",
                    str_replace(pathogens_tbl_row[["outcome"]], "\\.", "_"),
                    "_",
                    pathogens_tbl_row[["risk_factor"]],
                    ".RDS"))
  # if(x$message %in% model_pass) {
  #   print("Running check")
  #   check_gam_fit(outcome = str_replace(pathogens_tbl_row[["outcome"]], "\\.", "_"),
  #                 risk_factor = pathogens_tbl_row[["risk_factor"]],
  #                 output_type = "pdf",
  #                 analysis = "pathogens-unadjusted",
  #                 plot = "only risk factor",
  #                 results_dir = sherlock_results_dir)
  # } else {
  #   print("Skipping gam check because model not successful")
  # }
}
  
nb_row=nrow(pathogens_tbl)
print(nb_row)
missing <- vector(mode='list', length=1)

try_fit_pathogens_gam <- function(pathogens_tbl_row) {
  out <- tryCatch(
    {
      fit_pathogens_gam(pathogens_tbl_row = pathogens_tbl_row)
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
  print(pathogens_tbl[i,])
  if (pathogens_tbl[i,2] %in% names(d_pathogens)){
    try_fit_pathogens_gam(pathogens_tbl_row = pathogens_tbl[i,])
  } else {
    print("Error: missing risk factor")
  }
}

stopImplicitCluster()
print(missing)

