#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Effect modification evaluation of temp on precipitation for pathogen outcomes
#######################################

rm(list=ls())


# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

d_pathogens = readRDS(paste0(clean_offset_data_dir, clean_pathogen_merged_offset))

analysis = "pathogens-adjusted-interaction-ppt"

pathogens_outcomes = c("pos_virus","pos_parasite","pos_EAEC","pos_ETEC_any","pos_EPEC_any","pos_STEC","pos_Aeromonas","pos_B.fragilis","pos_Campylobacter","pos_C.difficile","pos_Plesiomonas","pos_Shigella_EIEC","pos_Adenovirus40_41","pos_Norovirus","pos_Sapovirus","pos_Cryptosporidium","pos_E.bieneusi","pos_Giardia")

#make table with one line for each outcome-risk factor pair
pathogens_tbl = data.frame(outcome = rep(pathogens_outcomes, length(risk_factors))) %>% 
  arrange(outcome) %>% 
  mutate(risk_factor = rep(risk_factors, length(pathogens_outcomes)))

#make table with one line for each outcome-risk factor pair and include list of adjustment covariates as another column
pathogens_tbl_adj <- unique(map_dfr(1:dim(pathogens_tbl)[1], function(x) {
  outcome = pathogens_outcomes
  risk_factor = pathogens_tbl[x, "risk_factor"]
  w = get_covariate_list(outcome_cat = "pathogens", risk_factor = risk_factor)
  out = data.frame(outcome = outcome, risk_factor = risk_factor) %>%
    mutate(w = list(w))
  return(out)
}))

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


EM_table = pathogens_tbl_adj %>%
  left_join(ppt_EM, by = "risk_factor") %>%
  filter(!is.na(interaction_term)) %>%  # we're only running interaction analysis, so remove any rows without an interaction term
  filter(!grepl("weeklag", risk_factor) | 
           (grepl("weeklag", risk_factor) & !grepl("weeklag", interaction_term)) |
           (grepl("0weeklag", risk_factor) & grepl("0weeklag", interaction_term)) | 
           (grepl("1weeklag", risk_factor) & grepl("1weeklag", interaction_term)) | 
           (grepl("2weeklag", risk_factor) & grepl("2weeklag", interaction_term)) | 
           (grepl("3weeklag", risk_factor) & grepl("3weeklag", interaction_term)))



#Create empty dataframe for error messages
pathogens_tbl_message <- data.frame(matrix(ncol = 4, nrow = 0))
write.table(pathogens_tbl_message, paste0(here::here(),"/2-analysis/0i-pathogens-adjusted-interaction-ppt-temp/pathogens-interaction-tbl_message_3.csv"), sep = ',', row.names = FALSE )
colnames(pathogens_tbl_message) <- c('outcome', 'risk_factor', 'interaction_term', 'message')



## function to fit model, save results, run check script and produce fit checking output file.
fit_pathogens_gam = function(pathogens_tbl_row) {
  print("Fitting GAM")
  print(pathogens_tbl_row[["outcome"]])
  x <- fit_gam_interaction(df = d_pathogens,
                           y = as.character(pathogens_tbl_row[["outcome"]]),
                           a = as.character(pathogens_tbl_row[["risk_factor"]]),
                           w = unlist(pathogens_tbl_row[["w"]]),
                           interaction_term = pathogens_tbl_row[["interaction_term"]],
                           random_intercept="clusterid",
                           family = "poisson")
  print("Saving results")
  saveRDS(x, paste0(sherlock_results_dir,
                    "gam_outputs/", analysis, "/gam_",
                    str_replace(pathogens_tbl_row[["outcome"]], "\\.", "_"),
                    "_",
                    pathogens_tbl_row[["risk_factor"]],
                    ## 2 new lines inserted here
                    "_by_", 
                    pathogens_tbl_row[["interaction_term"]],
                    ".RDS"))
  # if(x$message %in% model_pass) {
  #   print("Running check")
  #   check_gam_fit(outcome = str_replace(pathogens_tbl_row[["outcome"]], "\\.", "_"),
  #                 risk_factor = pathogens_tbl_row[["risk_factor"]],
  #                 output_type = "pdf",
  #                 analysis = analysis,
  #                 plot = "only risk factor",
  #                 results_dir = sherlock_results_dir)
  #   
  # } else {
  #   print("Skipping gam check because model not successful")
  # }
  pathogens_tbl_message = read.csv(paste0(here::here(),"/2-analysis/0i-pathogens-adjusted-interaction-ppt-temp/pathogens-interaction-tbl_message_3.csv"))
  pathogens_tbl_row = c(pathogens_tbl_row[["outcome"]], pathogens_tbl_row[["risk_factor"]], pathogens_tbl_row[["interaction_term"]], x$message)
  pathogens_tbl_message[nrow(pathogens_tbl_message)+1,] <- pathogens_tbl_row
  write.table(pathogens_tbl_message, paste0(here::here(),"/2-analysis/0i-pathogens-adjusted-interaction-ppt-temp/pathogens-interaction-tbl_message_3.csv"), sep = ',', row.names = FALSE)
}


nb_row=nrow(EM_table)
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

#registerDoParallel(detectCores())
#foreach(i=1:nb_row) %dopar% {
for (i in (361:540)){
  print(EM_table[i,])
  if (EM_table[i,2] %in% names(d_pathogens)){
    try_fit_pathogens_gam(pathogens_tbl_row = EM_table[i,])
  } else {
    print("Error: missing risk factor")
  }
}

#stopImplicitCluster()
