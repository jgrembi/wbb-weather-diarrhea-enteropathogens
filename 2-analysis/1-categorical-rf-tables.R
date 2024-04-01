#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens
#
# For use on Sherlock virtual machine (stored in data/intermediate-RDS-files).
# Generates intermediate RDS tables with PR and confidence intervals.
# 
# This script saves the output RDS table file to Sherlock. Some of the output RDS tables
# have abnormal CIs. The combined RDS tables file should then be uploaded to Box (figures-categorical-risk-factors/intermediate-RDS-files/).
# Correct-RDS-files-2.R fixes the abnormal CIs and uploads an updated version to Box, where the table file can then be used for creating plots.
# 
# The intermediate RDS files generated from this script have been uploaded to Box and can be accessed
# there.
#######################################
source(here::here("0-config.R"))
source(paste0(here::here(), "/0-utils/2-categorical-rf-tables-functions.R"))


#------------------------------------------------------
# The categorical-rf-tables and categorical-rf-table-functions scripts calculate tables for 
# pathogens and diarrhea outcomes. The scripts have been adjusted to support Sherlock 
# computing cluster instead of Box. 

outcome_subdir_binary <- c("diarrhea-0",
                           "diarrhea-0-negc",
                           rep("pathogens-", 22))

# Build paths to files in Sherlock to create data-frame
if(Sys.getenv("LMOD_SYSHOST")=="sherlock"){
  # Build path to subfolders in GROUP HOME
  sherlock_folder_path <- paste0(sherlock_results_dir, "gam_outputs")
} else {
  sherlock_folder_path <- paste0(offset_results_path, "gam_outputs")
}

if (!dir.exists(paste0(data_dir, "intermediate_RDS_files"))) dir.create(paste0(data_dir, "intermediate_RDS_files"))

all_subfolders <- list.dirs(sherlock_folder_path, recursive = F)

# Create list of un-adjusted directories 
unadj_dirs <- all_subfolders[grepl("unadj", all_subfolders) & !grepl("interaction", all_subfolders)]

# Create list of adjusted directories
adj_dirs <- all_subfolders[!grepl("unadj", all_subfolders) & !grepl("interaction", all_subfolders)]

max_length <- max(c(length(adj_dirs), length(unadj_dirs)))
outcome_dirs_df <- data.frame(unadjusted = c(unadj_dirs,                 # Create data frame with unequal vectors
                                             rep(NA, max_length - length(unadj_dirs))),
                              adjusted = c(adj_dirs,
                                           rep(NA, max_length - length(adj_dirs))))

#------------------------------------------------------
# Create lists with categorical risk factor variables 

#------------------------------------------------------
risk_factors_categorical = c(
  "heavyrain_0weeklag",
  "heavyrain_1weeklag",
  "heavyrain_2weeklag",
  "heavyrain_3weeklag",
  "heavyrain_0weeklag_90",
  "heavyrain_1weeklag_90",
  "heavyrain_2weeklag_90",
  "heavyrain_3weeklag_90",
  "distance_from_any_surface_water_tertile",
  "distance_from_seasonal_surface_water_tertile",     
  "distance_from_ephemeral_surface_water_tertile", 
  "prop_detected_any_surface_water_250_median",
  "prop_detected_seasonal_surface_water_250_median",
  "prop_detected_ephemeral_surface_water_250_median", 
  "prop_detected_any_surface_water_500_median",
  "prop_detected_seasonal_surface_water_500_median",
  "prop_detected_ephemeral_surface_water_500_median",
  "prop_detected_any_surface_water_750_median",
  "prop_detected_seasonal_surface_water_750_median",
  "prop_detected_ephemeral_surface_water_750_median",
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
  "ppt_week_sum_3weeklag_90",
  "temp_weekavg_0weeklag_median",
  "temp_weekavg_1weeklag_median",
  "temp_weekavg_2weeklag_median", 
  "temp_weekavg_3weeklag_median",
  "temp_weekmax_0weeklag_median",
  "temp_weekmax_1weeklag_median",
  "temp_weekmax_2weeklag_median", 
  "temp_weekmax_3weeklag_median",
  "temp_weekmin_0weeklag_median",
  "temp_weekmin_1weeklag_median",
  "temp_weekmin_2weeklag_median", 
  "temp_weekmin_3weeklag_median")

risk_factor_categorical_label = c(
  "Heavy Rain, 0 Week Lag",
  "Heavy Rain, 1 Week Lag",
  "Heavy Rain, 2 Week Lag",
  "Heavy Rain, 3 Week Lag",
  "Heavy Rain, 0 Week Lag - 90th percentile",
  "Heavy Rain, 1 Week Lag - 90th percentile",
  "Heavy Rain, 2 Week Lag - 90th percentile",
  "Heavy Rain, 3 Week Lag - 90th percentile",
  "Distance Tertile From Any Surface Water",
  "Distance Tertile From Seasonal Surface Water",
  "Distance Tertile From Ephemeral Surface Water",
  "Proportion of Area within 250m Radius of Household with Any Surface Water, Above Median",
  "Proportion of Area within 250m Radius of Household with Seasonal Surface Water, Above Median",
  "Proportion of Area within 250m Radius of Household with Ephemeral Surface Water, Above Median",
  "Proportion of Area within 500m Radius of Household with Any Surface Water, Above Median",
  "Proportion of Area within 500m Radius of Household with Seasonal Surface Water, Above Median",
  "Proportion of Area within 500m Radius of Household with Ephemeral Surface Water, Above Median",
  "Proportion of Area within 750m Radius of Household with Any Surface Water, Above Median",
  "Proportion of Area within 750m Radius of Household with Seasonal Surface Water, Above Median",
  "Proportion of Area within 750m Radius of Household with Ephemeral Surface Water, Above Median",
  "Weekly sum of precipitation above median, 0 week lag",
  "Weekly sum of precipitation above median, 1 week lag",
  "Weekly sum of precipitation above median, 2 week lag",
  "Weekly sum of precipitation above median, 3 week lag",
  "Weekly sum of precipitation above 75th percentile, 0 week lag",
  "Weekly sum of precipitation above 75th percentile, 1 week lag",
  "Weekly sum of precipitation above 75th percentile, 2 week lag",
  "Weekly sum of precipitation above 75th percentile, 3 week lag",
  "Weekly sum of precipitation above 90th percentile, 0 week lag",
  "Weekly sum of precipitation above 90th percentile, 1 week lag",
  "Weekly sum of precipitation above 90th percentile, 2 week lag",
  "Weekly sum of precipitation above 90th percentile, 3 week lag",
  "Weekly average temperature above median, 0 week lag",
  "Weekly average temperature above median, 1 week lag",
  "Weekly average temperature above median, 2 week lag",
  "Weekly average temperature above median, 3 week lag",
  "Weekly maximum temperature above median, 0 week lag",
  "Weekly maximum temperature above median, 1 week lag",
  "Weekly maximum temperature above median, 2 week lag",
  "Weekly maximum temperature above median, 3 week lag",
  "Weekly minimum temperature above median, 0 week lag",
  "Weekly minimum temperature above median, 1 week lag",
  "Weekly minimum temperature above median, 2 week lag",
  "Weekly minimum temperature above median, 3 week lag")

#------------------------------------------------------
# Create lists with outcome variables, binary
#------------------------------------------------------
outcome_binary = c("gam_diar7d_0",
                   "gam_bruise7d_0", # ADDED FOR negative control
                   "gam_pos_Adenovirus40_41",
                   "gam_pos_aEPEC",
                   "gam_pos_Aeromonas",
                   "gam_pos_B_fragilis",
                   "gam_pos_Campylobacter",
                   "gam_pos_Cryptosporidium",
                   "gam_pos_C_difficile",
                   "gam_pos_EAEC",
                   "gam_pos_EPEC_any",
                   "gam_pos_ETEC_any",
                   "gam_pos_E_bieneusi",
                   "gam_pos_Giardia", 
                   "gam_pos_LT_ETEC",
                   "gam_pos_Norovirus",
                   "gam_pos_parasite",
                   "gam_pos_Plesiomonas",
                   "gam_pos_Sapovirus", 
                   "gam_pos_Shigella_EIEC",
                   "gam_pos_STEC",
                   "gam_pos_ST_ETEC",
                   "gam_pos_tEPEC",
                   "gam_pos_virus"
)

outcome_binary_colnames <- c("diar7d",
                             "bruise7d",
                             "pos_Adenovirus40_41",
                             "pos_aEPEC",
                             "pos_Aeromonas",
                             "pos_B.fragilis",
                             "pos_Campylobacter",
                             "pos_Cryptosporidium",
                             "pos_C.difficile",
                             "pos_EAEC",
                             "pos_EPEC_any",
                             "pos_ETEC_any",
                             "pos_E.bieneusi",
                             "pos_Giardia",
                             "pos_LT_ETEC",
                             "pos_Norovirus",
                             "pos_parasite",
                             "pos_Plesiomonas",
                             "pos_Sapovirus",
                             "pos_Shigella_EIEC",
                             "pos_STEC",
                             "pos_ST_ETEC",
                             "pos_tEPEC",
                             "pos_virus")


outcome_binary_label <- c("7-Day Diarrhea, Control Arm",
                          "7-Day Bruising, Negative Control",
                          "Adenovirus 40/41",
                          "aEPEC",
                          "Aeromonas",
                          "B. fragilis",
                          "Campylobacter",
                          "Cryptosporidium",
                          "C. difficile",
                          "EAEC",
                          "EPEC",
                          "ETEC",
                          "E. bieneusi",
                          "Giardia",
                          "LT-ETEC",
                          "Norovirus",
                          "Any Parasite",
                          "Plesiomonas",
                          "Sapovirus",
                          "Shigella/EIEC",
                          "STEC",
                          "ST-ETEC",
                          "tEPEC",
                          "Any Virus")

#------------------------------------------------------
## CREATE TABLES ##
#-----------------------------------------------------
tic <- Sys.time()
#------------------------------------------------------
# Loop through each of the virus types in outcome_binary
#------------------------------------------------------
prev_table <- NA
for(k in 1:length(risk_factors_categorical)) {
  my_table <- as.data.frame(matrix(ncol = 14, nrow = ))
  colnames(my_table) <- c("Outcome", "RF", "PR.Lower", "PR.Upper",
                          "PR", "RF_Type", "Group", "Analysis", "N",  "risk_factor", "filename", "SE", "outcome_name", "outcome_name_binary")
  #------------------------------------------------------
  # Loop through each of the risk factors to create table rows
  #------------------------------------------------------
  for(i in 1:length(outcome_binary)) {
    print(paste0("Risk factor: ",risk_factor_categorical_label[[k]]))
    print(paste0("Outcome: ",outcome_binary_label[[i]]))
    
    tryCatch(expr = {
      new_row <- make_table_row(# Pass in type of outcome (diarrhea-0, -1, 14m)
        outcome_subdir = outcome_subdir_binary[[i]],
        # Pass in the dataframe storing un-adjusted and the adjusted paths
        dirs_df = outcome_dirs_df,
        risk_factor = risk_factors_categorical[[k]], 
        risk_factor_label = risk_factor_categorical_label[[k]], 
        outcome = outcome_binary[[i]], 
        outcome_colname = outcome_binary_colnames[[i]],
        outcome_label = outcome_binary_label[[i]]) %>%
        mutate(outcome_name = outcome_binary_colnames[[i]],
               outcome_name_binary = outcome_binary[[i]])
        
      my_table <- bind_rows(my_table, new_row)  # Append the row created by that rf x outcome combo into the larger table for the given outcome 
      
    },
    # Error if gam plot errors out on specific outcome binary factor
    error = function(e) {
      results = paste0(outcome_binary[[i]],"_",risk_factors_categorical[[k]],".RDS")
      print(paste0("Gam plot errored out on: ",results))
      print(paste0("Error message: ",e))
      df_row <- data.frame(results %>% rbind(paste0("Error: ", e)))
      colnames(df_row) <- ""
      # write.table(df_row, paste0(home_path, tab_dir, "PR_Table_Errors.csv"), append=TRUE)
    })
  }
  
  
  #------------------------------------------------------
  # Export table
  # Add title to table
  #------------------------------------------------------
  my_table <- subset(my_table, !is.na(Outcome))
  
  if (!is.na(prev_table)) {
    prev_table <- rbind(my_table, prev_table)
  } else {
    prev_table <- my_table
  }
  
  if (k == length(risk_factors_categorical)) {
    print(paste0("Saving to: ", data_dir, "intermediate_RDS_files/old_PR_tables_nointeraction.RDS"))
    saveRDS(prev_table, paste0(data_dir, "intermediate_RDS_files/old_PR_tables_nointeraction.RDS"))
  }
  
}

toc <- Sys.time()
toc - tic
