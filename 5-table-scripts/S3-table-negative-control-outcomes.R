#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens
# 
# This script makes the supplemental table showing negative control outcome of bruising
#
#######################################

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))



# read in results --------------------------------------------------
prev_table = readRDS(paste0(data_dir, "corrected_PR_tables_nointeraction.RDS")) %>%
  filter(!grepl("temp", RF), !grepl("90", RF), !grepl("75th", RF)) # removing the sensitivity analyses


neg_control = prev_table %>%
  filter(Group == "Adj.", grepl("Bruising", Outcome)) %>%
  mutate(order = factor(case_when(
    grepl("heavyrain", risk_factor) ~ "Heavy Rain",
    grepl("ppt_week_sum", risk_factor) ~ "Above Median Weekly Sum of Precipitation",
    grepl("distance_from_any", risk_factor) ~ "Distance Tertile from Any Surface Water",
    grepl("distance_from_season", risk_factor) ~ "Distance Tertile from Seasonal Surface Water",
    grepl("distance_from_ephemeral", risk_factor) ~ "Distance Tertile from Ephemeral Surface Water",
    grepl("prop_detected", risk_factor) ~ "Above Median Proportion of Surface Water Near Household"), 
    levels = c("Heavy Rain", "Above Median Weekly Sum of Precipitation", "Distance Tertile from Any Surface Water", "Distance Tertile from Seasonal Surface Water",
               "Distance Tertile from Ephemeral Surface Water", "Above Median Proportion of Surface Water Near Household")),
    category = factor(case_when(
      grepl("250m Radius", RF) ~ "250m Radius",
      grepl("500m Radius", RF) ~ "500m Radius",
      grepl("750m Radius", RF) ~ "750m Radius", 
      grepl("1weeklag", risk_factor) ~ "1-Week Lag",
      grepl("2weeklag", risk_factor) ~ "2-Week Lag",
      grepl("3weeklag", risk_factor) ~ "3-Week Lag",
      grepl("Close", RF_Type) ~ "Close vs. Far",
      grepl("Medium", RF_Type) ~ "Medium vs. Far"),
      levels = c("250m Radius", "500m Radius", "750m Radius", "1-Week Lag", "2-Week Lag", "3-Week Lag", "Close vs. Far","Medium vs. Far")),
    source_type = factor(case_when(
      grepl("any_surface", risk_factor) ~ "Any Surface Water",
      grepl("ephemeral_surface", risk_factor) ~ "Ephemeral Surface Water",
      grepl("seasonal_surface", risk_factor) ~ "Seasonal Surface Water"),
      levels = c("Any Surface Water", "Seasonal Surface Water", "Ephemeral Surface Water")),
    cutoff = factor(case_when(
      grepl("90", risk_factor) ~ "90th percentile",
      grepl("75", risk_factor) ~ "75th percentile",
      grepl("median", risk_factor) ~ "Median",
      TRUE ~ "None"), 
      levels = c("None", "Median", "75th percentile", "90th percentile"))) %>%
  arrange(order, cutoff, RF_Type, category, source_type) %>%
  mutate(pr_ci = paste0(round(PR, 2), " (", round(PR.Lower,2), ", ", round(PR.Upper, 2), ")")) %>%
  select(Outcome, RF, N, pr_ci, PR, PR.Lower, PR.Upper, category, source_type) 


write.csv(neg_control, file = paste0(tab_dir, "S3-Table-adjusted-pr-table-neg-control.csv"))
  
#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()