#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Script to create the demographic characteristics table
# of those studied in the diarrhea and pathogen cohorts
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

# Load datasets
d_diarrhea_0 = readRDS(paste0(clean_offset_data_dir, clean_diarr_merged_offset))
d_pathogens_0 = readRDS(paste0(clean_offset_data_dir, clean_pathogen_merged_offset))

# Restrict diarrhea to control arm
d_diarrhea_0 = d_diarrhea_0 %>% filter(intervention==0) %>%
  mutate(tr_received = ifelse(intervention == 0, "Control", tr))

# Rename / recode datasets
d_diarrhea = d_diarrhea_0
names(d_diarrhea)[names(d_diarrhea)=="tr_received"]="tr_ref"

d_pathogens = d_pathogens_0
names(d_pathogens)[names(d_pathogens)=="tr"]="tr_ref"

# Return name of the study
f_name <- function(study) {
  name_study = deparse(substitute(study))
  return(substring(name_study,3))
}

# Number of children
diarrhea_child = n_distinct(d_diarrhea$dataid,d_diarrhea$childid)
pathogens_child = n_distinct(d_pathogens$childid)

child_names = c("diarrhea_child" = diarrhea_child,
                "pathogens_child" = pathogens_child)

f_children <- function(name_study) {
  name_variable_child = paste0(name_study, "_child")
  n_children = as.numeric(child_names[name_variable_child])
  n_children_format = formatC(n_children, big.mark = ",")
  return(n_children_format)
}

# Observations
f_observations <- function(study){
  n_obs = nrow(study)
  return(n_obs)
}

format_obs <- function(observations) {
  obs_format <- formatC(observations, big.mark = ",")
  return(obs_format)
}

# Study years
years_list = list("2012", "2013", "2014", "2015", "2016")
f_years <- function(study,years = years_list) {
  c_year = c()
  for (year in years) {
    study_table = table(study["year"])
    study_y = ifelse(is.na(as.integer(study_table[year])) == TRUE, 0, as.integer(study_table[year]))
    study_y_format = paste0(formatC(study_y, big.mark = ","),
                            " (",sprintf("%0.01f", as.numeric(study_y)/f_observations(study)*100),"%",")" )
    c_year = append(c_year, study_y_format)
  }
  return(c_year)
}

# Mean ages
d_diarrhea <- d_diarrhea %>% mutate(agem = as.numeric(agey * 12))
d_pathogens <- d_pathogens %>% mutate(agem = as.numeric(agey * 12))
f_age <- function(study) {
  age_m <- study$agem
  mean_age <- mean(age_m, na.rm = T)
  sd_age <- sd(age_m)
  age_format <- paste0(sprintf("%0.01f", mean_age), " (", sprintf("%0.01f", sd_age), ")")
  return(age_format)
}

# Season
f_season <- function(study) {
  season_table = table(study$season)
  dry = as.integer(season_table["dry"])
  rainy = as.integer(season_table["rainy"])
  rainy_format = paste0(formatC(rainy, big.mark = ","),
                        " (",sprintf("%0.01f", rainy/f_observations(study)*100),"%",")" )
  dry_format = paste0(formatC(dry, big.mark = ","),
                      " (",sprintf("%0.01f", dry/f_observations(study)*100),"%",")" )
  c_season = c(rainy_format, dry_format)
  return(c_season)
}

# Antibiotics
f_antibiotics_deworming <- function(study, study_name) {
  if (study_name == "pathogens") {
    n_antibiotics = as.numeric(table(study$abx_7d)["1"])
    antibiotics_format = paste0(formatC(n_antibiotics, big.mark = ","),
                                " (",sprintf("%0.01f", n_antibiotics/f_observations(study)*100),"%",")" )
  } else {
    antibiotics_format = ""
  }
  return(antibiotics_format)
}

# Treatment arm assignments
f_treatment <- function(study) {
  tr_table = table(study$tr_ref)
  names = c("Control", "Handwashing", "Nutrition", "Nutrition + WSH", "Sanitation", "Water", "WSH")
  names_table = names(tr_table)
  print(names_table)
  n_tr = c("","","","","","","")
  numbers = as.numeric(tr_table)
  count = 1
  for (i in (1:length(names))) {
    if (names_table[i] %in% names) {
      row = which(names == names_table[i])
      n = as.numeric(tr_table[names[row]])
      tr_format =  paste0(formatC(n, big.mark = ",")," (",sprintf("%0.01f", n/f_observations(study)*100),"%",")" )
      n_tr[row] = tr_format
    } else {
      if (identical(names_table[i], "N+WSH")) {
        row = which(names == "Nutrition + WSH")
        n = as.numeric(tr_table["N+WSH"])
        print(n)
        print(names_table[i])
        tr_format =  paste0(formatC(n, big.mark = ",")," (", sprintf("%0.01f", n/f_observations(study)*100),"%",")" )
        n_tr[row] = tr_format
      }
    }
  }
  return(n_tr)
}

# Define variables (rows)
variables_f = c("Children" = f_children,
                "Observations" = f_observations,
                "years" = f_years, "age" = f_age,
                "season" = f_season,
                "Antibiotics_deworming" = f_antibiotics_deworming,
                "Treatment arms" = f_treatment)

# Calculate variables for each study
study_demo <- function(study, variables = variables_f) {
  c_study = c()
  name_study = substring(deparse(substitute(study)),3)
  print(name_study)
  variables_names = names(variables)
  count = 0
  for (v in variables) {
    count = count +1
    if (variables_names[count]=="Children") {
      c_study = append(c_study, f_children(name_study))
    } else {
      if (variables_names[count] == "Observations") {
        c_study = append(c_study, format_obs(f_observations(study))) 
        } else {
          if (variables_names[count]=="Antibiotics_deworming") {
            c_study = append(c_study,f_antibiotics_deworming(study, name_study)) 
            } else {
              c_study = append(c_study, v(study))
        }
      }
    }
  }
  return(c_study)
}

table_demographics = cbind(study_demo(d_diarrhea), 
                           study_demo(d_pathogens))
names = c("Diarrhea", 
          "Pathogens")

variables_names <- c("Children", "Observations", 
                     "2012", "2013", "2014", "2015", "2016",
                     "Mean age in months (SD)",
                     "Rainy", "Dry", "Antibiotics", # "Deworming",
                     "Control", "Handwashing", "Nutrition",
                     "Nutrition + WSH", "Sanitation", "Water", "WSH")

colnames(table_demographics) = names
rownames(table_demographics) = variables_names

write.csv(table_demographics, paste0(here::here(), "/6-tables/1-participant-demographics.csv"))
