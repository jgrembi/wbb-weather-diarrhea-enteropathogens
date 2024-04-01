#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# merge all variable data for
# each lat/long coordinate 
#######################################

rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

################################
# Load Data
################################
d_diarr = readRDS(paste0(clean_washb_path_box, clean_bdata_diarr_box)) %>%
  filter(!is.na(qgpslong))


d_path = readRDS(paste0(clean_washb_path_box, clean_bdata_pathogen_box)) %>%
  filter(!is.na(qgpslong))

################################
# We will make a df of mean values later
#  So here we set vectors to contain those values
################################
var_list = NULL
mean_list = NULL
mean_rainy = NULL
mean_dry = NULL


################################
# Define rainy season
################################
## We also need to define the seasons for the 80th and 20th percentile values
rainy_season_def = read.csv(rainy_seasons_path) %>% 
  select(-X) %>%
  rename("year" = "season",
         "season_start" = "start",
         "season_end" = "end") %>%
  mutate(season_start = as.Date(season_start), 
         season_end = as.Date(season_end),
         month_start = month(round_date(season_start, unit = "month")),
         month_end = month(round_date(season_end, unit = "month")))

################################
# VPD
# 7-extract-raster-terraclimate-vpd.R
################################

terraclimate_vpd = readRDS(paste0(box_data_path, "washb-bangladesh-vpd.RDS")) %>% 
  mutate(distance_from_source = ifelse(distance_from_source <= 4000, distance_from_source, NA),
         # center all risk_factors
         vpd_C = as.numeric(scale(vpd, center = T, scale = F))) %>%
  rename("distance_from_source_vpd" = "distance_from_source") %>%
  left_join(rainy_season_def, by = c("year")) %>%
  mutate(season = factor(ifelse(month > month_start & month < month_end, 1, 0), 
                         levels = c(0,1), 
                         labels = c("dry", "rainy"))) 


var_list = c(var_list, "vpd")
mean_list = c(mean_list, mean(terraclimate_vpd$vpd, na.rm = T))
mean_rainy = c(mean_rainy, mean(terraclimate_vpd %>% 
                                  filter(season == "rainy") %>%
                                  pull(vpd), na.rm = T))
mean_dry = c(mean_dry, mean(terraclimate_vpd %>% 
                              filter(season == "dry") %>%
                              pull(vpd), na.rm = T))

## Remove extra variables
terraclimate_vpd = terraclimate_vpd %>%
  select(-(season_start:season))

################################
# Surface Water
# 6-import-surface-water-data.R
################################

surface_water_distances = readRDS(paste0(box_data_path,"washb-bangladesh-distance_from_surface_water.RDS"))

surface_water_tertile_fun = function(var) {
  out = factor(case_when(
    var <= round(quantile(var, probs = 0.33, na.rm = T), 0) ~ "close water",
    var <= round(quantile(var, probs = 0.66, na.rm = T), 0) ~ "medium distance water",
    var > round(quantile(var, probs = 0.66, na.rm = T), 0) ~ "far water"),
    levels = c("far water","medium distance water", "close water"))
  return(out)
}

surface_water_medians_fun = function(var) {
  out = factor(case_when(
    var <= median(var, na.rm = T) ~ "less than or median proportion",
    var > median(var, na.rm = T) ~ "higher than median proportion"),
    levels = c("less than or median proportion", "higher than median proportion"))
}

surface_water_distances = surface_water_distances %>%
  #categorize 'distance_from' variables into tertiles
  mutate(across(.cols = contains("distance_from"), .fns = surface_water_tertile_fun, .names = "{.col}_tertile")) %>%
  #categorize 'prop_detected' variables into above or below median
  mutate(across(.cols = contains("prop_detected"), .fns = surface_water_medians_fun, .names = "{.col}_median"))

################################
# Precipitation 
# precipitation -> 4d-construct-ppt-variables.R
################################
precipitation = readRDS(paste0(box_data_path,"washb-bangladesh-ppt-data-new.RDS")) %>%
  mutate(qgpslong = as.numeric(qgpslong),
         qgpslat = as.numeric(qgpslat)) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  mutate(across(.cols = contains("ppt_30daysum"), ~ factor(.x, levels = c("at or below median", "heavy rain")))) %>%
  mutate(across(.cols = contains("ppt_60daysum"), ~ factor(.x, levels = c("low", "medium", "high")))) %>%
  mutate(across(.cols = contains("heavyrain"), ~ factor(ifelse(.x == T, "heavy rain", "not heavy rain"), levels = c("not heavy rain", "heavy rain")))) %>%
  mutate(across(.cols = contains("ppt_week_sum_") & !contains(c("median","75","90")), ~ as.numeric(scale(.x, center = T, scale = F)), .names = "{.col}_C"), 
         year = year(date)) %>%
  # add in season variable
  left_join(rainy_season_def, by = "year") %>%
  mutate(season = factor(ifelse(date > season_start & date < season_end, 1, 0), 
                         levels = c(0,1), 
                         labels = c("dry", "rainy"))) 

var_list = c(var_list,
             "ppt_week_sum_0weeklag", "ppt_week_sum_1weeklag", "ppt_week_sum_2weeklag", "ppt_week_sum_3weeklag")
mean_list = c(mean_list, 
              precipitation %>%
                summarise_at(vars(contains("ppt_week_sum") & !contains(c("_C", "median", "75", "90"))), list(~mean(., na.rm = T))))
              
mean_rainy = c(mean_rainy, 
               precipitation %>%
                 filter(season == "rainy") %>%
                 summarise_at(vars(contains("ppt_week_sum") & !contains(c("_C", "median", "75", "90"))), list(~mean(., na.rm = T))))
mean_dry = c(mean_dry, 
             precipitation %>%
               filter(season == "dry") %>%
               summarise_at(vars(contains("ppt_week_sum") & !contains(c("_C", "median", "75", "90"))), list(~mean(., na.rm = T))))

## Remove extra variables
precipitation = precipitation %>%
  select(-(year:season))

################################
# Temperature
# temperature -> 3c-construct-temp-variables.R
################################
temp_cutoffs = readRDS(paste0(box_data_path, "fldas_temp_cutoffs.RDS"))

weekly_temp_medians_fun = function(var) {
  out = factor(case_when(
    var <= temp_cutoffs$avgtemp_7_days_median_cutoff ~ "at or below median",
    var > temp_cutoffs$avgtemp_7_days_median_cutoff ~ "above median"),
    levels = c("at or below median", "above median"))
}

weekly_temp_min_fun = function(var) {
  out = factor(case_when(
    var <= temp_cutoffs$absmintemp_7_days_median_cutoff ~ "at or below median",
    var > temp_cutoffs$absmintemp_7_days_median_cutoff ~ "above median"),
    levels = c("at or below median", "above median"))
}

weekly_temp_max_fun = function(var) {
  out = factor(case_when(
    var <= temp_cutoffs$absmaxtemp_7_days_median_cutoff ~ "at or below median",
    var > temp_cutoffs$absmaxtemp_7_days_median_cutoff ~ "above median"),
    levels = c("at or below median", "above median"))
}

monthly_temp_medians_fun = function(var) {
  out = factor(case_when(
    var <= temp_cutoffs$avgtemp_30_days_median_cutoff ~ "at or below median",
    var > temp_cutoffs$avgtemp_30_days_median_cutoff ~ "above median"),
    levels = c("at or below median", "above median"))
}

monthly_temp_min_fun = function(var) {
  out = factor(case_when(
    var <= temp_cutoffs$absmintemp_30_days_median_cutoff ~ "at or below median",
    var > temp_cutoffs$absmintemp_30_days_median_cutoff ~ "above median"),
    levels = c("at or below median", "above median"))
}

monthly_temp_max_fun = function(var) {
  out = factor(case_when(
    var <= temp_cutoffs$absmaxtemp_30_days_median_cutoff ~ "at or below median",
    var > temp_cutoffs$absmaxtemp_30_days_median_cutoff ~ "above median"),
    levels = c("at or below median", "above median"))
}

temperature = readRDS(paste0(box_data_path, "washb-bangladesh-temperature-fldas.RDS")) %>%
  mutate(across(.cols = contains("temp_weekavg"), .fns = weekly_temp_medians_fun, .names = "{.col}_median")) %>%
  mutate(across(.cols = contains("temp_weekmin"), .fns = weekly_temp_min_fun, .names = "{.col}_median")) %>%
  mutate(across(.cols = contains("temp_weekmax"), .fns = weekly_temp_max_fun, .names = "{.col}_median")) %>%
  
  mutate(across(.cols = contains("temp_monthavg"), .fns = monthly_temp_medians_fun, .names = "{.col}_median")) %>%
  mutate(across(.cols = contains("temp_monthmin"), .fns = monthly_temp_min_fun, .names = "{.col}_median")) %>%
  mutate(across(.cols = contains("temp_monthmax"), .fns = monthly_temp_max_fun, .names = "{.col}_median")) %>%
  mutate(across(contains("temp") & !contains("median"), ~ . - 273.15)) %>%
  # center all risk_factors
  mutate(across(.cols = contains("temp") & !contains("median"), ~ as.numeric(scale(.x, center = T, scale = F)), .names = "{.col}_C"),
         year = year(date)) %>%
  # add in season variable
  left_join(rainy_season_def, by = "year") %>%
  mutate(season = factor(ifelse(date > season_start & date < season_end, 1, 0),
                         levels = c(0,1),
                         labels = c("dry", "rainy")))

var_list = c(var_list, 
             "temp_weekavg_0weeklag", "temp_weekavg_1weeklag", "temp_weekavg_2weeklag", "temp_weekavg_3weeklag",
             "temp_weekmin_0weeklag", "temp_weekmin_1weeklag", "temp_weekmin_2weeklag", "temp_weekmin_3weeklag",
             "temp_weekmax_0weeklag", "temp_weekmax_1weeklag", "temp_weekmax_2weeklag", "temp_weekmax_3weeklag",
             "temp_monthavg_0weeklag","temp_monthavg_1weeklag", "temp_monthavg_2weeklag", "temp_monthavg_3weeklag",
             "temp_monthmin_0weeklag","temp_monthmin_1weeklag", "temp_monthmin_2weeklag", "temp_monthmin_3weeklag",
             "temp_monthmax_0weeklag","temp_monthmax_1weeklag", "temp_monthmax_2weeklag", "temp_monthmax_3weeklag")

mean_list = c(mean_list, 
              temperature %>%
                summarise_at(vars(contains(c("temp_weekavg", "temp_weekmin", "temp_weekmax", "temp_monthavg", "temp_monthmin", "temp_monthmax")) & !contains(c("_C", "_median"))), 
                             list(~mean(., na.rm = T))))
                            
mean_rainy = c(mean_rainy, 
               temperature %>%
                 filter(season == "rainy") %>%
                 summarise_at(vars(contains(c("temp_weekavg", "temp_weekmin", "temp_weekmax", "temp_monthavg","temp_monthmin", "temp_monthmax")) & !contains(c("_C", "_median"))), 
                              list(~mean(., na.rm = T))))

mean_dry = c(mean_dry, 
             temperature %>%
               filter(season == "dry") %>%
               summarise_at(vars(contains(c("temp_weekavg", "temp_weekmin", "temp_weekmax", "temp_monthavg","temp_monthmin", "temp_monthmax")) & !contains(c("_C", "_median"))), 
                            list(~mean(., na.rm = T))))


## Remove extra variables
temperature = temperature %>%
  select(-year, -(season_start:season))


################################
# Make data.frame of mean values
################################

df_means = data.frame(variable = var_list, mean_value = unlist(mean_list), 
                      mean_rainy = unlist(mean_rainy), mean_dry = unlist(mean_dry))
saveRDS(df_means, file = paste0(clean_offset_data_dir, "risk_factor_mean_values.RDS"))


################################
# Merge all data sources
################################

merge_dfs = function(df, output_file_name) {
  merged_df = df %>% 
    left_join(terraclimate_vpd, by = c("qgpslong", "qgpslat", "month","year")) %>% 
    left_join(surface_water_distances, by = c("qgpslong", "qgpslat")) %>% 
    left_join(precipitation, by = c("qgpslong", "qgpslat", "date")) %>% 
    left_join(rainy_season_def, by = "year") %>% 
    mutate(season = factor(ifelse(date > season_start & date < season_end, 1, 0), 
                           levels = c(0,1), 
                           labels = c("dry", "rainy"))) %>% 
    select(-season_start, -season_end) %>%
    mutate(qgpslong = as.character(qgpslong), qgpslat = as.character(qgpslat)) %>% 
    left_join(temperature %>% mutate(qgpslong = as.character(qgpslong), qgpslat = as.character(qgpslat)), by = c("qgpslong", "qgpslat", "date")) %>%
    mutate(qgpslong = as.numeric(qgpslong), qgpslat = as.numeric(qgpslat))

  saveRDS(merged_df, paste0(box_data_path, output_file_name))
  return(merged_df)
}

path_merged = merge_dfs(df = d_path, output_file_name = "washb-bangladesh-merged-path.RDS")
diarr_merged = merge_dfs(df = d_diarr, output_file_name = "washb-bangladesh-merged-diarr.RDS")

source(paste0("~/Dropbox/08_JadeCollaboration/wbb-mapping", "/0-data-processing/offset_coords.R"))
