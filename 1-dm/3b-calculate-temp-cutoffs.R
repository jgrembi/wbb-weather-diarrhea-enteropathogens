#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Calculate cutoffs for categorical temperature variables
#######################################


rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
library(future)
library(future.apply)

all_daily_fldas_data = readRDS(paste0(box_data_path, "daily_temperatures_fldas.RDS"))
all_daily_fldas_data = all_daily_fldas_data %>% dplyr::select(cells, temp, date) %>% distinct()

# Calculate cutoffs
future::plan(multisession, workers = 8)

tic("all 7-d and 30-d temps") ## took 502 sec / 8.3 min
all_7_30day_temps = furrr::future_map_dfr(unique(all_daily_fldas_data$date), function (d) {
  # d = as.Date("2014-07-09")
  d7 = all_daily_fldas_data %>%
    filter(between(date, d - 7 + 1, d)) %>%
    group_by(cells) %>%
    summarize(absmaxtemp_7_days = max(temp, na.rm = TRUE),
              absmintemp_7_days = min(temp, na.rm = TRUE),
              avgtemp_7_days = mean(temp, na.rm = TRUE),
              n7 = n()) %>%
    mutate(date = d)
  
  d30 = all_daily_fldas_data %>%
    filter(between(date, d - 30 + 1, d)) %>%
    group_by(cells) %>%
    summarize(absmaxtemp_30_days = max(temp, na.rm = TRUE),
              absmintemp_30_days = min(temp, na.rm = TRUE),
              avgtemp_30_days = mean(temp, na.rm = TRUE),
              n30 = n()) %>%
    mutate(date = d)
  
  ret = left_join(d7, d30)
  return(ret)
})

toc()
future::plan(sequential)

saveRDS(all_7_30day_temps, paste0(box_data_path, "all_7_30day_temps.RDS")) 


median_all_7day_temps = all_7_30day_temps %>%
  filter(n7 == 7) %>%
  ungroup() %>% 
  mutate_at(vars(contains("temp")), ~ (ifelse(abs(.) == "Inf", NA, .))) %>%
  summarise(absmaxtemp_7_days_median = median(absmaxtemp_7_days, na.rm = T),
            absmintemp_7_days_median = median(absmintemp_7_days, na.rm = T), 
            avgtemp_7_days_median = median(avgtemp_7_days, na.rm = T),
            maxtemp_7_days_max = round(max(absmaxtemp_7_days, na.rm = T), 2),
            maxtemp_7_days_min = round(min(absmaxtemp_7_days, na.rm = T), 2),
            mintemp_7_days_max = round(max(absmintemp_7_days, na.rm = T), 2),
            mintemp_7_days_min = round(min(absmintemp_7_days, na.rm = T), 2),
            avgtemp_7_days_max = round(max(avgtemp_7_days, na.rm = T), 2),
            avgtemp_7_days_min = round(min(avgtemp_7_days, na.rm = T), 2))

median_all_30day_temps = all_7_30day_temps %>%
  filter(n30 == 30) %>%
  ungroup() %>%
  summarise(absmaxtemp_30_days_median = median(absmaxtemp_30_days, na.rm = T),
            absmintemp_30_days_median = median(absmintemp_30_days, na.rm = T), 
            avgtemp_30_days_median = median(avgtemp_30_days, na.rm = T))

temp_conversion_K_C = round(-273.15, 2)
(temp_cutoffs = list(absmaxtemp_7_days_median_cutoff = median_all_7day_temps$absmaxtemp_7_days_median,
                     absmintemp_7_days_median_cutoff =  median_all_7day_temps$absmintemp_7_days_median,
                     avgtemp_7_days_median_cutoff = median_all_7day_temps$avgtemp_7_days_median,
                     absmaxtemp_30_days_median_cutoff = median_all_30day_temps$absmaxtemp_30_days_median,
                     absmintemp_30_days_median_cutoff = median_all_30day_temps$absmintemp_30_days_median,
                     avgtemp_30_days_median_cutoff = median_all_30day_temps$avgtemp_30_days_median,
                     absmaxtemp_7_days_range_inC = paste0(round(median_all_7day_temps$maxtemp_7_days_min + temp_conversion_K_C, 0), ", ", round(median_all_7day_temps$maxtemp_7_days_max + temp_conversion_K_C, 0)),
                     absmintemp_7_days_range_inC = paste0(round(median_all_7day_temps$mintemp_7_days_min + temp_conversion_K_C, 0), ", ", round(median_all_7day_temps$mintemp_7_days_max + temp_conversion_K_C, 0)),
                     avgtemp_7_days_range_inC = paste0(round(median_all_7day_temps$avgtemp_7_days_min + temp_conversion_K_C, 0), ", ", round(median_all_7day_temps$avgtemp_7_days_max + temp_conversion_K_C, 0))))



saveRDS(temp_cutoffs, paste0(box_data_path, "fldas_temp_cutoffs.RDS"))
saveRDS(temp_cutoffs, paste0(data_dir, "fldas_temp_cutoffs.RDS"))

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()
