#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Construct temperature variables for analysis
#######################################

rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
library(future)
library(future.apply)


d_path = readRDS(paste0(clean_washb_path_box, clean_bdata_pathogen_box)) %>%
  filter(!is.na(qgpslong), !is.na(date))
ll_path = d_path %>% 
  dplyr::select(qgpslong, qgpslat, date)

d_diarr = readRDS(paste0(clean_washb_path_box, clean_bdata_diarr_box)) %>%
  filter(!is.na(qgpslong), !is.na(date))
ll_diarr = d_diarr %>% 
  dplyr::select(qgpslong, qgpslat, date)

ll = rbind(ll_path, ll_diarr) %>% 
  distinct()


all_daily_fldas_data = readRDS(paste0(box_data_path, "daily_temperatures_fldas.RDS"))

# Calculate lag time averages
week_lag_times = data.frame(num_weeks_lag = c(0, 1, 2, 3),
                            start = c(7, 14, 21, 28),
                            end = c(1, 8, 15, 22))

month_lag_times = data.frame(num_weeks_monthlylag = c(0, 1, 2, 3),
                             start = c(30, 37, 44, 51),
                             end = c(1, 8, 15, 22))


extract_temp_data = function(ll_row){
  row_date = as.Date(ll_row[["date"]])
  row_long = ll_row[["qgpslong"]]
  row_lat = ll_row[["qgpslat"]]
  
  ll_filter = all_daily_fldas_data %>% filter(qgpslong == row_long, qgpslat == row_lat)
  
  find_avg_temp = function(lag_time){
    date_filter = ll_filter %>% 
      filter(date >= row_date - lag_time[["start"]], 
             date <= row_date - lag_time[["end"]])
    
    avg_min_max_tbl = date_filter %>% group_by(qgpslong, qgpslat) %>% 
      summarise("temp_avg" = mean(temp, na.rm = TRUE),
                "temp_min" = min(temp, na.rm = TRUE),
                "temp_max" = max(temp, na.rm = TRUE)) %>% 
      ungroup() %>% 
      dplyr::select("temp_avg", "temp_min", "temp_max")
    
    return(avg_min_max_tbl)
  }
  
  avg_7day_temp = apply(week_lag_times, 1, find_avg_temp) %>% bind_cols()
  colnames(avg_7day_temp) = c("temp_weekavg_0weeklag",
                              "temp_weekmin_0weeklag",
                              "temp_weekmax_0weeklag",
                              
                              "temp_weekavg_1weeklag",
                              "temp_weekmin_1weeklag",
                              "temp_weekmax_1weeklag",
                              
                              "temp_weekavg_2weeklag",
                              "temp_weekmin_2weeklag",
                              "temp_weekmax_2weeklag",
                              
                              "temp_weekavg_3weeklag",
                              "temp_weekmin_3weeklag",
                              "temp_weekmax_3weeklag")
  
  avg_month_temp = apply(month_lag_times, 1, find_avg_temp) %>% bind_cols()
  colnames(avg_month_temp) = c("temp_monthavg_0weeklag",
                               "temp_monthmin_0weeklag",
                               "temp_monthmax_0weeklag",
                               
                               "temp_monthavg_1weeklag",
                               "temp_monthmin_1weeklag",
                               "temp_monthmax_1weeklag",
                               
                               "temp_monthavg_2weeklag",
                               "temp_monthmin_2weeklag",
                               "temp_monthmax_2weeklag",
                               
                               "temp_monthavg_3weeklag",
                               "temp_monthmin_3weeklag",
                               "temp_monthmax_3weeklag")
  
  ll_data = data.frame(ll_row)
  return(bind_cols(ll_data, avg_7day_temp, avg_month_temp))
}

##For some reason this gets hung running it in parallel.  
# I tried both an mclapply() call and also the same as below but run
# in parallel with 8 cores (registerDoParallel(detectCores()/2)).
# Both ran for over 24 h and never exited.

tic("make temp vars") ## took 6.7 hours/ 23992.155 sec  
temp_data = foreach(i = 1:dim(ll)[1],
                    .combine = "rbind") %dopar% {
                      extract_temp_data(ll[i,])
                    }
toc()

## Remove Inf values that resulted from periods where there were no temp values. 
# temp_weekavg resulted in NaN, but min() and max() resulted in Inf
temp_data = temp_data %>%
  mutate_if(is.numeric, ~ replace(., is.infinite(.), NA))

saveRDS(temp_data, paste0(box_data_path, "washb-bangladesh-temperature-fldas.RDS"))
