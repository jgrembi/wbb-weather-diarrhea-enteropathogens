#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Construct precipitation variables
################################################################################################

rm(list = ls())
source(paste0(here::here(), "/0-config.R"))

all_ppt_data = readRDS(paste0(box_data_path, "all_ppt_data.RDS"))
cutoffs = readRDS(paste0(box_data_path, "ppt_cutoffs.RDS"))

heavy_rain_cutoff = cutoffs$heavy_rain_cutoff
heavy_rain_cutoff_90 = cutoffs$heavy_rain_cutoff_90th
sum_7d_ppt_median_cutoff = cutoffs$median_7d_cutoff
sum_7d_ppt_75pct_cutoff = cutoffs$pct_75_cutoff_7_day_ppt_sum
sum_7d_ppt_90pct_cutoff = cutoffs$pct_90_cutoff_7_day_ppt_sum
median_30day_cutoff = cutoffs$median_30day_cutoff
med_60day_cutoff = cutoffs$med_60day_cutoff
high_60day_cutoff = cutoffs$high_60day_cutoff

lag_times = data.frame(num_weeks_lag = c(0, 1, 2, 3),
                       start = c(7, 14, 21, 28),
                       end = c(1, 8, 15, 22))


d_path = readRDS(paste0(clean_washb_path_box, clean_bdata_pathogen_box)) %>%
  mutate("date" = as.Date(st_date2, format = "%Y-%m-%d")) %>%
  filter(!is.na(qgpslong), !is.na(date))
ll_path = d_path %>% dplyr::select(qgpslong, qgpslat, date)

d_diarr = readRDS(paste0(clean_washb_path_box, clean_bdata_diarr_box)) %>%
  mutate("date" = as.Date(svydate,"%d%b%Y")) %>%
  filter(!is.na(qgpslong), !is.na(date))
ll_diarr = d_diarr %>% dplyr::select(qgpslong, qgpslat, date)

ll = rbind(ll_path, ll_diarr) %>% 
  distinct()

extract_ppt_data = function(ll_row){
  row_date = as.Date(ll_row[["date"]])
  row_long = ll_row[["qgpslong"]]
  row_lat = ll_row[["qgpslat"]]
  
  ll_filter = all_ppt_data %>% filter(as.numeric(qgpslong) == as.numeric(row_long), 
                                      as.numeric(qgpslat) == as.numeric(row_lat))
  

  
  find_sum_7day_ppt = function(lag_time){
    date_filter = ll_filter %>% 
      filter(between(date, 
                     row_date - lag_time[["start"]], 
                     row_date - lag_time[["end"]]))
    
    heavy_rain_ind = sum(date_filter$ppt >= heavy_rain_cutoff) > 0
    heavy_rain_90 = sum(date_filter$ppt >= heavy_rain_cutoff_90) > 0
    
    return(data.frame(ppt_week_sum = sum(date_filter$ppt, na.rm = T), 
                      heavy_rain = heavy_rain_ind,
                      heavy_rain_90 = heavy_rain_90) %>%
             mutate(ppt_week_sum_median = factor(ifelse(ppt_week_sum <= sum_7d_ppt_median_cutoff, "at or below median", "above median"), levels = c("at or below median", "above median")),
                    ppt_week_sum_75 = factor(ifelse(ppt_week_sum <= sum_7d_ppt_75pct_cutoff, "at or below 75th percentile", "above 75th percentile"), levels = c("at or below 75th percentile", "above 75th percentile")),
                    ppt_week_sum_90 = factor(ifelse(ppt_week_sum <= sum_7d_ppt_90pct_cutoff, "at or below 90th percentile", "above 90th percentile"), levels = c("at or below 90th percentile", "above 90th percentile"))))
  }
  
  sum_7day_ppt = apply(lag_times, 1, find_sum_7day_ppt) %>% bind_cols()
  
  colnames(sum_7day_ppt) = c("ppt_week_sum_0weeklag",
                             "heavyrain_0weeklag",
                             "heavyrain_0weeklag_90",
                             "ppt_week_sum_0weeklag_median",
                             "ppt_week_sum_0weeklag_75",
                             "ppt_week_sum_0weeklag_90",
                             
                             "ppt_week_sum_1weeklag",
                             "heavyrain_1weeklag",
                             "heavyrain_1weeklag_90",
                             "ppt_week_sum_1weeklag_median",
                             "ppt_week_sum_1weeklag_75",
                             "ppt_week_sum_1weeklag_90",
                             
                             "ppt_week_sum_2weeklag",
                             "heavyrain_2weeklag",
                             "heavyrain_2weeklag_90",
                             "ppt_week_sum_2weeklag_median",
                             "ppt_week_sum_2weeklag_75",
                             "ppt_week_sum_2weeklag_90",
                             
                             "ppt_week_sum_3weeklag",
                             "heavyrain_3weeklag",
                             "heavyrain_3weeklag_90",
                             "ppt_week_sum_3weeklag_median",
                             "ppt_week_sum_3weeklag_75",
                             "ppt_week_sum_3weeklag_90")
  
  
  find_sum_30day_ppt = function(lag_time) {
    date_filter = ll_filter %>% 
      filter(between(date, 
                     as.Date(row_date - lag_time[["start"]] - 30), 
                     as.Date(row_date - lag_time[["start"]] - 1)))
    
    sum_ppt = sum(date_filter$ppt, na.rm = T)
    
    if (sum_ppt <= median_30day_cutoff) {
      ppt_30d_cat = "at or below median"
    } else {
      ppt_30d_cat = "above median"
    }
    return(ppt_30d_cat)
  }
  
  sum_30day_ppt = as.data.frame(t(apply(lag_times, 1, find_sum_30day_ppt)))
  
  colnames(sum_30day_ppt) = c("ppt_30daysum_0weeklag",
                              "ppt_30daysum_1weeklag",
                              "ppt_30daysum_2weeklag",
                              "ppt_30daysum_3weeklag")
  
  
  find_sum_60day_ppt = function(lag_time) {
    date_filter = ll_filter %>% 
      filter(between(date, 
                     as.Date(row_date - lag_time[["start"]] - 60), 
                     as.Date(row_date - lag_time[["start"]] - 1)))
    
    sum_ppt = sum(date_filter$ppt, na.rm = T)
    
    if (sum_ppt <= med_60day_cutoff) {
      ppt_60d_cat = "low"
    } else if (sum_ppt <= high_60day_cutoff) {
      ppt_60d_cat = "medium"
    } else {
      ppt_60d_cat = "high"
    }
    return(ppt_60d_cat)
  }
  
  sum_60day_ppt = as.data.frame(t(apply(lag_times, 1, find_sum_60day_ppt)))
  
  colnames(sum_60day_ppt) = c("ppt_60daysum_0weeklag",
                              "ppt_60daysum_1weeklag",
                              "ppt_60daysum_2weeklag",
                              "ppt_60daysum_3weeklag")
  
  ll_data = data.frame(t(ll_row))
  return(bind_cols(ll_data, sum_7day_ppt, sum_30day_ppt, sum_60day_ppt))
}

## ~15000s/~250m
tic("process ppt data")
processed_ppt_data = apply(ll, 1, extract_ppt_data) %>% bind_rows()
toc()
saveRDS(processed_ppt_data, paste0(box_data_path, "washb-bangladesh-ppt-data-new.RDS"))

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()