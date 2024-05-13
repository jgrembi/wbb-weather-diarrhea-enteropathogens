#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Calculate cutoffs for categorical precipitation variables
################################################################################################

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(furrr)

## Read in raw  ppt data 
## This includes daily data for all 64 pixels comprising the study area for the duration of the study period
all_ppt_data = readRDS(paste0(box_data_path, "all_ppt_data_by_pixelID.RDS"))

average_daily_ppts = all_ppt_data %>% filter(ppt > 0) 

heavy_rain_cutoff_80th = as.numeric(quantile(average_daily_ppts$ppt, 0.8))
heavy_rain_cutoff_90th = as.numeric(quantile(average_daily_ppts$ppt, 0.9))

future::plan(multisession, workers = 5)

####################################################################################
## Weekly precipitation (7-day sum)
####################################################################################
# Runtime: ~7800 secs/~130 min
tic("7d ppt sum")
all_7day_ppts = furrr::future_map_dfr(unique(all_ppt_data$date), function (d) {
  ret = all_ppt_data %>%
    filter(between(date, d - 7 + 1, d)) %>%
    group_by(pixelID) %>%
    summarize(sum_ppt = sum(ppt, na.rm = T),
              n = n()) %>%
    mutate(date = d)
  return(ret)
})
toc()

saveRDS(all_7day_ppts, paste0(box_data_path, "all_7day_ppts.RDS")) 
# all_7day_ppts <- readRDS(paste0(box_data_path, "all_7day_ppts.RDS"))

##Take result and then, 
# 1) remove anything from the first 6 days because the sum doesn't cover a full 7d
# 2) calculate median over all values

median_7_day_ppts = all_7day_ppts %>%
  filter(n == 7) %>%
  summarise(median_7_day_ppt_sum = median(sum_ppt),
            min_7_day_ppt_sum = min(sum_ppt),
            max_7_day_ppt_sum = max(sum_ppt),
            pct_75_cutoff_7_day_ppt_sum = as.numeric(quantile(sum_ppt, 0.75)),
            pct_90_cutoff_7_day_ppt_sum = as.numeric(quantile(sum_ppt, 0.9)))

median_7d_cutoff = median_7_day_ppts$median_7_day_ppt_sum



## Also calculate medians in dry and rainy season, as we will report these in the manuscript
season_def = read.csv(rainy_seasons_path) %>% 
  dplyr::select(-X) %>%
  rename("year" = "season",
         "season_start" = "start",
         "season_end" = "end") %>%
  mutate(season_start = as.Date(season_start), 
         season_end = as.Date(season_end),
         month_start = month(round_date(season_start, unit = "month")),
         month_end = month(round_date(season_end, unit = "month")))


season_all_7day_ppts = all_7day_ppts %>%
  filter(n == 7) %>%
  mutate(year = year(date), 
         year = ifelse(year == 2011, 2012, year)) %>%
  left_join(season_def, by = "year") %>%
  mutate(season = factor(ifelse(date > season_start & date < season_end, 1, 0), 
                         levels = c(0,1), 
                         labels = c("dry", "rainy"))) %>%
  group_by(season) %>%
  summarise(median_7_day_ppt_sum = median(sum_ppt),
            min_7_day_ppt_sum = min(sum_ppt),
            max_7_day_ppt_sum = max(sum_ppt))

####################################################################################
## Monthly precipitation 
####################################################################################
# Runtime: ~8100 secs/~135 min
tic("30d ppt")
average_30day_ppts = future_map_dfr(unique(all_ppt_data$date), function (d) {
  ret = all_ppt_data %>%
    filter(between(date, d - 30 + 1, d)) %>%
    group_by(pixelID) %>%
    summarize(sum_ppt = sum(ppt, na.rm = T),
              n = n()) %>%
    mutate(date = d)
  return(ret)
})
toc()

saveRDS(average_30day_ppts, paste0(box_data_path, "average_30day_ppts.RDS")) 

df_cutoffs_30d = average_30day_ppts %>%
  filter(n == 30) #remove anything that doesn't sum over a full 30d period

(median_30day_cutoff = as.numeric(median(df_cutoffs_30d$sum_ppt)))
#102.31

####################################################################################
##  60-day precipitation (an important interaction term for assessing precipitation 
##    after an extended dry period where pathogens can accumulate in the environment)
####################################################################################
# Runtime: ~8030 secs/~134 min
tic("60d ppt")
average_60day_ppts = future_map_dfr(unique(all_ppt_data$date), function (d) {
  ret = all_ppt_data %>%
    filter(between(date, d - 60 + 1, d)) %>%
    group_by(pixelID) %>%
    summarize(sum_ppt = sum(ppt, na.rm = T),
              n = n()) %>%
    mutate(date = d)
  return(ret)
})
toc()

future::plan(sequential)
saveRDS(average_60day_ppts, paste0(box_data_path, "average_60day_ppts.RDS")) 
# average_60day_ppts <- readRDS(paste0(box_data_path, "average_60day_ppts.RDS"))

df_cutoffs_60d = average_60day_ppts %>%
  filter(n == 60) #remove anything that doesn't sum over a full 60d period


(med_60day_cutoff = as.numeric(quantile(df_cutoffs_60d$sum_ppt, 0.33)))
#47.86
(high_60day_cutoff = as.numeric(quantile(df_cutoffs_60d$sum_ppt, 0.66)))
#448.29



cutoffs = list(heavy_rain_cutoff = heavy_rain_cutoff_80th, 
               heavy_rain_cutoff_90th = heavy_rain_cutoff_90th,
               median_7d_cutoff = median_7d_cutoff,
               pct_75_cutoff_7_day_ppt_sum = median_7_day_ppts$pct_75_cutoff_7_day_ppt_sum,
               pct_90_cutoff_7_day_ppt_sum = median_7_day_ppts$pct_90_cutoff_7_day_ppt_sum,
               min_7d_sum = median_7_day_ppts$min_7_day_ppt_sum,
               max_7d_sum = median_7_day_ppts$max_7_day_ppt_sum,
               median_7d_dry = season_all_7day_ppts$median_7_day_ppt_sum[season_all_7day_ppts$season == "dry"],
               min_7d_dry = season_all_7day_ppts$min_7_day_ppt_sum[season_all_7day_ppts$season == "dry"],
               max_7d_dry = season_all_7day_ppts$max_7_day_ppt_sum[season_all_7day_ppts$season == "dry"],
               median_7d_wet = season_all_7day_ppts$median_7_day_ppt_sum[season_all_7day_ppts$season == "rainy"],
               min_7d_wet = season_all_7day_ppts$min_7_day_ppt_sum[season_all_7day_ppts$season == "rainy"],
               max_7d_wet = season_all_7day_ppts$max_7_day_ppt_sum[season_all_7day_ppts$season == "rainy"],
               median_30day_cutoff = median_30day_cutoff,
               med_60day_cutoff = med_60day_cutoff, 
               high_60day_cutoff = high_60day_cutoff)


saveRDS(cutoffs, paste0(box_data_path, "ppt_cutoffs.RDS")) 

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()
