#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Empirically define the rainy season for each year
#######################################

rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))


ppt_averages = read.csv(paste0(box_data_path, "full_ppt_averages.csv")) %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-")))


find_five_day_avg = function(end_date){
  filtered_ppt = ppt_averages %>% filter(as.Date(date) <= as.Date(end_date), 
                                         as.Date(date) >= as.Date(end_date) - 4)
  
  return(data.frame(date = end_date, 
                    ppt_roll_avg = mean(filtered_ppt$ppt_mean)))
}

dates = ppt_averages %>% arrange(date) %>% tail(-4) %>% pull(date)

ppt_rolling_avgs = lapply(dates, find_five_day_avg) %>% bind_rows()
over_10mm = ppt_rolling_avgs %>% mutate(rainy_season = ifelse(ppt_roll_avg > 10, TRUE, FALSE))

find_season_range = function(check_date){
  rain_day_before = over_10mm %>% filter(date == as.Date(check_date) - 1) %>% pull(rainy_season)
  rain_day_of = over_10mm %>% filter(date == as.Date(check_date)) %>% pull(rainy_season)
  rain_day_after = over_10mm %>% filter(date == as.Date(check_date) + 1) %>% pull(rainy_season)
  
  start_of_season = !rain_day_before & rain_day_of & rain_day_after
  end_of_season = rain_day_before & rain_day_of & !rain_day_after
  
  return(data.frame(date = check_date,
                    start_of_season = start_of_season,
                    end_of_season = end_of_season))
}

rainy_days = over_10mm %>% filter(rainy_season == 1) %>% pull(date) 
season_ranges = lapply(rainy_days, find_season_range) %>% 
  bind_rows() %>%
  filter(start_of_season | end_of_season)


find_season_dates = function(season_year){
  filter_date = season_ranges %>% filter(year(date) == season_year)
  start_date = min(filter_date$date)
  end_date = max(filter_date$date)
  
  return(data.frame(season = season_year, 
                    start = start_date,
                    end = end_date))
}

season_defs = lapply(c(2012, 2013, 2014, 2015, 2016), find_season_dates) %>% 
  bind_rows() %>% 
  mutate(start = as.Date(start),
         end = as.Date(end))

write.csv(season_defs, "results/season_definitions.csv")


rainy_season_plt = ggplot(ppt_rolling_avgs, aes(x = date, y = ppt_roll_avg)) + 
  geom_line() + 
  geom_vline(data = season_defs, aes(xintercept = start), color = "green", linetype = "dashed") + 
  geom_vline(data = season_defs, aes(xintercept = end), color = "red", linetype = "dashed") + 
  xlab("Date") +
  ylab("5 Day Precipitation \nRolling Average (mm/d)")     

ggsave(paste0(fig_dir, "0-empirical_rainy_season.png"), rainy_season_plt, width = 6, height = 3)

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()