#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# process temperature data 
# to calculate weekly sums with selected lags
# for all days, not just the days with study data. 
# this will be used in the figure showing risk 
# factors over time. 
#######################################

rm(list=ls())

## configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

# create date sequence for plot  ----------------------
master_biweek <- readRDS(paste0(data_dir, "biweekly_plot_data/master_biweek_dates.RDS")) %>% filter(biweek_date <= as.Date("2016-01-21"))
date_seq = as.list(master_biweek$biweek_date)

# read in temperature data ----------------------
df_temp_original = readRDS(paste0(box_data_path, "daily_temperatures_fldas.RDS"))

df_temp = df_temp_original %>% 
  mutate(lat = as.character(qgpslat),
         long = as.character(qgpslong)) %>%
  rename(Date = date)

# merge cluster ids into the temperature data ----------------------

## Read in diarrhea data 
df_diar = readRDS(paste0(data_dir, "biweekly_plot_data/d_diar_biweek.RDS"))

clusterids = df_diar %>%
  filter(intervention == 0) %>% # Keep only clusters in the control arm
  dplyr::select(clusterid, qgpslat, qgpslong, biweek) %>% 
    mutate(
      lat = as.character(qgpslat),
      long = as.character(qgpslong)
    ) %>% 
  select(-qgpslat, -qgpslong) %>% 
  distinct()

df_temp_merged = left_join(clusterids, df_temp, by = c("lat", "long"))

################################################################
# Check that the number of unique coordinates that have #
# been assigned to a cluster is equal to the number of unique 
# coordinates in the merged df with a cluster
################################################################

# Check for equality 
all_control = clusterids %>% select(lat, long) %>% distinct()
all_merged = df_temp_merged %>% select(lat, long) %>% distinct()
missing_coords = all_control %>% anti_join(all_merged,  by = c("long", "lat"))

assert_that(nrow(missing_coords) == 0, msg = glue::glue("{nrow(missing_coords)} coordinates were not matched with a cluster id"))

# Calculate lag time averages
lag_times = data.frame(num_weeks_lag = c(0, 1, 2, 3),
                       start = c(7, 14, 21, 28),
                       end = c(1, 8, 15, 22))

# function to get mean temp within lag period ----------------------
find_avg_7day_temp = function(data, date, lag_time){
  
  filtered_coords = data %>% filter(biweek == date) %>% select(long, lat) %>% distinct()
  
  while (nrow(filtered_coords) < 10) {
    date = data %>% filter(biweek > date) %>% pull(biweek) %>% min()
    filtered_coords = data %>% filter(biweek == date) %>% select(long, lat) %>% distinct()
  }
  
  # subset data frame that defines lag period
  selected_lag = lag_times %>% filter(num_weeks_lag == lag_time)
  
  # subset dates based on lag
  date_filter = data %>% 
    filter(Date >= date - selected_lag[["start"]], 
           Date <= date - selected_lag[["end"]]) %>% 
    inner_join(filtered_coords, by = c("long", "lat")) %>% 
    select(-biweek) %>% 
    distinct()
  
  # convert Kelvin to celsius
  date_filter$temp = date_filter$temp - 273.15
  
  # get mean and CI
  estimate = washb_mean(date_filter$temp, id=date_filter$clusterid, print = F) %>% 
    as.data.frame()

  # calculate mean in lag period in Celsius
  return(data.frame(mean = estimate$Mean,
                    lb = estimate$`Lower 95%CI`,
                    ub = estimate$`Upper 95%CI`))

}


for (n in lag_times$num_weeks_lag){
  # run for each date 
  mean_temp_df = lapply(date_seq, function(x) find_avg_7day_temp(data = df_temp_merged, 
                                                                 date = x,
                                                                 lag_time = n)) %>% bind_rows()
  
  mean_temp_savedata = data.frame(
    date = master_biweek$biweek_date,
    temp_weekavg_weeklag = mean_temp_df$mean,
    temp_weekavg_weeklag_lb = mean_temp_df$lb,
    temp_weekavg_weeklag_ub = mean_temp_df$ub
  )
  
  colnames(mean_temp_savedata) = c("date", glue::glue("temp_weekavg_{n}weeklag"), 
                                   glue::glue("temp_weekavg_{n}weeklag_lb"), glue::glue("temp_weekavg_{n}weeklag_ub"))
  
  saveRDS(mean_temp_savedata, file = paste0(data_dir, glue::glue("biweekly_plot_data/temp_mean_{n}wklag_time_for_plot.RDS")))
}



