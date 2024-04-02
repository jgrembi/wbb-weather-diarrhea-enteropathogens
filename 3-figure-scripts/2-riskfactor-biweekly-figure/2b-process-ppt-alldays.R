#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# process precipitation data 
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

# read in ppt data ----------------------
df_ppt_original = readRDS(paste0(box_data_path, "all_ppt_data.RDS"))

# merge cluster ids into the ppt data ----------------------

## Read in diarrhea data 
df_diar = readRDS(paste0(data_dir, "biweekly_plot_data/d_diar_biweek.RDS"))

clusterids = df_diar %>%
  dplyr::select(clusterid, qgpslat, qgpslong, biweek) %>% 
  distinct()

df_ppt_merged = left_join(df_ppt_original, clusterids, by = c("qgpslat", "qgpslong"))

################################################################
# Check that the number of unique coordinates that have #
# been assigned to a cluster is equal to the number of unique 
# coordinates in the merged df with a cluster
################################################################

# Number of coordinates that have been assigned to a cluster
n_coords_clusterid = nrow(clusterids)

# Number of coordinates that have a cluster when merged
n_coords_merged_ppt = df_ppt_merged %>% 
  filter(!is.na(clusterid)) %>% 
  select(qgpslong, qgpslat) %>% 
  distinct() %>% 
  nrow()

# Keep only clusters in the control arm
control_cluster_ids = df_diar %>% filter(intervention == 0) %>% pull(clusterid) %>% unique()
df_ppt_merged = df_ppt_merged %>% filter(clusterid %in% control_cluster_ids)

# Check for equality 
all_control = clusterids %>% filter(clusterid %in% control_cluster_ids) %>% select(qgpslong, qgpslat) %>% distinct()
all_merged = df_ppt_merged %>% select(qgpslong, qgpslat) %>% distinct()
missing_coords = all_control %>% anti_join(all_merged,  by = c("qgpslong", "qgpslat"))

assert_that(nrow(missing_coords) == 0, msg = glue::glue("{nrow(missing_coords)} coordinates were not matched with a cluster id"))

# Calculate lag time averages
lag_times = data.frame(num_weeks_lag = c(0, 1, 2, 3),
                       start = c(7, 14, 21, 28),
                       end = c(1, 8, 15, 22))

# function to get mean temp within lag period ----------------------
find_total_7day_ppt = function(data, biweekly_date, lag_time){
  filtered_coords = data %>% filter(biweek == biweekly_date) %>% select(qgpslong, qgpslat) %>% distinct()
  
  while (nrow(filtered_coords) < 10) {
    biweekly_date = data %>% filter(biweek > biweekly_date) %>% pull(biweek) %>% min()
    filtered_coords = data %>% filter(biweek == biweekly_date) %>% select(qgpslong, qgpslat) %>% distinct()
  }
  
  # subset data frame that defines lag period
  selected_lag = lag_times %>% filter(num_weeks_lag == lag_time)
  
  # subset dates based on lag
  date_filter = data %>% 
    filter(date >= biweekly_date - selected_lag[["start"]], 
           date <= biweekly_date - selected_lag[["end"]]) %>% 
    inner_join(filtered_coords, by = c("qgpslong", "qgpslat")) %>% 
    select(-biweek) %>% 
    distinct() %>% 
    group_by(qgpslong, qgpslat, clusterid) %>% 
    summarize(total_ppt = sum(ppt, na.rm = T)) %>% 
    ungroup()
  
  # get mean and CI
  estimate = washb_mean(date_filter$total_ppt, id=date_filter$clusterid, print = F) %>% 
    as.data.frame()
  
  # calculate mean in lag period in Celsius
  return(data.frame(mean = estimate$Mean,
                    lb = estimate$`Lower 95%CI`,
                    ub = estimate$`Upper 95%CI`))
  
}

for (n in lag_times$num_weeks_lag){
  # run for each date 
  total_ppt_df = lapply(date_seq, function(x) find_total_7day_ppt(data = df_ppt_merged,
                                                                  biweekly_date = x,
                                                                  lag_time = n)) %>% bind_rows()
  
  total_ppt_savedata = data.frame(
    date = master_biweek$biweek_date,
    ppt_total_weeklag = total_ppt_df$mean,
    ppt_total_weeklag_lb = total_ppt_df$lb,
    ppt_total_weeklag_ub = total_ppt_df$ub
  )
  colnames(total_ppt_savedata) = c("date", glue::glue("ppt_total_{n}weeklag"), 
                                   glue::glue("ppt_total_{n}weeklag_lb"), glue::glue("ppt_total_{n}weeklag_ub"))
  
  saveRDS(total_ppt_savedata, file = paste0(data_dir, glue::glue("biweekly_plot_data/ppt_total_{n}wklag_time_for_plot.RDS")))
}

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()