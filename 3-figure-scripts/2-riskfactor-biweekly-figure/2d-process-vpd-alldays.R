#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Process vapor pressure deficit data 
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

# read in vpd data ----------------------
df_vpd_original = readRDS(paste0(box_data_path, "washb-bangladesh-vpd.RDS"))

# merge cluster ids into the vpd data ----------------------

## Read in diarrhea data 
df_diar = readRDS(paste0(data_dir, "biweekly_plot_data/d_diar_biweek.RDS"))

clusterids = df_diar %>%
  dplyr::select(clusterid, qgpslat, qgpslong, biweek) %>% 
  distinct()

df_vpd_merged = left_join(df_vpd_original, clusterids, by = c("qgpslat", "qgpslong"))

################################################################
# Check that the number of unique coordinates that have #
# been assigned to a cluster is equal to the number of unique 
# coordinates in the merged df with a cluster
################################################################

# Keep only clusters in the control arm
control_cluster_ids = df_diar %>% filter(intervention == 0) %>% pull(clusterid) %>% unique()
df_vpd_merged = df_vpd_merged %>% filter(clusterid %in% control_cluster_ids)

# Check for equality 
all_control = clusterids %>% filter(clusterid %in% control_cluster_ids) %>% select(qgpslat, qgpslong) %>% distinct()
all_merged = df_vpd_merged %>% select(qgpslat, qgpslong) %>% distinct()
missing_coords = all_control %>% anti_join(all_merged,  by = c("qgpslong", "qgpslat"))

# assert_that(nrow(missing_coords) == 0, msg = glue::glue("{nrow(missing_coords)} coordinates were not matched with a cluster id"))


# function to get mean vpd ----------------------
find_avg_vpd = function(data, biweekly_date){
  filtered_coords = data %>% filter(biweek == biweekly_date, !is.na(vpd)) %>% select(qgpslong, qgpslat) %>% distinct()
  
  while (nrow(filtered_coords) < 10) {
    biweekly_date = data %>% filter(biweek > biweekly_date) %>% pull(biweek) %>% min()
    filtered_coords = data %>% filter(biweek == biweekly_date, !is.na(vpd)) %>% select(qgpslong, qgpslat) %>% distinct()
  }
  
  date_filter = data %>% 
    filter(year == year(biweekly_date), month == month(biweekly_date)) %>% 
    inner_join(filtered_coords, by = c("qgpslong", "qgpslat")) %>% 
    select(-biweek) %>% 
    distinct() 
  
  # get mean and CI
  estimate = washb_mean(date_filter$vpd, id=date_filter$clusterid, print = F) %>% 
    as.data.frame()
  
  # calculate mean in lag period in Celsius
  return(data.frame(mean = estimate$Mean,
                    lb = estimate$`Lower 95%CI`,
                    ub = estimate$`Upper 95%CI`))
  
}

# run for each date 
mean_vpd_df = lapply(date_seq, function(x) find_avg_vpd(data = df_vpd_merged, 
                                                        biweekly_date = x)) %>% bind_rows()

mean_vpd_savedata = data.frame (
  date = master_biweek$biweek_date,
  mean_vpd = mean_vpd_df$mean,
  mean_vpd_lb = mean_vpd_df$lb,
  mean_vpd_ub = mean_vpd_df$ub
)

saveRDS(mean_vpd_savedata, file = paste0(data_dir, glue::glue("biweekly_plot_data/mean_vpd_time_for_plot.RDS")))

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()