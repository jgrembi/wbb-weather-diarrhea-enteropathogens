#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Create master data frame with
# biweekly dates for risk factor plot over time
####################################### 

rm(list=ls())

## configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

## Read in diarrhea data and subset to control arms only.
df_diar = readRDS(paste0(box_data_path, clean_diarr_merged_box))

# Subset data.frame to diarrhea_0 (control arms or baseline measurements from intervention arms before intervention delivery)
d_diar = df_diar %>%
  filter(intervention == 0) %>%
  mutate(diar7d = as.numeric(as.character(diar7d)),
         ym = floor_date(date, "month"),
         yw = floor_date(date, "week"),
         age_cat2 = factor(ifelse(aged < 365, "6mo-1yr", ifelse(aged >= 365 & aged < 730, "1-2yrs", ifelse(aged >= 730, "2+ yrs", "missing"))), 
                           levels = c("6mo-1yr", "1-2yrs", "2+ yrs")),
         personid = paste0(dataid, childid)) 


# make master list of biweekly dates
# this is needed to add NAs so that geom_line doesn't
# connect periods with no measurement
master_biweek = data.frame(date = seq.Date(from = min(d_diar$date),
                                           to = max(d_diar$date), "days")) %>% 
  mutate(year = year(date))  %>% 
  mutate(week2 = ceiling(lubridate::week(date) / 2)) %>% 
  dplyr::select(date, year, week2) %>% 
  distinct() %>% 
  group_by(year, week2) %>% 
  summarise(
    min_date = min(date),
    max_date = max(date)
  ) %>% 
  mutate(biweek_date = min_date+(max_date - min_date)/2)

dir.create(paste0(data_dir, "biweekly_plot_data"))

saveRDS(master_biweek, paste0(data_dir, "biweekly_plot_data/master_biweek_dates.RDS"))


# get the mid-point date for each biweekly period in 
# the study dataset
assign_biweek <- function(date){
  out_date = master_biweek %>% filter(year == year(date)) %>% 
    filter(min_date <= date & max_date >= date) %>% 
    pull(biweek_date)
  if(length(out_date)==0){
    return(NA)
  }else{
    return(out_date)
  }
}

# create biweekly variable
d_diar$biweek=NA
for(i in 1:nrow(d_diar)){
  d_diar$biweek[i] = assign_biweek(d_diar$date[i])
}

d_diar$biweek = as.Date(d_diar$biweek, origin = "1970-01-01")
assert_that(min(d_diar$biweek) == min(master_biweek$biweek_date))

saveRDS(d_diar, paste0(data_dir, "biweekly_plot_data/d_diar_biweek.RDS"))

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()
