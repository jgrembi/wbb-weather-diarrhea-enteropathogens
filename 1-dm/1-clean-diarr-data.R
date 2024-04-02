#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# clean bangladesh main survey baseline, midline, & endline diarrhea data
# and EE baseline, midline, & endline diarrhea data
#######################################
rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
## Use the librarian package to install and load the following libraries only used in this script
shelf(rdrop2)

#--------------------------------------------
# set up Dropbox authentication
#--------------------------------------------
my_dropbox_token <- drop_auth(cache = F)


#--------------------------------------------
# define raw data paths for Box
#--------------------------------------------
raw_washb_path_box = paste0(local_root_path, local_box_path, "raw-washb/")
b_raw_ee_diarrhea_path_box = "wbb-ee-diar-all-timepoints.RDS"
b_raw_baseline_path_box =  "washb-bangladesh-enrol.csv"

#--------------------------------------
# load and clean raw bangladesh diarrhea data
#--------------------------------------
## load main survey diarrhea data 
bd_main_diarr <- drop_read_csv("WASHB-Bangladesh-Data/1-primary-outcome-datasets/washb-bangladesh-diar.csv") %>%
  mutate(svydate = as.Date(svydate, format = "%d%b%Y"), 
         dob = as.Date(dob, format = "%d%b%Y"), 
         cohort = "main diarrhea") %>%
  # Remove all children less than 6 months of age due to issues with accuracy of caregiver diarrhea for this age group
  filter(agedays > 182)

## load EE cohort diarrhea data
bd_ee_diarr <- readRDS(paste0(raw_washb_path_box, b_raw_ee_diarrhea_path_box)) %>%
  rename(tchild = tChild) %>%
  mutate(cohort = "EE diarrhea", 
         sex = ifelse(sex == 0, "female", ifelse(sex == 1, "male", NA))) %>%
  # remove the EE baseline measurement because children are on average ~3mo and caregiver diarrhea reporting is not accurate for this age (lots of false positives)
  filter(svy != 3)

bd_diarr <- bd_main_diarr %>%
  bind_rows(bd_ee_diarr %>%
              select(any_of(names(bd_main_diarr)))) %>%
  rename(aged = agedays, 
         agey = ageyrs) 

#--------------------------------------
# load enrolment/baseline bangladesh data
#--------------------------------------
bbase = read.csv(paste0(raw_washb_path_box, b_raw_baseline_path_box))

bbase = bbase %>% 
  rename(date_baseline_svy = svydate)

## load additional animal variables from raw baseline bangladesh data
baseline_animal_data = drop_read_csv("/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/1_Baseline/1. WASHB_Baseline_main_survey.csv") %>%
  select(dataid, q4114_1h, q4114_2h, q4114_3h, q4114_1com, q4114_2com, q4114_3com) %>%
  rename(n_cow = q4114_1h, 
         n_goat = q4114_2h, 
         n_chkn = q4114_3h, 
         ncow_c = q4114_1com,
         ngoat_c = q4114_2com,
         nchicken_c = q4114_3com) %>%
  mutate(dataid = as.numeric(dataid))

bbase = bbase %>%
  left_join(baseline_animal_data %>%
              select(dataid, n_cow, n_goat, n_chkn), by = "dataid")


#--------------------------------------
# create PCA-based household asset/wealth index
# using enrollment data
#--------------------------------------
# There are 2 important caveats to this calculation:
# 1) This index is created on the baseline household survey data so does not capture changes over
#     the course of the study or household wealth at the time of diarrhea/pathogen measurement.
# 2) We assume that all households within a compound are in the same wealth quartile. Wealth 
#     index is calculated at the compound level using data from only 1 household (because that's
#     all we collected). For example, older children from another family in the compound that 
#     are included are assumed to have the same wealth index of the index child in that compound.
asset.vars <- c("roof","walls", "floor", "cement", "elec", "asset_radio", 
                "asset_tvbw", "asset_tvcol", "asset_refrig", "asset_bike", 
                "asset_moto", "asset_sewmach", "asset_phone", "asset_tv", 
                "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", 
                "asset_khat", "asset_chouki", "asset_mobile", "n_asset_wardrobe", 
                "n_asset_table", "n_asset_chair", "n_asset_clock", "n_asset_khat", 
                "n_asset_chouki", "n_asset_mobile", "latown", "landacre", "n_cow", "n_goat", "n_chkn")

wealth.index <- assetPCA(bbase, varlist = asset.vars)

## merge wealth index and baseline data,  remove variables included in wealth index
bbase = bbase %>%
  left_join(wealth.index, by = c("dataid", "union")) %>%
  select(-(all_of(asset.vars)))


#--------------------------------------
# merge diarrhea data with all covariate data
#--------------------------------------
## merge baseline covariate data
nrow(bd_diarr)
bd_diarr = left_join(bd_diarr, bbase, by = c("dataid", "clusterid", "block"))
nrow(bd_diarr)


## merge in compound-level animal counts at time of stool collection
midline_animal_data = drop_read_csv("/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/2_Midline/1. WASHB_Midline_main_survey_cleaned.csv") %>% 
  select(dataid, q4114_1com, q4114_2com, q4114_3com) %>%
  rename(ncow_c = q4114_1com,
         ngoat_c = q4114_2com,
         nchicken_c = q4114_3com) 

endline_animal_data = drop_read_csv("/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/3_Endline/04. WASHB_Endline_main_survey_cleaned.csv") %>% 
  select(dataid, q4114_1com, q4114_2com, q4114_3com) %>%
  rename(ncow_c = q4114_1com,
         ngoat_c = q4114_2com,
         nchicken_c = q4114_3com) 


all_animal_data = baseline_animal_data %>% # matches main survey baseline data, svy == 0
  select(dataid, ncow_c:nchicken_c) %>%
  mutate(svy = 0) %>% 
  bind_rows(baseline_animal_data %>% # matches EE baseline data, svy == 3
              select(dataid, ncow_c:nchicken_c) %>%
              mutate(svy = 3)) %>%
  bind_rows(midline_animal_data %>% # matches main survey midline data, svy == 1
              mutate(svy = 1)) %>%
  bind_rows(midline_animal_data %>% # matches EE midline data, svy == 4
              mutate(svy = 4)) %>%
  bind_rows(endline_animal_data %>% # matches main survey endline data, svy == 2
              mutate(svy = 2)) %>%
  bind_rows(endline_animal_data %>% # matches EE endline data, svy == 5
              mutate(svy = 5)) %>%
  mutate(cow_cat_c = factor(ifelse(is.na(ncow_c), NA, ifelse(ncow_c == 0, 0, ifelse(ncow_c < 3, 1, 2))), labels = c("0", "1-2", "3+")),
         goat_cat_c = factor(ifelse(is.na(ngoat_c), NA, ifelse(ngoat_c == 0, 0, ifelse(ngoat_c < 3, 1, 2))), labels = c("0", "1-2", "3+")),
         chkn_cat_c = factor(ifelse(is.na(nchicken_c), NA, ifelse(nchicken_c == 0, 0, ifelse(nchicken_c < 11, 1, 2))), labels = c("0", "1-10","11+"))) %>%
  select(dataid, svy, cow_cat_c:chkn_cat_c, ncow_c:nchicken_c)


bd_diarr = left_join(bd_diarr, all_animal_data, by = c("dataid", "svy"))


## get correct assigned study arm from sth dataset
bsthmain = read.csv(paste0(raw_washb_path_box, "sth.csv")) %>%
  select(clusterid, tr) %>%
  unique() 

bd_diarr = bd_diarr %>%
  left_join(bsthmain, by = "clusterid") %>%
  mutate(intervention = as.factor(ifelse(svy == 0 | tr == "Control", 0, 1)),
         tr = factor(tr))


#--------------------------------------
# create new covariate variables
#--------------------------------------
## create month, year, and quarter variables 
bd_diarr = bd_diarr %>%
  mutate(date = as.Date(svydate,"%d%b%Y"),
         month = month(date),
         year = year(date),
         age_cat = as.factor(ifelse(aged <= 1.5*365.25, "<1.5yr", ifelse(aged > 1.5*365.25, "1.5-5yr", NA)))) 

#--------------------------------------
# recode sparse covariates 
#--------------------------------------
bd_diarr = bd_diarr %>%
  mutate(hfiacat = as.factor(ifelse(hfiacat == "Food Secure", "Food Secure", "Food Insecure")),
         dadedu = case_when(is.na(dadeduy) ~ NA_character_,
                            dadeduy == 0 ~ "No education",
                            dadeduy <= 5 ~ "Primary (1-5y)", 
                            dadeduy > 5 ~ "Secondary (>5y)"),
         Nhh_cat = case_when(is.na(Nhh) ~ NA_character_,
                             Nhh <=3 ~ "2-3",
                             Nhh == 4 ~ "4",
                             Nhh <= 6 ~ "5-6",
                             Nhh > 6 ~ "7+")) %>%
  mutate(momedu = factor(momedu, levels = c("No education", "Primary (1-5y)", "Secondary (>5y)")),
         dadedu = factor(dadedu, levels = c("No education", "Primary (1-5y)", "Secondary (>5y)")),
         Nhh_cat = factor(Nhh_cat, levels = c("2-3", "4", "5-6", "7+")),
         Nhh_median = factor(ifelse(Nhh > median(.$Nhh, na.rm = T), "above median", "at or below median"), 
                             levels = c("at or below median", "above median")))

#--------------------------------------
# center continuous covariates
#--------------------------------------
bd_diarr = bd_diarr %>%
  mutate(across(.cols = c(aged, Nhh, momeduy, dadeduy, nchicken_c, ncow_c, ngoat_c), 
                ~ as.numeric(scale(.x, center = T, scale = F)), .names = "{.col}_C"))



#--------------------------------------
# remove extra covariates and observations where is.na(diar7d)
#--------------------------------------
bd_diarr = bd_diarr %>%
  select(-fracode, -(dadagri:hwsws)) %>%
  filter(!is.na(diar7d))


#--------------------------------------
# merge in lat long
#--------------------------------------
gps = read.csv(paste0(raw_washb_path_box, "gps_27aug2013_corrected_temporary.csv"))

gps = gps %>% dplyr::select(c(dataid, qgpslong, qgpslat, clusterid, block)) 

bdata = left_join(bd_diarr, gps, by = c("dataid", "clusterid", "block")) %>%
  mutate(clusterid = as.factor(clusterid),
         dataid = as.factor(dataid))

#--------------------------------------
# Save clean data
#--------------------------------------
saveRDS(bdata, paste0(clean_washb_path_box, clean_bdata_diarr_box))

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()