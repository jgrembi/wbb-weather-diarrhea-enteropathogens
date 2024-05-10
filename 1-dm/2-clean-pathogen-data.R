#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# clean bangladesh EE midline enteropathogen data
#######################################
rm(list=ls())

drop_last_char = function(x){
  return(substr(x, 1, nchar(x)-1))
}

## configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
library(rdrop2)

#--------------------------------------------
# set up Box and Dropbox authentication
#--------------------------------------------
my_dropbox_token <- drop_auth(cache = F)

#--------------------------------------------
# define raw data paths for Box
#--------------------------------------------
b_raw_ee_pathogen_path_box = "pathCounts.RDS"
raw_washb_path_box = paste0(local_root_path, local_box_path, "raw-washb/")
b_raw_baseline_path_box =  "washb-bangladesh-enrol.csv"
b_raw_ee_midline_medHist = "ChildID_Midline_Cleaned_MatchedwEnrollment_2Feb16.dta"

#--------------------------------------
# load and clean raw bangladesh ee midline pathogen data
#--------------------------------------
p = readRDS(paste0(raw_washb_path_box, b_raw_ee_pathogen_path_box)) %>%
  filter(childid != 68071) # This is a stool sample that we have for which we have no information on consent/collection. Likely mislabeled, but discrepancy never identified, so discard.

p = p %>%
  mutate(dataid = as.numeric(drop_last_char(childid))) %>%
  select(childid, dataid, `Campylobacter jejuni/coli`:birthord, clusterid:block, st_agem2, st_aged2, st_date2) %>%
  # rename to match other datasets
  rename(aged = st_aged2,
         agem = st_agem2) %>%
  mutate(agey = aged/365.25)

## define virus, bacteria, parasite groups
virus = c("Norovirus", "Astrovirus", "Adenovirus 40/41", "Rotavirus", "Sapovirus")
bacteria = c("EAEC", "ETEC.any", "EPEC.any", "STEC", "Shigella spp./EIEC", "Salmonella", "B.fragilis", "H.pylori", "V.cholerae", "C.difficile", "Plesiomonas", "Campylobacter spp.", "Aeromonas")
parasite = c("Ancyclostoma", "pan Entamoeba", "Giardia", "Cryptosporidium", "Ascaris", "Trichuris",  "Schistosoma", "Cyclospora", "Isospora","Blastocystis", "E.bieneusi")

## calculate number of viruses, bacteria, parasites for each child
n_paths = p %>% 
  select(all_of(virus), childid) %>%
  mutate_at(vars(all_of(virus)), function(x) ifelse(x > 1.8495, 1, 0)) %>%
  mutate(n_virus = rowSums(.[setdiff(names(.), "childid")]),
         pos_virus = ifelse(is.na(n_virus), NA, ifelse(n_virus == 0, 0, 1))) %>%
  select(childid, n_virus, pos_virus) %>%
  left_join(p %>% 
              select(all_of(bacteria), childid) %>%
              mutate_at(vars(all_of(bacteria)), function(x) ifelse(x > 1.8495, 1, 0)) %>%
              mutate(n_bact = rowSums(.[setdiff(names(.), "childid")]),
                     pos_bact = ifelse(is.na(n_bact), NA, ifelse(n_bact == 0, 0, 1))) %>%
              select(childid, n_bact, pos_bact), by = "childid") %>%
  left_join(p %>% 
              select(all_of(parasite), childid) %>%
              mutate_at(vars(all_of(parasite)), function(x) ifelse(x > 1.8495, 1, 0)) %>%
              mutate(n_parasite = rowSums(.[setdiff(names(.), "childid")]),
                     pos_parasite = ifelse(is.na(n_parasite), NA, ifelse(n_parasite == 0, 0, 1))) %>%
              select(childid, n_parasite, pos_parasite), by = "childid") %>%
  mutate(n_pathogens = rowSums(.[c("n_virus", "n_bact", "n_parasite")]))


## create indicators of positive test for prevalence
p = p %>% 
  mutate(
    `pos_EAEC`    = ifelse(`EAEC` > 1.8495, 1, 0),
    `pos_ST_ETEC` = ifelse(`ST-ETEC` > 1.8495, 1, 0),
    `pos_LT_ETEC` = ifelse(`LT-ETEC` > 1.8495, 1, 0),
    `pos_aEPEC`   = ifelse(`aEPEC` > 1.8495, 1, 0),
    `pos_tEPEC`   = ifelse(`tEPEC` > 1.8495, 1, 0),
    `pos_STEC`    = ifelse(`STEC` > 1.8495, 1, 0),
    `pos_Aeromonas` = ifelse(`Aeromonas` > 1.8495, 1, 0),
    `pos_B.fragilis` = ifelse(`B.fragilis` > 1.8495, 1, 0),
    `pos_Campylobacter` = ifelse(`Campylobacter spp.` > 1.8495, 1, 0),
    `pos_C.difficile` = ifelse(`C.difficile` > 1.8495, 1, 0),
    `pos_Plesiomonas` = ifelse(`Plesiomonas` > 1.8495, 1, 0),
    `pos_Shigella_EIEC` = ifelse(`Shigella spp./EIEC` > 1.8495, 1, 0),
    `pos_Adenovirus40_41` = ifelse(`Adenovirus 40/41` > 1.8495, 1, 0),
    `pos_Norovirus` = ifelse(`Norovirus` > 1.8495, 1, 0),
    `pos_Sapovirus` = ifelse(`Sapovirus` > 1.8495, 1, 0),
    `pos_Cryptosporidium` = ifelse(`Cryptosporidium` > 1.8495, 1, 0),
    `pos_E.bieneusi` = ifelse(`E.bieneusi` > 1.8495, 1, 0),
    `pos_Giardia` = ifelse(`Giardia` > 1.8495, 1, 0),
    `pos_ETEC_any` = ifelse(ETEC.any > 1.84995, 1, 0),
    `pos_EPEC_any` = ifelse(EPEC.any > 1.84995, 1, 0)) %>%
  rename(Adenovirus40_41 = `Adenovirus 40/41`, 
         Campylobacter = `Campylobacter spp.`, 
         Shigella_EIEC = `Shigella spp./EIEC`,
         ST_ETEC = `ST-ETEC`,
         LT_ETEC = `LT-ETEC`) %>%
  left_join(n_paths, by = "childid") %>%
  ## recode to NA all non-detects (qty = 1.8495)
  mutate(across(.cols = `Campylobacter jejuni/coli`:Norovirus, ~ifelse(.x > 2, .x, NA))) 


#--------------------------------------
# load enrolment/baseline bangladesh data
#--------------------------------------
bbase = read.csv(paste0(raw_washb_path_box, b_raw_baseline_path_box)) %>% 
  # mutate(dataid = as.character(dataid)) %>%
  select(-svydate, -fracode, -hhid)

## load additional animal variables from raw baseline bangladesh data
baseline_animal_data = drop_read_csv("/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/1_Baseline/1. WASHB_Baseline_main_survey.csv") %>%
  select(dataid, q4114_1h, q4114_2h, q4114_3h) %>%
  rename(n_cow = q4114_1h, 
         n_goat = q4114_2h, 
         n_chkn = q4114_3h)

bbase = bbase %>%
  left_join(baseline_animal_data, by = "dataid")

#--------------------------------------
# create PCA-based household asset/wealth index
# using enrollment data
#--------------------------------------
# There are 2 important caveats to this calculation:
# 1) This index is created on the baseline household survey data so does not capture changes over the course of the study or household wealth at the time of diarrhea/pathogen measurement.
# 2) We assume that all households within a compound are in the same wealth quartile. Wealth index is calculated at the compound level using data from only 1 household 
#   (because that's all we collected). For example, older children from another family in the compound that are included in the wbb_giardia or 
#   wbb_sth datasets are assumed to have the same wealth index of the index child in that compound.
asset.vars <- c("roof","walls", "floor", "cement", "elec", "asset_radio", 
                "asset_tvbw", "asset_tvcol", "asset_refrig", "asset_bike", 
                "asset_moto", "asset_sewmach", "asset_phone", "asset_tv", 
                "asset_wardrobe", "asset_table", "asset_chair", "asset_clock", 
                "asset_khat", "asset_chouki", "asset_mobile", "n_asset_wardrobe", 
                "n_asset_table", "n_asset_chair", "n_asset_clock", "n_asset_khat", 
                "n_asset_chouki", "n_asset_mobile", "latown", "landacre", "n_cow", "n_goat", "n_chkn")

wealth.index <- assetPCA(bbase, varlist = asset.vars)

## merge wealth index and baseline data, remove variables included in wealth index
bbase = bbase %>%
  left_join(wealth.index, by = c("dataid", "union")) %>%
  select(-(all_of(asset.vars)))


#--------------------------------------
# merge pathogen data with all covariate data
#--------------------------------------
## merge baseline covariate data
nrow(bbase) 
nrow(p)
bdata = left_join(p, bbase, by = c("clusterid", "dataid", "block") )
nrow(bdata)


## merge in compound-level animal counts at time of stool collection
midline_animal_data = drop_read_csv("/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/2_Midline/1. WASHB_Midline_main_survey_cleaned.csv") %>% 
  select(dataid, q4114_1com, q4114_2com, q4114_3com) %>%
  rename(ncow_c = q4114_1com,
         ngoat_c = q4114_2com,
         nchicken_c = q4114_3com) %>%
  mutate(#dataid = as.character(dataid),
    cow_cat_c = factor(ifelse(is.na(ncow_c), NA, ifelse(ncow_c == 0, 0, ifelse(ncow_c < 3, 1, 2))), labels = c("0", "1-2", "3+")),
    goat_cat_c = factor(ifelse(is.na(ngoat_c), NA, ifelse(ngoat_c == 0, 0, ifelse(ngoat_c < 3, 1, 2))), labels = c("0", "1-2", "3+")),
    chkn_cat_c = factor(ifelse(is.na(nchicken_c), NA, ifelse(nchicken_c == 0, 0, ifelse(nchicken_c < 11, 1, 2))), labels = c("0", "1-10","11+")),
    cow_median_c = factor(ifelse(ncow_c > median(.$ncow_c), "above median", "at or below median"), levels = c("at or below median", "above median")),
    goat_median_c = factor(ifelse(ngoat_c > median(.$ngoat_c), "above median", "at or below median"), levels = c("at or below median", "above median")),
    chkn_median_c = factor(ifelse(nchicken_c > median(.$nchicken_c), "above median", "at or below median"), levels = c("at or below median", "above median")),)


bdata = left_join(bdata, midline_animal_data, by = c("dataid"))


## merge in antibiotic use at time of stool collection
abx = haven::read_dta(paste0(raw_washb_path_box, b_raw_ee_midline_medHist))

abx = abx %>%
  mutate(dataid = as.numeric(dataid), 
         abx_7d = factor(ifelse(q18 == 99 | is.na(q18), NA, ifelse(q18 == 88, 0, ifelse(q18_days < 8 & q18_days > 0, 1, 0))), levels = c(0,1)),
         childid = as.numeric(paste0(dataid, childno))) %>%
  select(childid, dataid, abx_7d)

bdata = left_join(bdata, abx, by = c("childid", "dataid"))

#--------------------------------------
# create new covariate variables
#--------------------------------------
## create month, year, quarter, season and wsh/nutrition variables 
bdata = bdata %>%
  mutate(date = as.Date(st_date2, format = "%Y-%m-%d"),
         month = month(date),
         year = year(date)) %>% 
  mutate(wsh = factor(ifelse(tr %in% c("WSH", "N+WSH"), "WSH", "no WSH")),
         nutrition = factor(ifelse(tr %in% c("N+WSH", "Nutrition"), "Nutrition", "no Nutrition")))


#--------------------------------------
# recode sparse covariates
#--------------------------------------
bdata = bdata %>%
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
bdata = bdata %>%
  mutate(across(.cols = c(aged, Nhh, momeduy, dadeduy, nchicken_c, ncow_c, ngoat_c), 
                ~ as.numeric(scale(.x, center = T, scale = F)), .names = "{.col}_C"))



#--------------------------------------
# remove extra covariates
#--------------------------------------
bdata = bdata %>%
  select(-(dadagri:hwsws), -st_date2)


#--------------------------------------
# merge in lat long
#--------------------------------------
gps = read.csv(paste0(raw_washb_path_box, "gps_27aug2013_corrected_temporary.csv"))

gps = gps %>% 
  select(c(dataid, qgpslong, qgpslat, clusterid, block)) 


bdata = left_join(bdata, gps, by = c("dataid", "block", "clusterid")) %>%
  mutate(clusterid = as.factor(clusterid),
         dataid = as.factor(dataid),
         tr = as.factor(tr)) 

#--------------------------------------
# Save clean data
#--------------------------------------
saveRDS(bdata, paste0(clean_washb_path_box, clean_bdata_pathogen_box))

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()
