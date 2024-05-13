#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Evaluate diarrhea prevalence based on season
#######################################

rm(list=ls())

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

d_diarrhea <- readRDS(paste0(clean_offset_data_dir, clean_diarr_merged_offset)) %>%
  filter(intervention == 0)

## diarrhea prevalence

d_diarrhea %>%
  summarise(prev = mean(diar7d))

## diarrhea prevalence by season
d_diarrhea %>% 
  group_by(season) %>%
  summarise(prev = mean(diar7d))

# determine if this is statistically significantly different
res <- glm(diar7d ~ season, data = d_diarrhea, family = "poisson"(link=log))
summary(res)

vcovCL <- sandwichSE(fm = res, cluster = d_diarrhea$dataid)
rfit <- coeftest(res, vcovCL)[,] %>%
  as.data.frame %>%
  rownames_to_column()

estimate <- rfit$Estimate[2]
SE <- rfit$`Std. Error`[2]
PR <- exp(estimate)
PR_lower <- exp(estimate - qnorm(0.975)*SE)
PR_upper <- exp(estimate + qnorm(0.975)*SE)

PR
PR_lower
PR_upper

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()