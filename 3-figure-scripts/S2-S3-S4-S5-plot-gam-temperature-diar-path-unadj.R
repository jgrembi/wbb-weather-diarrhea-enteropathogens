#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Script to plot temperature continuous risk factors 
# Diarrhea, control arm and pathogen outcomes 

# Adjusted models
#######################################

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/0-utils/1-plot-functions.R"))

# define results path in Box 
diar0_results_dir <- paste0(offset_results_path, "gam_outputs/diarrhea-adjusted-0/")
pathogen_results_dir <- paste0(offset_results_path, "gam_outputs/pathogens-adjusted/")

# define list of temperature variable names
temp_vars <- list("temp_weekmax_0weeklag_C", "temp_weekmax_1weeklag_C",
                  "temp_weekmin_0weeklag_C", "temp_weekmin_1weeklag_C",
                  "temp_monthmax_0weeklag_C", "temp_monthmin_0weeklag_C","temp_monthavg_0weeklag_C",
                  "temp_monthmax_1weeklag_C", "temp_monthmin_1weeklag_C","temp_monthavg_1weeklag_C",
                  "temp_weekavg_0weeklag_C",
                  "temp_weekavg_1weeklag_C",
                  "temp_weekavg_2weeklag_C",
                  "temp_weekavg_3weeklag_C")
#------------------------------------------------------
## Make plots
#------------------------------------------------------
set.seed(22242)
plot_diar0_list <- lapply(temp_vars %>% keep(!grepl("0weeklag_", temp_vars)) , function(x) 
  prep_gam_plot(results_directory = diar0_results_dir, 
                     yname = "diar7d_0_", 
                     risk_factor = x)) 

plot_diar0_data <- plot_diar0_list %>% bind_rows() %>% 
  mutate(outcome = "Diarrhea - control arm")

# pathogen outcomes
path_outcomes <- c(
  "pos_Adenovirus40_41_",
  # "pos_aEPEC_",
  "pos_Aeromonas_",
  "pos_B_fragilis_",
  "pos_C_difficile_",
  "pos_Campylobacter_",
  "pos_Cryptosporidium_",
  "pos_E_bieneusi_",
  "pos_EAEC_",
  "pos_EPEC_any_",
  "pos_ETEC_any_",
  "pos_Giardia_",
  # "pos_LT_ETEC_",
  "pos_Norovirus_",
  "pos_parasite_",
  "pos_Plesiomonas_",
  "pos_Sapovirus_",
  "pos_Shigella_EIEC_",
  # "pos_ST_ETEC_",
  "pos_STEC_",
  # "pos_tEPEC_",
  "pos_virus_"
)

plot_pathogen_list <- list()

set.seed(22242)
for(i in 1:length(path_outcomes)){

  print(path_outcomes[i])
  plot_pathogen_y_list <- lapply(temp_vars, function(x) 
    prep_gam_plot(results_directory = pathogen_results_dir, 
                    yname = path_outcomes[i], 
                    risk_factor = x)) 
  plot_pathogen_list[[i]] <- plot_pathogen_y_list %>% bind_rows() %>% 
    mutate(outcome = path_outcomes[i])
}

plot_pathogen_data <- plot_pathogen_list %>% bind_rows()

prep_for_plot <- function(data){
  data %>% mutate(lag = case_when(
    risk_factor == "temp_weekavg_0weeklag_C" ~ "0 week lag",
    risk_factor == "temp_weekavg_1weeklag_C" ~ "1 week lag",
    risk_factor == "temp_weekavg_2weeklag_C" ~ "2 week lag",
    risk_factor == "temp_weekavg_3weeklag_C" ~ "3 week lag",
    TRUE ~ "NA"
  )) %>% 
    mutate(measure = case_when(
      grepl("min", risk_factor) ~ "Minimum",
      grepl("max", risk_factor) ~ "Maximum",
      grepl("avg", risk_factor) ~ "Mean",
      TRUE ~ "NA"
    )) %>% 
    mutate(measure = factor(measure, levels = c("Minimum",
                                                "Mean",
                                                "Maximum"))) %>% 
    mutate(period = case_when(
      grepl("temp_month", risk_factor) ~ "30 days",
      grepl("temp_week", risk_factor) ~ "7 days",
      TRUE ~ "NA"
    ))
}

plot_diar0_data <- prep_for_plot(plot_diar0_data)
plot_pathogen_data <- prep_for_plot(plot_pathogen_data)

pathogen_names <- list("pos_Aeromonas_" = expression(italic("Aeromonas")),
                       "pos_B_fragilis_" = expression(italic("B. fragilis")),
                       "pos_Campylobacter_" = expression(italic("Campylobacter")),
                       "pos_C_difficile_" = expression(italic("C. difficile")),
                       "pos_Plesiomonas_" = expression(italic("Plesiomonas")),
                       "pos_Shigella_EIEC_" = expression(paste(italic("Shigella"), "/EIEC")),
                       "pos_EAEC_" = "EAEC",
                       "pos_EPEC_any_" = "EPEC",
                       "pos_ETEC_any_" = "ETEC",
                       # "pos_aEPEC_" = "aEPEC",
                       # "pos_tEPEC_" = "tEPEC",
                       # "pos_LT_ETEC_" = "LT-ETEC",
                       # "pos_ST_ETEC_" = "ST-ETEC",
                       "pos_STEC_" = "STEC",
                       "pos_Cryptosporidium_" = expression(italic("Cryptosporidium")),
                       "pos_E_bieneusi_" = expression(italic("E. bieneusi")),
                       "pos_Giardia_" = expression(italic("Giardia")),
                       "pos_parasite_" = "Any parasite",
                       "pos_Adenovirus40_41_" = "Adenovirus 40/41",
                       "pos_Norovirus_" = "Norovirus",
                       "pos_Sapovirus_" = "Sapovirus",
                       "pos_virus_" = "Any virus")

pathogen_labeller <- function(variable, value){
  return(pathogen_names[value])
}

outcome_order <- c("pos_Aeromonas_",
                   "pos_B_fragilis_",
                   "pos_Campylobacter_",
                   "pos_C_difficile_",
                   "pos_Plesiomonas_",
                   "pos_Shigella_EIEC_",
                   "pos_EAEC_",
                   "pos_EPEC_any_",
                   "pos_ETEC_any_",
                   # "pos_aEPEC_",
                   # "pos_tEPEC_",
                   # "pos_LT_ETEC_",
                   # "pos_ST_ETEC_",
                   "pos_STEC_",
                   "pos_Cryptosporidium_",
                   "pos_E_bieneusi_",
                   "pos_Giardia_",
                   "pos_parasite_",
                   "pos_Adenovirus40_41_",
                   "pos_Norovirus_",
                   "pos_Sapovirus_",
                   "pos_virus_")
  
plot_pathogen_data <- plot_pathogen_data %>% mutate(outcome = factor(outcome, levels = outcome_order))

#------------------------------------------------------
## Fig S2
## Make plot of weekly average temperature with different lags
### Pathogens
#------------------------------------------------------
plot_weekly_avg_path <-
  ggplot(plot_pathogen_data %>% filter(risk_factor %in% c("temp_weekavg_0weeklag_C",
                                                          "temp_weekavg_1weeklag_C",
                                                          "temp_weekavg_2weeklag_C",
                                                          "temp_weekavg_3weeklag_C")) , 
       aes(x = risk_factor_value, y = fit)) + 
  geom_ribbon(aes(ymin = lwrS, ymax= uprS, fill = lag), alpha =0.5) +
  geom_line(aes(col = lag))  +
  facet_wrap(~ outcome, labeller = pathogen_labeller, ncol = 3) +
  ylab("Prevalence (%)") +
  xlab("Weekly average temperature (C)") +
  scale_colour_brewer(palette = "OrRd") +
  scale_fill_brewer(palette = "OrRd") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) 

plot_weekly_avg_path

ggsave(plot_weekly_avg_path,
       filename = paste0(fig_dir, "S2-plot-gam-temp-weeklyavg-pathogens.tiff"),
       width = 8, height = 9)

#------------------------------------------------------
## Fig S3
## Make plot of min, max, mean temp within 7 and 30 days
### Diarrhea control arm
#------------------------------------------------------
plot_temp_other_diar <- ggplot(plot_diar0_data %>% 
                                 filter(grepl("1weeklag", risk_factor)), 
                               aes(x = risk_factor_value, y = fit)) + 
  geom_ribbon(aes(ymin = lwrS, ymax= uprS, fill = period), alpha =0.5) +
  geom_line(aes(col=period), linewidth=0.8)  +
  facet_wrap(.~measure, nrow = 3) +
  ylab("Diarrhea Prevalence (%)") +
  xlab("Temperature (C)") +
  scale_colour_brewer("Period", palette = "Accent") +
  scale_fill_brewer("Period", palette = "Accent") +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) 

plot_temp_other_diar

ggsave(plot_temp_other_diar, 
       filename = paste0(fig_dir, "S3-plot-gam-temp-measures-diar.tiff"),
       width = 4, height = 5.5)


#------------------------------------------------------
## Fig S4
## Make plot of min temp within 7 and 30 days
### Pathogens
#------------------------------------------------------
plot_temp_min_path <- ggplot(plot_pathogen_data %>% 
                 filter(risk_factor %in% c("temp_monthmin_0weeklag_C",
                                           "temp_weekmin_0weeklag_C")),
                               aes(x = risk_factor_value, y = fit)) + 
  geom_ribbon(aes(ymin = lwrS, ymax= uprS, fill = period), alpha =0.5) +
  geom_line(aes(col=period), size=0.8)  +
  facet_wrap(~ outcome, labeller = pathogen_labeller, ncol = 3) +
  ylab("Prevalence (%)") +
  xlab("Temperature (C)") +
  scale_colour_brewer("Period", palette = "Accent") +
  scale_fill_brewer("Period", palette = "Accent") +
  theme_bw() +
  theme(
    legend.position = "bottom")

plot_temp_min_path

ggsave(plot_temp_min_path, 
       filename = paste0(fig_dir, "S4-plot-gam-temp-min-pathogens.tiff"),
       width = 8, height = 9)

#------------------------------------------------------
## Fig S5
## Make plot of max temp within 7 and 30 days
### Pathogens
#------------------------------------------------------
plot_temp_max_path <- ggplot(plot_pathogen_data %>% 
                                filter(risk_factor %in% c("temp_weekmax_0weeklag_C",
                                                          "temp_monthmax_0weeklag_C")), 
                              aes(x = risk_factor_value, y = fit)) + 
  geom_ribbon(aes(ymin = lwrS, ymax= uprS, fill = period), alpha =0.5) +
  geom_line(aes(col=period), size=0.8)  +
  facet_wrap(~ outcome, labeller = pathogen_labeller, ncol = 3) +
  ylab("Prevalence (%)") +
  xlab("Temperature (C)") +
  scale_colour_brewer("Period", palette = "Accent") +
  scale_fill_brewer("Period", palette = "Accent") +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) 

plot_temp_max_path

ggsave(plot_temp_max_path, 
       filename = paste0(fig_dir, "S5-plot-gam-temp-max-pathogens.tiff"),
       width = 8, height = 9)

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()