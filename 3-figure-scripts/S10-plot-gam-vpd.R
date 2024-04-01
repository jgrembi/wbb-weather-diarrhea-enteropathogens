#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Script to plot vapor pressure deficit risk factor
# Diarrhea, control arm, and pathogen adjusted models
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/0-utils/1-plot-functions.R"))

# define results path in Box 
diar0_results_dir <- paste0(offset_results_path, "gam_outputs/diarrhea-adjusted-0/")
pathogen_results_dir <- paste0(offset_results_path, "gam_outputs/pathogens-adjusted/")

# define risk factor variable name
risk_factor <- "vpd_C"


#------------------------------------------------------
## Pre-process data
#------------------------------------------------------
set.seed(22242)
plot_diar_data <- prep_gam_plot(results_directory = diar0_results_dir, 
                                yname = "diar7d_0_", 
                                risk_factor = risk_factor) %>% 
  mutate(outcome = "Diarrhea")


# pathogen outcomes
path_outcomes <- c(
  "pos_Adenovirus40_41_",
  "pos_aEPEC_",
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
  "pos_LT_ETEC_",
  "pos_Norovirus_",
  "pos_parasite_",
  "pos_Plesiomonas_",
  "pos_Sapovirus_",
  "pos_Shigella_EIEC_",
  "pos_ST_ETEC_",
  "pos_STEC_",
  "pos_tEPEC_",
  "pos_virus_"
)

set.seed(22242)
plot_pathogen_data <- map_dfr(path_outcomes, function(outcome) {
  plot_pathogen_y_list <- prep_gam_plot(results_directory = pathogen_results_dir, 
                                        yname = outcome, 
                                        risk_factor = risk_factor) %>% 
    mutate(outcome = outcome)
})
  

#------------------------------------------------------
# trim portion of x-axis where risk factor data is sparse
#------------------------------------------------------
plot_data <- bind_rows(plot_diar_data, plot_pathogen_data)

#------------------------------------------------------
# clean pathogen names, add category
#------------------------------------------------------
drop_ecoli <- c("LT-ETEC", "ST-ETEC", "aEPEC", "tEPEC")

plot_data <- plot_data %>% mutate(outcome_clean = 
                                    clean_pathogen_names(outcome)) %>%
  add_pathogen_category("outcome") %>% 
  mutate(pathogen_category = ifelse(outcome=="Diarrhea", "N/A", pathogen_category)) %>%
  mutate(pathogen_category = factor(pathogen_category, levels = c("Bacteria", "Parasite", "Virus", "N/A"))) %>%
  filter(!outcome_clean %in% drop_ecoli)

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
                   "pos_virus_",
                   "Diarrhea")

plot_data <- plot_data %>% mutate(outcome = factor(outcome, levels = outcome_order))

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
                       "pos_virus_" = "Any virus",
                       "Diarrhea" = "Diarrhea")

pathogen_labeller <- function(variable, value){
  return(pathogen_names[value])
}

#------------------------------------------------------
## Make plot of vapor pressure deficit
#------------------------------------------------------
plot_VPD <- ggplot(plot_data,
                       aes(x = risk_factor_value, y = fit)) + 
  geom_ribbon(aes(ymin = lwrS, ymax= uprS, fill = pathogen_category), alpha =0.5) +
  geom_line(aes(col = pathogen_category))  +
  facet_wrap(~ outcome, labeller = pathogen_labeller, ncol = 5) +
  xlab("Vapor pressure deficit (kPA)") +
  ylab("Prevalence (%)") +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  guides(color = guide_legend(title = "Pathogen category"),
         fill = guide_legend(title = "Pathogen category")) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) 

plot_VPD

ggsave(plot_VPD, 
       filename = paste0(here::here(), "/4-figures/S10-plot-gam-vpd.tiff"),
       width = 8, height = 6)


