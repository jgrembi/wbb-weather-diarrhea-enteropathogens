#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Script to plot temperature - weekly average 3 week lag 
# Diarrhea, control arm 
# and pathogen outcomes 
# unadjusted models
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/0-utils/1-plot-functions.R"))


# define results path in Box 
diar0_results_dir <- paste0(offset_results_path, "gam_outputs/diarrhea-adjusted-0/")
pathogen_results_dir <- paste0(offset_results_path, "gam_outputs/pathogens-adjusted/")
# define list of temperature variable names
temp_vars <- list("temp_weekavg_1weeklag_C",
                  "temp_weekavg_2weeklag_C",
                  "temp_weekavg_3weeklag_C")

#------------------------------------------------------
## Preprocess data for plotting 
#------------------------------------------------------
set.seed(22242)
plot_diar0_list <- lapply(temp_vars, function(x) 
  prep_gam_plot(results_directory = diar0_results_dir, 
                     yname = "diar7d_0_", 
                     risk_factor = x) %>%
    select(risk_factor: risk_factor_value,fit:lwrS)) 

plot_diar0_data <- plot_diar0_list %>% bind_rows() %>% 
  mutate(outcome = "Diarrhea")

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
  "pos_ETEC_any_",
  "pos_EPEC_any_",
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

plot_pathogen_list <- list()
set.seed(22242)
for(i in 1:length(path_outcomes)){
  plot_pathogen_y_list <- lapply(temp_vars, function(x) 
    prep_gam_plot(results_directory = pathogen_results_dir, 
                    yname = path_outcomes[i], 
                    risk_factor = x) %>%
      select(risk_factor: risk_factor_value,fit:lwrS)) 
  plot_pathogen_list[[i]] <- plot_pathogen_y_list %>% bind_rows() %>% 
    mutate(outcome = path_outcomes[i])
}

plot_pathogen_data <- plot_pathogen_list %>% bind_rows()

# make plot -------------------------------------------
plot_data <- bind_rows(plot_diar0_data, plot_pathogen_data) %>%
# plot_data <- plot_diar0_data %>%
  mutate(outcome_clean = clean_pathogen_names(outcome)) %>%
  add_pathogen_category("outcome") %>% 
  mutate(pathogen_category = ifelse(outcome=="Diarrhea", "N/A", pathogen_category)) %>% 
  mutate(outcome_cat = case_when(
    outcome=="Diarrhea" ~ "Diarrhea",
    pathogen_category == "Bacteria" ~ "Bacteria",
    pathogen_category == "Parasite" ~ "Parasite",
    pathogen_category == "Virus" ~ "Virus"
  )) %>% 
  mutate(outcome_cat = factor(outcome_cat, levels = c("Diarrhea","Bacteria","Parasite","Virus"))) %>% 
  mutate(outcome_clean = clean_pathogen_names(outcome)) %>% 
  mutate(outcome_clean = factor(outcome_clean, levels = c(
    "aEPEC", "Aeromonas", "B. fragilis", "Campylobacter", "C. difficile", "Plesiomonas", 
    "E. bieneusi" , "EAEC","ETEC", "EPEC", "LT-ETEC" ,  "Shigella/EIEC",  "STEC",  "ST-ETEC" , "tEPEC",          
    "Adenovirus 40/41", "Norovirus", "Sapovirus","Any virus",
    "Cryptosporidium" , "Giardia"  , "Any parasite", 
    "Diarrhea"
  )))


# subset lag based on incubation period -------------------------------------------
plotdata_subset <- plot_data %>% 
  mutate(lag = case_when(
    risk_factor == "temp_weekavg_1weeklag_C" ~ "1 week",
    risk_factor == "temp_weekavg_2weeklag_C" ~ "2 weeks",
    risk_factor == "temp_weekavg_3weeklag_C" ~ "3 weeks")) %>% 
  mutate(keep_lag = case_when(
    outcome_clean == "Adenovirus 40/41" ~ "1 week",
    outcome_clean == "aEPEC" ~ "1 week",
    outcome_clean == "Aeromonas" ~ "1 week",
    outcome_clean == "Any parasite" ~ "2 weeks",
    outcome_clean == "Any virus" ~ "1 week",
    outcome_clean == "B. fragilis" ~ "2 weeks",
    outcome_clean == "C. difficile" ~ "1 week",
    outcome_clean == "Campylobacter" ~ "1 week",
    outcome_clean == "Cryptosporidium" ~ "2 weeks",
    outcome_clean == "Diarrhea" ~ lag,
    outcome_clean == "E. bieneusi" ~ "2 weeks",
    outcome_clean == "EAEC" ~ "1 week",
    outcome_clean == "Giardia" ~ "3 weeks",
    outcome_clean == "LT-ETEC" ~ "1 week",
    outcome_clean == "Norovirus" ~ "1 week",
    outcome_clean == "Plesiomonas" ~ "1 week",
    outcome_clean == "Sapovirus" ~ "1 week",
    outcome_clean == "Shigella/EIEC" ~ "1 week",
    outcome_clean == "ST-ETEC" ~ "1 week",
    outcome_clean == "STEC" ~ "1 week",
    outcome_clean == "tEPEC" ~ "1 week",
    outcome_clean == "EPEC" ~ "1 week",
    outcome_clean == "ETEC" ~ "1 week")) %>% 
  filter(lag == keep_lag)


drop_ecoli <- c("LT-ETEC", "ST-ETEC", "aEPEC", "tEPEC")

diar_data <- plotdata_subset %>% filter(outcome_cat == "Diarrhea")
bacteria_data <- plotdata_subset %>% filter(outcome_cat == "Bacteria",!outcome_clean %in% drop_ecoli)
parasite_data <- plotdata_subset %>% filter(outcome_cat == "Parasite")
virus_data <- plotdata_subset %>% filter(outcome_cat == "Virus")

#------------------------------------------------------
## Make plot of weekly average temperature with different outcomes
#------------------------------------------------------
diarrhea_palette = c("#bdd7e7","#6baed6","#084594")
(diar_plot <- ggplot(diar_data,  
       aes(x = risk_factor_value, y = fit, group=lag)) + 
  geom_ribbon(aes(ymin = lwrS, ymax= uprS, fill=lag), alpha = 0.6) +
    scale_color_manual(values = diarrhea_palette) +
    scale_fill_manual(values = diarrhea_palette) +
    # scale_fill_brewer("Lag period", palette = "Blues") +
  scale_x_continuous(labels = c(15, 20, 25, 30),
                     breaks = c(15, 20, 25, 30)) +
  geom_line(col="black")  +
  ylab("Prevalence (%)") +
  xlab("Weekly average temperature (C)") +
    labs(fill = "Lag period") +
  facet_wrap(~lag, ncol=1)+
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(1/10, "in"),
    legend.key.height= unit(1/10, 'in'),
    legend.key.width= unit(1/10, 'in'),
    legend.title = element_text(size=8),
    legend.text = element_text(size=8),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(size= 10),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) + 
  ggtitle("A) Diarrhea"))


bacteria_data = bacteria_data %>% 
  mutate(pattern = factor(ifelse(outcome_clean %in% c("STEC", "ETEC"), "A_pattern", "B_no pattern"), levels = c("B_no pattern", "A_pattern"))) 

ecoli_palette <- c("firebrick1",
                   "goldenrod1",
                   "mediumslateblue",
                   "black")

pathogenic_ecoli <- c("EAEC", "STEC", "EPEC", "ETEC") 
(pathogenic_ecoli_plot <- ggplot(bacteria_data %>% filter(outcome_clean %in% pathogenic_ecoli),
                                 aes(x = risk_factor_value, y = fit)) + 
    geom_ribbon(aes(ymin = lwrS, ymax= uprS, fill=outcome_clean,
                    alpha = pattern)) +
    scale_color_manual(values = ecoli_palette) +
    scale_fill_manual(values = ecoli_palette) +
    scale_alpha_manual(values = c(0.1, 0.5), guide="none") +
    scale_x_continuous(labels = c(15, 20, 25, 30),
                       breaks = c(15, 20, 25, 30)) +
    geom_line(aes(col=outcome_clean))  +
    ylab("Prevalence (%)") +
    xlab("Weekly average temperature (C)") +
    guides(color = guide_legend(ncol = 3, nrow = ),
           fill = guide_legend(ncol = 3)) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.size = unit(1/10, "in"),
      legend.key.height= unit(1/10, 'in'),
      legend.key.width= unit(1/10, 'in'),
      legend.text = element_text(size=8),
      axis.text = element_text(size = 6),
      axis.title = element_text(size = 8),
      plot.title = element_text(size= 10),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) + 
    ggtitle(expression(paste("B) Pathogenic ", italic("Escherichia coli")))))



bacteria_palette <- c("darkorchid1", 
                      "darkgoldenrod1",
                      "dodgerblue2",
                      "deeppink",
                      "darkgreen")

(bacteria_plot <- ggplot(bacteria_data %>% filter(!outcome_clean %in% pathogenic_ecoli, outcome_clean != "Plesiomonas"),
                    aes(x = risk_factor_value, y = fit)) + 
  geom_ribbon(aes(ymin = lwrS, ymax= uprS, fill=outcome_clean, alpha = pattern)) +
  scale_color_manual(values = bacteria_palette) +
  scale_fill_manual(values = bacteria_palette) +
  scale_alpha_manual(values = c(0.08, 0.5), guide="none") +
  scale_x_continuous(labels = c(15, 20, 25, 30),
                     breaks = c(15, 20, 25, 30)) +
  geom_line(aes(col=outcome_clean))  +
  ylab("Prevalence (%)") +
  xlab("Weekly average temperature (C)") +
  guides(color = guide_legend(ncol = 3, nrow = ),
         fill = guide_legend(ncol = 3)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(1/10, "in"),
    legend.key.height= unit(1/10, 'in'),
    legend.key.width= unit(1/10, 'in'),
    legend.text = element_text(size=8),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(size= 10),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) + 
  ggtitle("C) Other Bacteria"))


parasite_palette = c("#CC61B0", "#5D69B1", "#E58606", "#52BCA3")
parasite_data = parasite_data %>% mutate(pattern = 
                                           factor(ifelse(outcome_clean %in% c("Cryptosporidium"), "A_pattern", "B_no pattern"), #"E. bieneusi" - this was no longer significant after switching to adjusted models
                                                  levels = c("B_no pattern", "A_pattern")))
parasite_plot <- ggplot(parasite_data, 
                        aes(x = risk_factor_value, y = fit)) + 
  geom_ribbon(aes(ymin = lwrS, ymax= uprS, fill=outcome_clean, alpha = pattern)) +
  scale_color_manual(values=parasite_palette) +
  scale_fill_manual(values=parasite_palette) +
  scale_alpha_manual(values = c(0.1, 0.5), guide="none") +
  geom_line(aes(col=outcome_clean))  +
  ylab("Prevalence (%)") +
  xlab("Weekly average temperature (C)") +
  ylim(0, 50) +
  scale_x_continuous(labels = c(15, 20, 25, 30),
                     breaks = c(15, 20, 25, 30)) +
  guides(color = guide_legend(ncol = 2),
         fill = guide_legend(ncol = 2)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(1/10, "in"),
    legend.key.height= unit(1/10, 'in'),
    legend.key.width= unit(1/10, 'in'),
    legend.text = element_text(size=8),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(size= 10)
  ) + 
  ggtitle("D) Parasites")
parasite_plot

virus_palette <- c("#931d54","#f2a527","midnightblue","#29671b")

virus_plot <- ggplot(virus_data %>% mutate(pattern = factor(ifelse(outcome_clean == "Sapovirus", "A_pattern","B_no pattern"), levels = c("B_no pattern", "A_pattern"))),
                        aes(x = risk_factor_value, y = fit)) + 
  geom_ribbon(aes(ymin = lwrS, ymax= uprS, fill=outcome_clean, alpha = pattern)) +
  scale_color_manual(values=virus_palette) +
  scale_fill_manual(values=virus_palette) +
  scale_alpha_manual(values = c(0.1, 0.5), guide="none") +
  geom_line(aes(col=outcome_clean))  +
  ylab("Prevalence (%)") +
  xlab("Weekly average temperature (C)") +
  guides(color = guide_legend(ncol = 2),
         fill = guide_legend(ncol = 2)) +
  scale_x_continuous(labels = c(15, 20, 25, 30),
                     breaks = c(15, 20, 25, 30)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(1/10, "in"),
    legend.key.height= unit(1/10, 'in'),
    legend.key.width= unit(1/10, 'in'),
    legend.text = element_text(size=8),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(size= 10)
  ) + 
  ggtitle("E) Viruses")
virus_plot

# diar_virus <- grid.arrange(diar_plot, virus_plot, parasite_plot, ncol = 1, nrow = 3, heights = c(1.5, 1.5, 1.5))

ecoli_parasite <- grid.arrange(pathogenic_ecoli_plot, parasite_plot, ncol = 1, nrow = 2, heights = c(1, 1))

bacteria_virus <- grid.arrange(bacteria_plot, virus_plot, ncol = 1)

combined_plot <- grid.arrange(diar_plot, ecoli_parasite, bacteria_virus, ncol = 3)

ggsave(combined_plot,
       filename = paste0(here::here(), "/4-figures/3-plot-gam-temp-maintext.tiff"),
       height = 6, width = 10, units = "in")
