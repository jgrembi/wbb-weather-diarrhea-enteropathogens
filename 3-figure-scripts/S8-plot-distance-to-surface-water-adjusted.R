#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Script to plot forest plots of pathogens, diarrhea, and distance to surface water

# Adjusted models

# @prerequisite Box intermediate RDS file must be updated and corrected (PR values)
# using the correct-RDS-files-2.R script
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))


# read in results --------------------------------------------------
prev_table = readRDS(paste0(data_dir, "corrected_PR_tables_nointeraction.RDS"))

drop_ecoli <- c("LT-ETEC", "ST-ETEC", "aEPEC", "tEPEC")

adj = prev_table %>%
  filter(Group == "Adj.", grepl("surface_water_tertile", risk_factor), ! grepl("Bruising", Outcome)) %>% 
  filter(!Outcome %in% drop_ecoli) %>%
  mutate(risk_factor = factor(risk_factor, 
                              levels = c(
                                "distance_from_any_surface_water_tertile",
                                "distance_from_ephemeral_surface_water_tertile",
                                "distance_from_seasonal_surface_water_tertile"))) %>% 
  mutate(Pathogen_Type = case_when(
    outcome_name == "diar7d" ~ "Diarrhea",
    outcome_name == "pos_Adenovirus40_41" ~ "Virus",
    outcome_name == "pos_aEPEC" ~ "Bacteria",
    outcome_name == "pos_Aeromonas" ~ "Bacteria",
    outcome_name == "pos_B.fragilis" ~ "Bacteria",
    outcome_name == "pos_C.difficile" ~ "Bacteria",
    outcome_name == "pos_Campylobacter" ~ "Bacteria",
    outcome_name == "pos_Cryptosporidium" ~ "Parasite",
    outcome_name == "pos_E.bieneusi" ~ "Parasite",
    outcome_name == "pos_EAEC" ~ "Bacteria",
    outcome_name == "pos_ETEC_any" ~ "Bacteria",
    outcome_name == "pos_EPEC_any" ~ "Bacteria",
    outcome_name == "pos_Giardia" ~ "Parasite",
    outcome_name == "pos_LT_ETEC" ~ "Bacteria",
    outcome_name == "pos_Norovirus" ~ "Virus",
    outcome_name == "pos_parasite" ~ "Parasite",
    outcome_name == "pos_Plesiomonas" ~ "Bacteria",
    outcome_name == "pos_Sapovirus" ~ "Virus",
    outcome_name == "pos_Shigella_EIEC" ~ "Bacteria",
    outcome_name == "pos_ST_ETEC" ~ "Bacteria",
    outcome_name == "pos_STEC" ~ "Bacteria",
    outcome_name == "pos_tEPEC" ~ "Bacteria",
    outcome_name == "pos_virus" ~ "Virus"),
    RF = recode(RF, 'Distance Tertile From Seasonal Surface Water' = 'Seasonal Surface Water',
                     'Distance Tertile From Ephemeral Surface Water' = 'Ephemeral Surface Water',
                     'Distance Tertile From Any Surface Water' = 'Any Surface Water'),
    category = factor(RF_Type, levels = c("Medium vs. Far", "Close vs. Far")),
    Pathogen_Type = recode(Pathogen_Type, 'Non-pathogen' = 'Diarrhea'))


# make plot ---------------------------------------------------

(bacteria_plot <- ggplot(adj %>% filter(Pathogen_Type=="Bacteria"), aes(x=Outcome, y=PR, color=category)) +
  geom_hline(yintercept=1) +
  geom_pointrange(aes(ymin = PR.Lower, ymax=PR.Upper), position = position_dodge(.65)) +
  ylab("Prevalence Ratio (95% CI)") +
  coord_flip() +
  scale_colour_manual(values = c("royalblue",  "skyblue2"),
                      labels = c("Middle tertile vs. Farthest tertile", "Nearest tertile vs. Farthest tertile")) + 
  scale_y_continuous(trans="log", limits = c(0.2, 3), breaks = c(0.3, 0.5, 1, 1.5, 2), 
                     labels=c(0.2, 0.5, 1, 1.5, 2)) +
  scale_x_discrete(labels = c("EPEC" = "EPEC", "STEC" = "STEC", 
                              "ETEC" = "ETEC", "EAEC" = "EAEC",
                              "Campylobacter" = expression(italic("Campylobacter")),
                              "Plesiomonas" = expression(italic("Plesiomonas")),
                              "Shigella/EIEC" = expression(paste(italic("Shigella"), "/EIEC")),
                              "C. difficile" = expression(italic("C. difficile")),
                              "B. fragilis" = expression(italic("B. fragilis")),
                              "Aeromonas" = expression(italic("Aeromonas"))),
                   limits = c("STEC", "ETEC", "EPEC", "EAEC", "Shigella/EIEC", "Plesiomonas", 
                              "C. difficile", "Campylobacter", "B. fragilis", "Aeromonas")) +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  ggtitle("D) Bacteria") +
  facet_wrap(~RF) + guides(color = guide_legend(reverse = T)))

parasite_plot <- ggplot(adj %>% filter(Pathogen_Type == "Parasite"), 
                        aes(x=Outcome, y=PR, color=category)) +
  geom_hline(yintercept=1) +
  geom_pointrange(aes(ymin = PR.Lower, ymax=PR.Upper), position = position_dodge(.65)) +
  ylab("Prevalence Ratio (95% CI)") +
  coord_flip() +
  scale_x_discrete(labels = c("Any Parasite" = "Any parasite", "E. bieneusi" = expression(italic("E. bieneusi")), 
                              "Giardia" = expression(italic("Giardia")), "Cryptosporidium" = expression(italic("Cryptosporidium"))), 
                   limits = c("Any Parasite", "Giardia", "E. bieneusi", "Cryptosporidium")) +
  scale_colour_manual(values = c("royalblue",  "skyblue2"),
                      labels = c("Middle tertile vs. Farthest tertile", "Nearest tertile vs. Farthest tertile")) +  
  scale_y_continuous(trans="log", limits = c(0.2, 3), breaks = c(0.3, 0.5, 1, 1.5, 2), 
                     labels=c(0.2, 0.5, 1, 1.5, 2)) +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 15)) +
  theme(strip.background = element_blank()) + 
  ggtitle("C) Parasites") + facet_wrap(~RF)

virus_plot <- ggplot(adj %>% filter(Pathogen_Type == "Virus"), 
                     aes(x=Outcome, y=PR, color=category)) +
  geom_hline(yintercept=1) +
  geom_pointrange(aes(ymin = PR.Lower, ymax=PR.Upper), position = position_dodge(.65)) +
  ylab("Prevalence Ratio (95% CI)") +
  coord_flip() +
  scale_x_discrete(labels = c("Any Virus" = "Any virus", "Adenovirus 40/41" = "Adenovirus 40/41", 
                              "Sapovirus" = "Sapovirus", "Norovirus" = "Norovirus"), 
                   limits = c("Any Virus", "Sapovirus", "Norovirus", "Adenovirus 40/41")) +
  scale_colour_manual(values = c("royalblue",  "skyblue2"),
                      labels = c("Middle tertile vs. Farthest tertile", "Nearest tertile vs. Farthest tertile")) +  
  scale_y_continuous(trans="log", limits = c(0.2, 3), breaks = c(0.3, 0.5, 1, 1.5, 2), 
                     labels=c(0.2, 0.5, 1, 1.5, 2)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  #remove x axis labels
        axis.ticks.x=element_blank(),  #remove x axis ticks
        plot.title = element_text(size = 15)) +
  theme(strip.background = element_blank()) +
  ggtitle("B) Viruses") + 
  facet_wrap(~RF)

diarr_plot <- ggplot(adj %>% 
                       filter(Pathogen_Type == "Diarrhea") %>%
                       mutate(Outcome = "7-Day Diarrhea"), 
                     aes(x=Outcome, y=PR, color=category)) + 
  geom_hline(yintercept=1) +
  geom_pointrange(aes(ymin = PR.Lower, ymax=PR.Upper), position = position_dodge(.65)) +
  ylab("Prevalence Ratio (95% CI)") +
  coord_flip() +
  scale_colour_manual(values = c("royalblue",  "skyblue2"),
                      labels = c("Middle tertile vs. Farthest tertile", "Nearest tertile vs. Farthest tertile")) +  
  scale_x_discrete(labels = c("7-Day Diarrhea" = "7-Day Diarrhea")) +
  scale_y_continuous(trans="log", limits = c(0.2, 3), breaks = c(0.3, 0.5, 1, 1.5, 2), 
                     labels=c(0.2, 0.5, 1, 1.5, 2)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),  #remove x axis labels
        axis.ticks.x=element_blank(),  #remove x axis ticks
        plot.title = element_text(size = 15)) + 
  ggtitle("A) Diarrhea") +
  theme(strip.background = element_blank()) +
  facet_wrap(~RF)

aligned_plots <- plot_grid(diarr_plot, virus_plot, parasite_plot, ncol = 1, nrow = 3, 
                           rel_heights = c(3, 5, 5), rel_widths = (c(1, 1, 1)), align = "hv")
legend <- get_legend(bacteria_plot)

disease_plot <- grid.arrange(aligned_plots, bacteria_plot + theme(legend.position = "none"), ncol = 2, nrow = 1)
(combined_plot <- grid.arrange(disease_plot, legend, heights = c(1, .1)))

ggsave(combined_plot, filename = paste0(fig_dir, "S8-plot-distance-water-adjusted.tiff"), 
       width = 15, height = 8)

