#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Script to plot forest plots of pathogens, diarrhea, and precipitation 
# with different lags

# unadjusted models
#######################################
rm(list=ls())
source("0-config.R")

# read in results --------------------------------------------------
prev_table = readRDS(paste0(data_dir, "corrected_PR_tables_nointeraction.RDS"))

# We decided to drop these E. .coli pathotypes because the definition of one excludes the other 
# and we identified potential confounding in the results whereby one decreased most likely because 
# the other was increasing.  Instead we use composite values for any ETEC or any EPEC.
drop_ecoli <- c("LT-ETEC", "ST-ETEC", "aEPEC", "tEPEC")

adj = prev_table %>%
  filter(Group == "Adj.", grepl("ppt_week_sum", risk_factor), !grepl("percentile", RF_Type), ! grepl("Bruising", Outcome)) %>% 
  filter(!Outcome %in% drop_ecoli) %>%
  mutate(risk_factor = factor(risk_factor, 
                              levels = c(
                                "ppt_week_sum_3weeklag_median",
                                "ppt_week_sum_2weeklag_median",
                                "ppt_week_sum_1weeklag_median",
                                "ppt_week_sum_0weeklag_median"))) %>% 
  mutate(category = case_when(
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
    outcome_name == "pos_EPEC_any" ~ "Bacteria",
    outcome_name == "pos_ETEC_any" ~ "Bacteria",
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
    outcome_name == "pos_virus" ~ "Virus"
  ),
  expected_lag = case_when(
    outcome_name %in% c("pos_Cryptosporidium", "pos_E.bieneusi", "pos_Giardia") ~ "2-week lag",
    outcome_name %in% c("diar7d", "pos_parasite", "pos_virus") ~ "NA",
    TRUE ~ "1-week lag"
  ),
  lag = factor(case_when(
    grepl("0weeklag", risk_factor) ~ "0-week lag",
    grepl("1weeklag", risk_factor)  ~ "1-week lag",
    grepl("2weeklag", risk_factor) ~ "2-week lag",
    grepl("3weeklag", risk_factor) ~ "3-week lag",
  ), levels = c("3-week lag", "2-week lag", "1-week lag", "0-week lag")),
  expected_lag_sym = ifelse(lag == expected_lag | outcome_name %in% c("pos_virus", "pos_parasite"), "Yes", "No"))


diar_plot <- ggplot(adj %>% filter(category=="Diarrhea") %>% mutate(Outcome = "7-day Diarrhea"), aes(x=Outcome, y=PR, group=lag)) +
  geom_hline(yintercept=1) +
  geom_pointrange(aes(y = PR, ymin = PR.Lower, ymax=PR.Upper, shape = expected_lag_sym, fill=risk_factor, color=lag), position = position_dodge(.5)) +
  xlab("Diarrhea") +
  ylab("Prevalence Ratio (95% CI)") +
  coord_flip() +
  scale_x_discrete(labels = c("diarrhea-0" = "Diarrhea")) +
  scale_colour_manual(values = c("#024d81",  "#317cb0","#60a6d6", "#97ccf0")) +
  scale_fill_manual(values = c("#024d81",  "#317cb0","#60a6d6", "#97ccf0"), 
                    labels=c("3 week lag", "2 week lag", "1 week lag", "0 week lag")) +
  scale_shape_manual(values = c(21,1)) +
  scale_y_continuous(trans="log", breaks = c(0.25, 0.5, 1, 1.5, 2, 3, 4), 
                     labels=c(0.25, 0.5, 1, 1.5, 2, 3, 4),
                     limits = c(0.25, 5.25)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 15)) +
  ggtitle(label = "A) Diarrhea")

parasite_plot <- ggplot(adj %>% filter(category=="Parasite") %>% arrange(lag), aes(x=Outcome, y=PR, group = lag)) +
  geom_hline(yintercept=1) +
  geom_pointrange(aes(y = PR, ymin = PR.Lower, ymax=PR.Upper, shape = expected_lag_sym, fill = risk_factor, color=lag), position = position_dodge(.65)) +
  ylab("Prevalence Ratio (95% CI)") +
  coord_flip() +
  scale_x_discrete(labels = c("Any Parasite" = "Any parasite", "E. bieneusi" = expression(italic("E. bieneusi")), 
                              "Giardia" = expression(italic("Giardia")), "Cryptosporidium" = expression(italic("Cryptosporidium"))), 
                   limits = c("Any Parasite", "Giardia", "E. bieneusi", "Cryptosporidium")) +
  scale_colour_manual(values = c("#024d81",  "#317cb0","#60a6d6", "#97ccf0"), 
                      labels=c("3 week lag", "2 week lag", "1 week lag", "0 week lag"), 
                      limits = c()) +
  scale_fill_manual(values = c("#024d81",  "#317cb0","#60a6d6", "#97ccf0"), 
                    labels=c("3 week lag", "2 week lag", "1 week lag", "0 week lag")) +
  scale_shape_manual(values = c(1,21)) +
  scale_y_continuous(trans="log", breaks = c(0.25, 0.5, 1, 1.5, 2, 3, 4), 
                     labels=c(0.25, 0.5, 1, 1.5, 2, 3, 4),
                     limits = c(0.25, 5.25)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 15)) + 
  ggtitle(label = "B) Parasites")

virus_plot <- ggplot(adj %>% filter(category=="Virus"), aes(x=Outcome, y=PR, group=lag)) +
  geom_hline(yintercept=1) +
  geom_pointrange(aes(y = PR, ymin = PR.Lower, ymax=PR.Upper, shape = expected_lag_sym, fill=risk_factor, color=lag), position = position_dodge(.65)) +
  ylab("Prevalence Ratio (95% CI)") +
  coord_flip() +
  scale_x_discrete(limits = c("Any Virus", "Sapovirus", "Norovirus", "Adenovirus 40/41")) +
  scale_colour_manual(values = c("#024d81",  "#317cb0","#60a6d6", "#97ccf0"), 
                      labels=c("3 week lag", "2 week lag", "1 week lag", "0 week lag")) +
  scale_fill_manual(values = c("#024d81",  "#317cb0","#60a6d6", "#97ccf0"), 
                    labels=c("3 week lag", "2 week lag", "1 week lag", "0 week lag")) +
  scale_shape_manual(values = c(1,21)) +
  scale_y_continuous(trans="log", breaks = c(0.25, 0.5, 1, 1.5, 2, 3, 4), 
                     labels=c(0.25, 0.5, 1, 1.5, 2, 3, 4),
                     limits = c(0.25, 5.25)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 15)) + 
  ggtitle(label = "C) Viruses")

(bacteria_plot <- ggplot(adj %>% filter(category=="Bacteria"), aes(x=Outcome, y=PR, group = lag)) +
  geom_hline(yintercept=1) +
  geom_pointrange(aes(y = PR, ymin = PR.Lower, ymax=PR.Upper, shape = expected_lag_sym, fill = risk_factor, color=lag), position = position_dodge(.65)) +
  ylab("Prevalence Ratio (95% CI)") +
  coord_flip() +
  scale_x_discrete(labels = c("STEC" = "STEC", "EAEC" = "EAEC", "ETEC" = "ETEC", "EPEC" = "EPEC",
                              "Campylobacter" = expression(italic("Campylobacter")),
                              "Plesiomonas" = expression(italic("Plesiomonas")),
                              "Shigella/EIEC" = expression(paste(italic("Shigella"), "/EIEC")),
                              "C. difficile" = expression(italic("C. difficile")),
                              "B. fragilis" = expression(italic("B. fragilis")),
                              "Aeromonas" = expression(italic("Aeromonas"))),
                   limits = c("STEC", "ETEC", "EPEC", "EAEC", "Shigella/EIEC", "Plesiomonas", 
                              "C. difficile", "Campylobacter", "B. fragilis", "Aeromonas")) +
  scale_colour_manual(values = c("#024d81",  "#317cb0","#60a6d6", "#97ccf0")) +
  scale_fill_manual(values = c("#024d81",  "#317cb0","#60a6d6", "#97ccf0"), 
                    labels=c("3 week lag", "2 week lag", "1 week lag", "0 week lag")) +
  scale_shape_manual(values = c(1,21)) +
  scale_y_continuous(trans="log", breaks = c(0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4), 
                     labels=c(0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4),
                     limits = c(0.25, 6.25)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 15)) +
  ggtitle(label = "D) Bacteria"))

diar_para_vir <- plot_grid(diar_plot, parasite_plot, virus_plot, ncol = 1, nrow = 3, 
                           rel_heights = c(2, 5, 5), rel_widths = (c(1, 1, 1)), align = "hv")

all_disease <- grid.arrange(diar_para_vir, bacteria_plot, ncol = 2, nrow = 1)

legend_plot <- ggplot(adj %>% filter(category=="Bacteria"), aes(x=Outcome, y=PR, color=lag, shape = expected_lag_sym, fill = risk_factor, group = lag)) +
  geom_hline(yintercept=1) +
  geom_pointrange(aes(ymin = PR.Lower, ymax=PR.Upper), position = position_dodge(.5)) +
  xlab("Diarrhea") +
  ylab("Prevalence Ratio (95% CI)") +
  coord_flip() +
  scale_colour_manual(values = c("#024d81",  "#317cb0","#60a6d6", "#97ccf0"), 
                      labels=c("3-week lag", "2-week lag", "1-week lag", "0-week lag")) +
  scale_fill_manual(values = c("#024d81",  "#317cb0","#60a6d6", "#97ccf0"), 
                    labels=c("3-week lag", "2-week lag", "1-week lag", "0-week lag"), guide = "none") +
  scale_shape_manual(values = c(21,19), 
                     labels = c("Less important", "Most relevant")) +
  scale_y_continuous(trans="log", breaks = c(0.25, 0.5, 1, 1.5, 2,  3, 4), 
                     labels=c(0.25, 0.5, 1, 1.5, 2, 3, 4),
                     limits = c(0.25, 5.25)) +
  guides(shape = guide_legend(title = "Expected Lag Period Relevance", reverse = T),
         color = guide_legend(title = "Weekly Sum of Precipitation Above vs. Below Median", reverse = T)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box="vertical",
        legend.spacing.y = unit(-0.15, "cm"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 10)) + 
  ggtitle(label = "Legend")

legend <- get_legend(legend_plot)
(combined_plot <- grid.arrange(all_disease, legend, heights = c(1, .15)))

ggsave(combined_plot, filename = paste0(fig_dir, "4-plot-precipitation-binary.tiff"), 
       width = 12, height = 12)
       

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()
