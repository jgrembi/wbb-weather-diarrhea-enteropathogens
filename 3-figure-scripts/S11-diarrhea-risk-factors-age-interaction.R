#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# visualize age category interactions with all categorical risk factors 
#  plus the continuous variables that had significant interaction terms in model results
#######################################

rm(list=ls())

## configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))
library(patchwork)

## read table of all categorical variables corrected PR tables for age-interaction results
age_interaction_df_cat = readRDS(paste0(data_dir, "corrected_PR_tables_age_interaction_diarrhea.RDS")) %>%
  mutate(rf2_stratum = factor(ifelse(rf2_stratum == "<1.5yr", "6m-1.5yr", "1.5-5yr"), levels = c("6m-1.5yr", "1.5-5yr")),
         RF_cat = case_when(
           grepl("rain", RF) | grepl("ppt", RF) ~ "Precipitation",
           grepl("vpd", RF) ~ "Humidity", 
           grepl("water", RF) ~ "Surface Water"),
         # term_cat = gsub(":age_cat1.5-5yr", "", term_name),
         label2 = case_when(
           grepl('seasonal', RF) ~ 'Seasonal Surface Water',
           grepl('ephemeral', RF) ~ 'Ephemeral Surface Water',
           grepl('any', RF) ~ 'Any Surface Water'),
         category = factor(case_when(
           grepl("medium", term_name) ~ "Middle tertile vs. Farthest tertile", 
           grepl("close", term_name) ~ "Nearest tertile vs. Farthest tertile",
           grepl("prop", RF) ~ "Above vs. Below median"), 
           levels = c("Above vs. Below median", "Nearest tertile vs. Farthest tertile", "Middle tertile vs. Farthest tertile")),
         RF_type = factor(case_when(
           grepl("Distance Tertile", label) ~ "Distance Tertile From Surface Water",
           grepl("Proportion", label) & grepl("250", label) ~ "Proportion of Area within 250m Radius of Household with Surface Water",
           grepl("Proportion", label) & grepl("500", label) ~ "Proportion of Area within 500m Radius of Household with Surface Water",
           grepl("Proportion", label) & grepl("750", label) ~ "Proportion of Area within 750m Radius of Household with Surface Water",
           grepl("Heavy Rain", label) & !grepl("90", label) ~ "Heavy Rain above 80th Percentile",
           grepl("Heavy Rain", label) & grepl("90", label) ~ "Heavy Rain above 90th Percentile",
           grepl("Weekly Sum of Precipitation above Median", label) ~ "Weekly Sum of Precipitation above Median",
           grepl("Weekly Sum of Precipitation above 75", label) ~ "Weekly Sum of Precipitation above 75th Percentile",
           grepl("Weekly Sum of Precipitation above 90", label) ~ "Weekly Sum of Precipitation above 90th Percentile"),
         levels = c("Heavy Rain above 80th Percentile", "Heavy Rain above 90th Percentile", "Weekly Sum of Precipitation above Median",
                    "Weekly Sum of Precipitation above 75th Percentile","Weekly Sum of Precipitation above 90th Percentile",
                    "Proportion of Area within 250m Radius of Household with Surface Water",
                    "Proportion of Area within 500m Radius of Household with Surface Water",
                    "Proportion of Area within 750m Radius of Household with Surface Water",
                    "Distance Tertile From Surface Water")),
         lag = case_when(
           grepl("1week", RF) ~ "1-Week Lag",
           grepl("2week", RF) ~ "2-Week Lag",
           grepl("3week", RF) ~ "3-Week Lag"))

## Plot ppt variables
(ppt <- ggplot(age_interaction_df_cat %>% filter(RF_cat == "Precipitation"), 
               aes(y = RF_type, x = PR, color = rf2_stratum, group = rf2_stratum)) + 
    geom_pointrange(aes(xmin = lb, xmax=ub), position = position_dodge(0.5), shape = 18, size = 1) + 
    geom_vline(xintercept=1) +
    scale_color_manual(values = c("darkorange", "blue")) + 
    scale_x_continuous(trans="log", limits = c(0.2, 3), breaks = c(0.3, 0.5, 1, 1.5, 2), 
                       labels=c(0.3, 0.5, 1, 1.5, 2)) +
    scale_y_discrete(labels = ~ str_wrap(as.character(.x), 30), limits = rev) + 
    labs(color = "Age category", x = "Prevalence Ratio (95% CI)") + 
    facet_wrap(~lag) + 
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 15),
          strip.text = element_text(size = 12),
          strip.background = element_blank()) +
    ggtitle("A) Precipitation"))

## Plot surface water
(sw <- ggplot(age_interaction_df_cat %>% filter(RF_cat == "Surface Water", 
                                                label2 %in% c("Seasonal Surface Water", "Ephemeral Surface Water", "Any Surface Water")), 
              aes(y = RF_type, x = PR, color = rf2_stratum, group = interaction(rf2_stratum, category))) + 
    geom_pointrange(aes(xmin = lb, xmax=ub, shape = category), size = 0.8, position = position_dodge(0.5)) + 
    geom_vline(xintercept=1) +
    scale_color_manual(values = c("darkorange", "blue"), guide = "none") + 
    scale_x_continuous(trans="log", limits = c(0.2, 3), breaks = c(0.3, 0.5, 1, 1.5, 2), 
                       labels=c(0.3, 0.5, 1, 1.5, 2)) +
    scale_y_discrete(labels = ~ str_wrap(as.character(.x), 30), limits = rev) + 
    labs(color = "Age category", shape = "Comparison", x = "Prevalence Ratio (95% CI)") +
    facet_wrap(~label2) + 
    theme_bw() + 
    guides(shape=guide_legend(nrow=1,
                              title.position = "left")) +
    theme(legend.position = c(-0.3, -0.25),
          legend.justification = "left",
          plot.margin = unit(c(1, 1, 2,1.8), "cm"),
          legend.margin=margin(0,0,0,10),
          legend.box.margin=margin(-10,10,0,10),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 15),
          strip.text = element_text(size = 12),
          strip.background = element_blank()) +
    ggtitle("B) Surface Water"))

legend <- get_legend(ppt)

# Define filepath to age-interaction model results
file_path = paste0(offset_results_path, "gam_outputs/diarrhea-adjusted-interaction-age-0/")

## Now we'll use the custom function plot the temperature variables with suspected interaction 
## Suspected based on a review of model output p-value results
set.seed(2345)
plot_temp_weekavg_2wk <- plot_gam_int_hist(file_path = file_path, file_name = "gam_diar7d_0_temp_weekavg_2weeklag_C_by_age_cat.RDS",
                                           model_subdir = "figures-interaction/",
                                           risk_factor1 = "temp_weekavg_2weeklag_C", 
                                           risk_factor1_label = "Weekly Average Temperature, 2 week lag", 
                                           risk_factor2 = "age_cat", risk_factor2_label = "Age category", 
                                           outcome = "diar7d", outcome_label = "Diarrhea",
                                           scale = 100, 
                                           histogram = FALSE)

(plot_temp_weekavg_2wk = plot_temp_weekavg_2wk + 
    labs(y = "Diarrhea Prevalence (%)", 
         color = "Age category", fill = "Age category", 
         x = "Weekly Average Temperature (C), 2-Week Lag",
         title = "C) Temperature") + 
    theme(axis.title.x = element_text(),
          plot.title = element_text(size = 15)) +
    scale_fill_manual(aesthetics = c("fill", "color"), values = c("darkorange", "blue")))


plot_temp_weekmax_2wk <- plot_gam_int_hist(file_path = file_path, file_name = "gam_diar7d_0_temp_weekmax_2weeklag_C_by_age_cat.RDS",
                                           model_subdir = "figures-interaction/",
                                           risk_factor1 = "temp_weekmax_2weeklag_C", 
                                           risk_factor1_label = "Weekly Maximum Temperature, 2 week lag", 
                                           risk_factor2 = "age_cat", risk_factor2_label = "Age category", 
                                           outcome = "diar7d", outcome_label = "Diarrhea",
                                           scale = 100, 
                                           histogram = FALSE)

(plot_temp_weekmax_2wk = plot_temp_weekmax_2wk + 
    labs(y = "Diarrhea Prevalence (%)", 
         color = "Age category", fill = "Age category", 
         x = "Weekly Maximum Temperature (C), 2-Week Lag") + 
    theme(axis.title.x = element_text(),
          plot.title = element_text(size = 15)) +
    scale_fill_manual(aesthetics = c("fill", "color"), values = c("darkorange", "blue")))


combined_plot <- ((ppt + theme(legend.position = "none")) + plot_spacer() +
  (plot_temp_weekavg_2wk + theme(legend.position = "none")) +
     sw + plot_spacer() +
     (plot_temp_weekmax_2wk + theme(legend.position = "none")) + 
     plot_layout(widths = c(2, 0.1, 1.2), ncol = 3)) 
   
(final_plot <- (combined_plot / legend) + plot_layout(heights = c(1.5, 0.05)))

ggsave(final_plot, file = paste0(fig_dir, "S11-diarrhea-rf-age-interaction.tiff"),
       height = 10, 
       width = 14)
