#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens


# Script to plot temperature 
# Negative control outcome, control arm 
#######################################
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
source(paste0(here::here(), "/0-utils/1-plot-functions.R"))


# define results path in Box 
negc_results_dir <- paste0(offset_results_path, "gam_outputs/diarrhea-adjusted-0-negc/")

# define list of temperature variable names
# temp_vars <- list("temp_weekavg_1weeklag_C",
#                   "temp_weekavg_2weeklag_C",
#                   "temp_weekavg_3weeklag_C",
#                   "temp_monthavg_1weeklag_C",
#                   "temp_monthavg_2weeklag_C",
#                   "temp_monthavg_3weeklag_C")
temp_vars <- list("temp_weekmax_1weeklag_C", "temp_weekmax_2weeklag_C", "temp_weekmax_3weeklag_C",
                  "temp_weekmin_1weeklag_C", "temp_weekmin_2weeklag_C", "temp_weekmin_3weeklag_C",
                  "temp_weekavg_1weeklag_C", "temp_weekavg_2weeklag_C", "temp_weekavg_3weeklag_C")
                  # "temp_monthmax_1weeklag_C", "temp_monthmax_2weeklag_C", "temp_monthmax_3weeklag_C",
                  # "temp_monthmin_1weeklag_C", "temp_monthmin_2weeklag_C", "temp_monthmin_3weeklag_C",
                  # "temp_monthavg_1weeklag_C", "temp_monthavg_2weeklag_C", "temp_monthavg_3weeklag_C")

#------------------------------------------------------
## Preprocess data for plotting 
#------------------------------------------------------
plot_negc_list <- lapply(temp_vars, function(x) 
  prep_gam_plot(results_directory = negc_results_dir, 
                yname = "bruise7d_0_", 
                risk_factor = x)) 

plot_negc_data <- plot_negc_list %>% bind_rows() %>% 
  mutate(outcome = "Bruising")


# make plot -------------------------------------------
plot_negc_data <- plot_negc_data %>%
  mutate(lag = case_when(
    grepl("1weeklag_C", risk_factor) ~ "1-week lag",
    grepl("2weeklag_C", risk_factor) ~ "2-week lag",
    grepl("3weeklag_C", risk_factor) ~ "3-week lag"),
    period = factor(ifelse(grepl("month", risk_factor), "30 days", "7 days"), 
                    levels = c("30 days", "7 days")),
    measure = factor(case_when(
      grepl("min", risk_factor) ~ "Minimum",
      grepl("max", risk_factor) ~ "Maximum",
      grepl("avg", risk_factor) ~ "Mean",
      TRUE ~ "NA"
    ), levels = c("Minimum", "Mean", "Maximum"))) 


#------------------------------------------------------
## Make plot of weekly average temperature with different outcomes
#------------------------------------------------------
negc_plot <- ggplot(plot_negc_data,  
                    aes(x = risk_factor_value, y = fit)) + 
  geom_ribbon(aes(ymin = lwrS, ymax= uprS, fill=lag), alpha = 0.5) +
  scale_color_manual(aesthetics = c("color", "fill"), values = c("#bdd7e7","#6baed6","#084594")) + #c("#BEAED4")) + 
  scale_x_continuous(labels = c(15, 20, 25, 30),
                     breaks = c(15, 20, 25, 30)) +
  geom_line(aes(col=lag), linewidth=0.8)  +
  ylab("Prevalence (%)") +
  xlab("Temperature (C)") +
  facet_grid(lag~measure)+
  theme_bw() +
  theme(
    legend.position = "none",
    legend.key.size = unit(1/10, "in"),
    legend.key.height= unit(1/10, 'in'),
    legend.key.width= unit(1/10, 'in'),
    legend.title = element_text(size=8),
    legend.text = element_text(size=8),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    plot.title = element_text(size= 10),
  ) + ylim(1,10)

negc_plot 

ggsave(negc_plot, filename = paste0(fig_dir, "S12-plot-gam-temp-negc.tiff"),
       width = 8, height = 6)



plot_negc_data %>% 
  filter(risk_factor == "temp_weekavg_1weeklag_C" & 
           risk_factor_value >14.5 & 
           risk_factor_value <=15.4) %>% 
  summarise(prev = mean(fit, na.rm = T),
            lb = mean(lwrS,na.rm = T),
            ub = mean(uprS, na.rm = T))

plot_negc_data %>% 
  filter(risk_factor == "temp_weekavg_1weeklag_C" & 
           risk_factor_value >29.5 & 
           risk_factor_value <=30.4) %>% 
  summarise(prev = mean(fit, na.rm = T),
            lb = mean(lwrS,na.rm = T),
            ub = mean(uprS, na.rm = T))

