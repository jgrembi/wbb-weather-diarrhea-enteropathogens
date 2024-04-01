#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Fig. 2 - Diarrhea prevalence, and risk-factors over time
# Raw data

# Note that scripts 1-2d in this directory must be run 
# to generate the data files used in this script
#######################################

rm(list=ls())

## configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

## load temperature weekly means for all days, not just those with samples
temp_alldays <- readRDS(paste0(here::here(), "/data/biweekly_plot_data/temp_mean_3wklag_time_for_plot.RDS")) %>% 
  mutate(N = NA, SD= NA, Robust.SE = NA, measure = "temp_weekavg_3weeklag") %>% 
  select(biweek = date, N, Mean = temp_weekavg_3weeklag, SD, Robust.SE, 
         Lower.95.CI = temp_weekavg_3weeklag_lb, Upper.95.CI = temp_weekavg_3weeklag_ub, measure)

## load ppt weekly sums for all days, not just those with samples
ppt_alldays <- readRDS(paste0(here::here(), "/data/biweekly_plot_data/ppt_total_2wklag_time_for_plot.RDS")) %>% 
  mutate(N = NA, SD= NA, Robust.SE = NA, measure = "ppt_week_sum_2weeklag") %>% 
  select(biweek = date, N, Mean = ppt_total_2weeklag, SD, Robust.SE, 
         Lower.95.CI = ppt_total_2weeklag_lb, Upper.95.CI = ppt_total_2weeklag_ub, measure)

## load vpd for all days, not just those with samples
vpd_alldays <- readRDS(paste0(here::here(), "/data/biweekly_plot_data/mean_vpd_time_for_plot.RDS")) %>% 
  mutate(N = NA, SD= NA, Robust.SE = NA, measure = "vpd") %>% 
  select(biweek = date, N, Mean = mean_vpd, SD, Robust.SE, 
         Lower.95.CI = mean_vpd_lb, Upper.95.CI = mean_vpd_ub, measure)

## load distance from surface water for all days, not just those with samples
dist_surface_water_alldays <- readRDS(paste0(here::here(), "/data/biweekly_plot_data/mean_distance_from_any_surface_water_time_for_plot.RDS")) %>% 
  mutate(N = NA, SD= NA, Robust.SE = NA, measure = "distance_from_any_surface_water") %>% 
  select(biweek = date, N, Mean = distance_from_any_surface_water, SD, Robust.SE, 
         Lower.95.CI = distance_from_any_surface_water_lb, Upper.95.CI = distance_from_any_surface_water_ub, measure)

## load prop any surface water for all days, not just those with samples
prop_any_surface_water_alldays <- readRDS(paste0(here::here(), "/data/biweekly_plot_data/mean_prop_detected_any_surface_water_250_time_for_plot.RDS")) %>% 
  mutate(N = NA, SD= NA, Robust.SE = NA, measure = "prop_detected_any_surface_water_250") %>% 
  select(biweek = date, N, Mean = prop_detected_any_surface_water_250, SD, Robust.SE, 
         Lower.95.CI = prop_detected_any_surface_water_250_lb, Upper.95.CI = prop_detected_any_surface_water_250_ub, measure)

## load prop seasonal surface water for all days, not just those with samples
prop_seasonal_surface_water_alldays <- readRDS(paste0(here::here(), "/data/biweekly_plot_data/mean_prop_detected_seasonal_surface_water_250_time_for_plot.RDS")) %>% 
  mutate(N = NA, SD= NA, Robust.SE = NA, measure = "prop_detected_seasonal_surface_water_250") %>% 
  select(biweek = date, N, Mean = prop_detected_seasonal_surface_water_250, SD, Robust.SE, 
         Lower.95.CI = prop_detected_seasonal_surface_water_250_lb, Upper.95.CI = prop_detected_seasonal_surface_water_250_ub, measure)

## load prop ephemeral surface water for all days, not just those with samples
prop_ephemeral_surface_water_alldays <- readRDS(paste0(here::here(), "/data/biweekly_plot_data/mean_prop_detected_ephemeral_surface_water_250_time_for_plot.RDS")) %>% 
  mutate(N = NA, SD= NA, Robust.SE = NA, measure = "prop_detected_ephemeral_surface_water_250") %>% 
  select(biweek = date, N, Mean = prop_detected_ephemeral_surface_water_250, SD, Robust.SE, 
         Lower.95.CI = prop_detected_ephemeral_surface_water_250_lb, Upper.95.CI = prop_detected_ephemeral_surface_water_250_ub, measure)

master_biweek <- readRDS(paste0(here::here(), "/data/biweekly_plot_data/master_biweek_dates.RDS"))

## Read in diarrhea data and subset to intervention arms only.
df_diar = readRDS(paste0(here::here(), "/data/biweekly_plot_data/d_diar_biweek.RDS"))

# Subset data.frame to diarrhea_1 (intervention arms pooled)
d_diar = df_diar %>%
  filter(intervention == 0) %>%
  mutate(diar7d = as.numeric(as.character(diar7d)),
         ym = floor_date(date, "month"),
         yw = floor_date(date, "week"),
         age_cat2 = factor(ifelse(aged < 365, "6mo-1yr", ifelse(aged >= 365 & aged < 730, "1-2yrs", ifelse(aged >= 730, "2+ yrs", "missing"))), 
                           levels = c("6mo-1yr", "1-2yrs", "2+ yrs")),
         personid = paste0(dataid, childid)) 

table(d_diar$age_cat2)
table(d_diar$age_cat)

length(unique(d_diar$personid))

var_names = c("diar7d", 
              "vpd", 
              "temp_weekavg_1weeklag",
              "temp_weekavg_2weeklag", 
              "temp_weekavg_3weeklag",
              "ppt_week_sum_0weeklag", 
              "ppt_week_sum_1weeklag",
              "ppt_week_sum_2weeklag", 
              "ppt_week_sum_3weeklag", 
              "prop_detected_any_surface_water_250",
              "prop_detected_seasonal_surface_water_250",
              "prop_detected_ephemeral_surface_water_250",
              "prop_detected_permanent_water_250",
              "distance_from_any_surface_water")

var_labs = c("7-day diarrhea prevalence (%)", 
             "Vapor pressure deficit", 
             "Average weekly temp (C) - 1 week lag",
             "Average weekly temp (C) - 2 week lag", 
             "Average weekly temp (C) - 3 week lag",
             "Total weekly precipitation - 0 week lag", 
             "Total weekly precipitation (mm) - 1 week lag",
             "Total weekly precipitation (mm) - 2 week lag", 
             "Total weekly precipitation (mm) - 3 week lag",
             "Any surface water detected within 250m (%)",
             "Seasonal surface water detected within 250m (%)",
             "Ephemeral surface water detected within 250m (%)",
             "Permanent water detected within 250m (%)",
             "Distance from any surface water")

names(var_labs) = var_names


# get bi weekly cluster means
cluster_level_mean_by_week = d_diar %>%
    group_by(biweek) %>%
    summarise(data.frame(washb_mean(.data[["diar7d"]], id = .data$clusterid, print = F))) %>%
    mutate(measure = "diar7d") %>%
  mutate(Mean = ifelse(measure == "diar7d", Mean*100, Mean),
         Upper.95.CI = ifelse(measure == "diar7d", Upper.95.CI*100, Upper.95.CI),
         Lower.95.CI = ifelse(measure == "diar7d", Lower.95.CI*100, Lower.95.CI)) %>%
  filter(biweek > "2012-07-21" & biweek < "2016-02-04") %>%
  # drop sparse rows
  filter(N>10) 

# merge against master bi-weekly dataframe 
master_biweek_plot_diar <- master_biweek %>% ungroup() %>% 
  filter(!biweek_date %in% unique(cluster_level_mean_by_week$biweek)) %>% 
  dplyr::select(biweek_date) %>% 
  rename(biweek = biweek_date) %>% 
  mutate(N=NA, Mean=NA, SD=NA, Robust.SE=NA,
         Lower.95.CI = NA, Upper.95.CI = NA, measure="diar7d")


cluster_level_mean_by_week <- bind_rows(cluster_level_mean_by_week,
                                        master_biweek_plot_diar,
                                        temp_alldays, 
                                        ppt_alldays,
                                        vpd_alldays,
                                        dist_surface_water_alldays,
                                        prop_any_surface_water_alldays,
                                        prop_seasonal_surface_water_alldays,
                                        prop_ephemeral_surface_water_alldays) %>%
  arrange(biweek)


plot_dat <-cluster_level_mean_by_week %>%
  filter(measure %in% c("diar7d", "ppt_week_sum_2weeklag","temp_weekavg_3weeklag",
                        "vpd",
                        # "prop_detected_seasonal_surface_water_500",
                        # "prop_detected_ephemeral_surface_water_500",
                        "distance_from_any_surface_water")) %>% 
  mutate(measure = factor(measure, levels = var_names)) 

## Create sequential, lettered plot labels (adjust based on measures filtered, above)
plot_labs = c("7-day diarrhea prevalence (%)",
              "Monthly vapor pressure deficit (kPA)",
             "Average weekly temp (C) - 3 week lag",
             "Total weekly precipitation (mm) - 2 week lag",
             "Distance from any surface water (m)")

plot_vars = c("diar7d",
              "vpd",
              "temp_weekavg_3weeklag",
              "ppt_week_sum_2weeklag",
              "distance_from_any_surface_water")

min_plot_dat <- min(plot_dat$biweek[!is.na(plot_dat$N)])
max_plot_dat <- max(plot_dat$biweek[!is.na(plot_dat$N)])

label_function <- function(var_list){
  letters <- LETTERS[1:length(var_list)]
  paren <- ") "
  labs <- vector()
  for (i in 1:length(var_list)){
    lab <- paste0(letters[i], paren, var_list[i])
    labs <- append(labs, lab, after = length(labs))
  }
  return(labs)
}

labs <- label_function(plot_labs)
names(labs) = plot_vars


## create dummy variable so n_plot_weekly can also have a facet label
plot_dat <- plot_dat %>% mutate(n_color = "black")
n_letter <- LETTERS[length(plot_vars) + 1] # n_plot will be last
n_lab <- paste0(n_letter, ") ", "Number of diarrhea observations per time period")
names(n_lab) = c("black")

## define rainy season
rainy_season_def = read.csv(paste0(here::here(),"/results/season_definitions.csv")) %>% 
  select(-X) %>%
  rename("year" = "season") %>%
  mutate(season_start = as.Date(start),
         season_end = as.Date(end))
## make plots
rf_plot_weekly = ggplot(plot_dat, aes(x = biweek, y = Mean, color = measure)) +
  geom_rect(data = rainy_season_def, inherit.aes = F, aes(xmin = season_start, xmax = season_end, ymin = -Inf, ymax = Inf), color = "grey85", alpha = 0.2) +
  geom_line() +
  geom_ribbon(aes(ymax = Upper.95.CI, ymin = Lower.95.CI, fill = measure), alpha = 0.5, color = NA) +
  scale_x_date(date_breaks = "month", date_labels = "%Y-%m-%d",
               limits = c(min_plot_dat, max_plot_dat)) +
  facet_wrap(~measure, scales = "free_y", ncol = 1, labeller = labeller(measure = labs)) +
  theme_light() + 
  labs(x = NULL, y = NULL) +
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0, l = 0.1, unit = "in"),
        strip.background = element_rect(fil = "white", color = "gray50"),
        strip.text.x = element_text(size = 8, color = "gray40"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 6),
        legend.position = "none")

n_plot_weekly = ggplot(plot_dat) + 
  geom_rect(data = rainy_season_def, inherit.aes = F, aes(xmin = season_start, xmax = season_end, ymin = -Inf, ymax = Inf), color = "grey85", alpha = 0.2) +
  geom_col(aes(x = biweek, y = N, fill = n_color)) + 
  scale_x_date(date_breaks = "month", date_labels = "%Y-%m-%d",
               limits = c(min_plot_dat, max_plot_dat)) +
  labs(y = NULL, x = NULL) + 
  scale_fill_manual(values = c("black")) +
  facet_wrap(~n_color, ncol = 1,
             labeller = labeller(n_color = n_lab)) +
  theme_light() + 
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "in"),
        strip.background = element_rect(fil = "white", color = "gray50"),
        strip.text.x = element_text(size = 8, color = "gray40"),
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle=-90, size = 6, vjust = 0.5),
        legend.position = "none")

plot <- plot_grid(rf_plot_weekly, n_plot_weekly, ncol = 1, rel_heights = c(3, 1), axis = "lr", align = "hv", labels = NULL, label_size = 1)

ggsave(plot, filename = paste0(fig_dir, "2-diarrhea-0-risk-factors-weekly.tiff"), height = 7, width = 5, units = "in")

