#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Script to plot Figure 1 from manuscript 
# Map of Bangladesh with seasonal diarrhea prevalence  
#######################################

rm(list=ls())

source(paste0(here::here(), "/0-config.R"))
library(bangladesh)
library(patchwork)
library(ggspatial)


p = readRDS(paste0(box_data_path, clean_diarr_merged_box)) 

p_gps = p %>% filter(!is.na(qgpslong)) %>% # drop missing ll
  filter(intervention == 0) %>% # filter to only control HHs
  mutate(date_trim = floor_date(as.Date(date), "month"),
         year = floor_date(as.Date(date), "year")) # convert date to datetime and change dates to first of each month

plotdf_season = p_gps %>%
  mutate(svy_year = factor(case_when(
    date < as.Date("2013-07-31", format = "%Y-%m-%d", origin="1970-01-01") ~ "May 2012- July 2013",
    (date > as.Date("2013-07-31", format = "%Y-%m-%d", origin="1970-01-01") & date < as.Date("2014-10-01", format = "%Y-%m-%d", origin="1970-01-01")) ~ "Sept 2013 - Sept 2014",
    date > as.Date(2014-10-01, format = "%Y-%m-%d", origin="1970-01-01") ~ "Nov 2014 - Mar 2016"
  ), levels = c("May 2012- July 2013", "Sept 2013 - Sept 2014", "Nov 2014 - Mar 2016")),
  season = factor(season, levels = c("dry", "rainy"), labels = c("Dry", "Rainy"))) %>%
  group_by(clusterid, season) %>%
  summarise(cluster_mean = mean(as.numeric(as.character(diar7d)), na.rm = T) * 100,
            mean_lat = mean(qgpslat),
            mean_long = mean(qgpslong), 
            n = n()) %>%
  mutate(cluster_mean_bin = cut(cluster_mean,
                                breaks = c(0, 10, 20, 50, 100),
                                right  = T,
                                include.lowest = T,
                                labels = c("0-10", "11-20", "21-50", "51-100"),
                                ordered_result = T)) 


table(plotdf_season$cluster_mean_bin, useNA="ifany")

##################################################################################
# Get GPS Coordinates of Bangladesh District and Division boundaries, and Dhaka city
##################################################################################
district_keep <- c("Mymensingh", "Gazipur", "Tangail", "Kishoreganj")
district <- get_map("district") %>%
  mutate(District = ifelse(District %in% district_keep, as.character(District), NA))
division_map <- get_map("division")
dhaka = data.frame(lat = 23.8103, long = 90.4125, label = "Dhaka")

##################################################################################
# Plot zoomed out map of Bangladesh with study area highlighted
##################################################################################
(BD_map <-
   ggplot(data = district) +
   geom_sf(fill = "grey95") +
   geom_sf(data = division_map, linewidth = .6, fill = NA) +
   ggtitle("A)") + 
   ggstar::geom_star(data = dhaka, mapping = aes(x = long, y = lat, group = NA), 
                     starshape = 1, 
                     size = 3,
                     color = "#2166AC", 
                     fill = "#2166AC") +
   geom_segment(aes(y = 23.7, yend = 23.7, x = 89.8, xend = 91.1), lineend = "square") + 
   geom_segment(aes(y = 25, yend = 25, x = 89.8, xend = 91.1),  lineend = "square") + 
   geom_segment(aes(y = 23.7, yend = 25, x = 89.8, xend = 89.8),  lineend = "square") + 
   geom_segment(aes(y = 23.7, yend = 25, x = 91.1, xend = 91.1),  lineend = "square") + 
   labs(x = "", y = "") +
   annotation_scale(height= unit(0.01, "cm"), pad_y = unit(0.8, "cm")) +
   theme_bw() + 
   theme(panel.border = element_blank(), 
         panel.grid.major = element_blank(),     
         panel.grid.minor = element_blank(), 
         plot.title = element_text(vjust = -11),
         axis.line = element_blank(),
         axis.text = element_blank(), 
         axis.ticks = element_blank(), 
         plot.margin = unit(c(0, 0, 0, 0), "cm")))
         
##################################################################################
# Plot diarrhea prevalence by season
##################################################################################
my.palette <- viridis(n=5, option = "B",
                      end = 0.9,
                      direction = -1)


(BD_map_zoom <-
    ggplot(data = district) +
    geom_sf(fill = "grey95") +
    geom_sf(data = division_map, linewidth = .7, fill = NA) +
    geom_sf_text(aes(label = District)) +
    xlim(89.87, 91.08) + ylim(23.78, 24.96) +
    ggstar::geom_star(data = dhaka, mapping = aes(x = long, y = lat, group = NA), 
                      starshape = 1, 
                      color = "#2166AC", 
                      fill = "#2166AC", 
                      size = 3) +
    labs(x = "", y = "", color = "Diarrhea prevalence (%)") +
    theme_bw() + 
    ggtitle("B)") + 
    theme(panel.grid.major = element_blank(),     
          panel.grid.minor = element_blank(), 
          plot.title = element_text(vjust = -8.9, hjust = -0.025),
          axis.line = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          strip.background =element_rect(fill=NA),
          strip.placement = NULL,
          legend.position = "top",
          legend.margin = margin(0, 0, -1, 0, "cm"),
          plot.margin = unit(c(0, 0, 0, -0.5), "cm")) + 
    geom_point(data = plotdf_season, 
               mapping = aes(x = mean_long, y = mean_lat, group = NULL, color = cluster_mean_bin),
               size = 1.3,
               alpha = 0.7,
               shape = 16) + 
    facet_wrap(~season, ncol = 2) + 
    scale_color_manual(values = my.palette) + 
    annotation_scale(height= unit(0.01, "cm")) +
    guides(color = guide_legend(override.aes = list(size=3))))


(all_maps = BD_map | BD_map_zoom)

ggsave(paste0(fig_dir, "1-diarrhea_map_seasons.tiff"), all_maps, height = 5, width = 10)

#--------------------------------------
# Capture session info
#--------------------------------------
sessionInfo()