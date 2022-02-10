# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

source("00_setup.R")

library("readr")
library("dplyr")
library("tidyr")

library("ggplot2")
library("patchwork")

library("sf")
library("bcmaps")

library("rcaaqs")
library("envreportutils")

# Load Data --------------------------------------------------
pm25_results <- read_rds("data/datasets/pm25_results.rds")
pm25_24h_mgmt <- read_rds("data/datasets/pm25_24h_mgmt.rds")
pm25_annual_mgmt <- read_rds("data/datasets/pm25_annual_mgmt.rds")

az_ambient <- read_rds("data/datasets/az_ambient.rds")
az_mgmt <- read_rds("data/datasets/az_mgmt.rds")


# Spatial data summaries --------------------------------------------
## @knitr pre
az <- airzones() %>%
  st_make_valid() %>%
  st_transform(st_crs(bc_bound())) %>%
  st_intersection(st_geometry(bc_bound())) %>% 
  group_by(airzone = Airzone) %>% 
  summarize()

az_mgmt_sf <- az %>%
  left_join(az_mgmt, by = "airzone") %>% 
  mutate(mgmt_level = replace_na(mgmt_level, "Insufficient Data"))

az_ambient_sf <- az %>% 
  left_join(az_ambient, by = "airzone") %>% 
  mutate(caaqs_ambient = replace_na(caaqs_ambient, "Insufficient Data"))

stations_sf <- pm25_results %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  transform_bc_albers()

station_summary <- stations_sf %>%
  group_by(metric) %>%
  summarise(n = n(), 
            n_achieved = sum(caaqs_ambient == "Achieved"), 
            percent_achieved = round(n_achieved / n * 100))

# Maps ----------------------------------------------------------------------
## @knitr pm_ambient_summary_plot
summary_plot(pm25_results, 
             metric_val = "metric_value_ambient", 
             airzone = "airzone", station = "site", 
             parameter = "metric", pt_size = 2, 
             az_labeller = label_wrap_gen(10)) + 
  theme(strip.text.y = element_text(angle = 0))


## @knitr achievement_map_24h
achivement_map(az_data = filter(az_ambient_sf, metric == "pm2.5_24h"),
               stn_data = filter(stations_sf, metric == "pm2.5_24h"),
               az_labs = "Airzones:\nPM2.5 24h Air Quality Standard",
               stn_labs = "Monitoring Stations:\nPM2.5 24h Metric (ug/m3)")

## @knitr achievement_map_annual
achivement_map(az_data = filter(az_ambient_sf, metric == "pm2.5_annual"),
               stn_data = filter(stations_sf, metric == "pm2.5_annual"),
               az_labs = "Airzones:\nPM2.5 Annual Air Quality Standard",
               stn_labs = "Monitoring Stations:\nPM2.5 Annual Metric (ug/m3)")

# Individual Station Plots ------------------------------------------------

# - Using management data because contains differences between the non-tfee data
#   (ann_98_percentiles) and tfee-adjusted data (ann_98_percentiles_mgmt)

## @knitr stn_plots
sites <- unique(pm25_results$site)

for(s in sites) {
  message("Creating plots for ", s, "\n")
  g1 <- plot_rolling(pm25_24h_mgmt, id = s, id_col = "site")
  g2 <- plot_rolling(pm25_annual_mgmt, id = s, id_col = "site")
  #g1 / g2 + plot_layout(guides = "collect")
  
  ggsave(paste0(loc, site, "_24.svg"), g1, 
         width = 778, height = 254, dpi = 72, units = "px")
  
  ggsave(paste0(loc, site, "_24.svg"), g2, 
         width = 778, height = 254, dpi = 72, units = "px")
}


## @knitr stn_plots_orig
# sites <- unique(pm25_results$site)
# stn_plots <- vector("list", length(sites)) %>%
#   setNames(., sites)

### Are these still desired for the print version?

# for (s in sites) {
#   
#   # Create plots
#   p_24 <- plot_ts(pm25_24h_caaqs, id = s, id_col = "site", 
#                   rep_yr = rep_year, plot_caaqs = TRUE, plot_exceedances = FALSE, 
#                   base_size = 14)
#   
#   p_annual <- plot_ts(pm25_caaqs_annual, id = s, id_col = "ems_id", 
#                       rep_yr = rep_year, plot_caaqs = TRUE, plot_exceedances = FALSE, 
#                       base_size = 14)
#   
#   p_annual <- p_annual + coord_cartesian(ylim = c(0, 80))
#   p_24 <- p_24 + coord_cartesian(ylim = c(0, 100))
#   
#   
#   stn_plots[[s]]$daily <- p_24
#   stn_plots[[s]]$annual <- p_annual
#   message("creating plots for ", s, "\n")
# }


# Management figures --------------------------------------------

## Management Air Zone Map
## @knitr pm_mgmt_map

colrs <- get_colours("management", drop_na = FALSE)

labels_df = data.frame(x = c(680000, 1150000, 780000, 1150000, 
                             1550000, 1150000, 1500000),
                       y = c(950000, 1550000, 1500000, 950000, 
                             600000, 325000, 410000), 
                       airzone_name = c("Coastal", "Northeast", "Northwest", 
                                        "Central\nInterior", "Southern\nInterior", 
                                        "Georgia Strait", "Lower Fraser Valley"))

pm_mgmt_map <- ggplot(az_mgmt_sf) +   
  geom_sf(aes(fill = mgmt_level), colour = "white") + 
  coord_sf(datum = NA) + 
  theme_minimal() + 
  scale_fill_manual(values = colrs, 
                    drop = FALSE, 
                    name = "Air Zone Management Levels", 
                    guide = guide_legend(reverse = TRUE)) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0),"mm")) +
  geom_text(data = labels_df, aes(x = x, y = y, label = airzone_name), 
            colour = "black", size = 6)

## Management Bar Chart
## @knitr pm_mgmt_chart

pm_mgmt_chart <- ggplot(data = pm25_results, aes(x = metric, fill = mgmt_level)) + 
  geom_bar(alpha = 1, width = 0.8) +
  facet_wrap(~ airzone, ncol = 1) +
  xlab("") + ylab("Number of Reporting Stations") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = colrs, 
                    drop = TRUE, 
                    name = "Air Zone Management Levels", 
                    guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = c("pm2.5_24h" = "24-hour", "pm2.5_annual" = "Annual")) +
  theme_soe_facet() +
  theme(panel.grid.major.y = (element_blank()),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.spacing = unit(5,"mm"),
        plot.margin = unit(c(10,0,1,0),"mm"),
        strip.text = element_text(size = 13))

## @knitr end

# Output data ------------------------------------------------

filter(stations_sf, metric == "pm2.5_24h") %>%
  st_transform(4326) %>% 
  st_write("out/pm_caaqs_24h.geojson", delete_dsn = TRUE)

filter(stations_sf, metric == "pm2.5_annual") %>%
  st_transform(4326) %>% 
  st_write("out/pm_caaqs_annual.geojson", delete_dsn = TRUE)

filter(az_ambient_sf, metric == "pm2.5_24h") %>%
  st_transform(4326) %>% 
  st_write("out/pm_airzone_24h.geojson", delete_dsn = TRUE)

filter(az_ambient_sf, metric == "pm2.5_annual") %>%
  st_transform(4326) %>% 
  st_write("out/pm_airzone_annual.geojson", delete_dsn = TRUE)

# SVG of airzone CAAQS mgmt level map
ggsave("out/pm_caaqs_mgmt_map.svg", plot = pm_mgmt_map, dpi = 72,
       width = 500, height = 500, units = "px", bg = "white")

# SVG of airzone/station CAAQS mgmt achievement chart
ggsave("out/pm_caaqs_mgmt_chart.svg", plot = pm_mgmt_chart, dpi = 72,
       width = 500, height = 500, units = "px", bg = "white")

