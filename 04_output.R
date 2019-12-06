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

library("rcaaqs")
library("readr")
library("dplyr")
library("tidyr")
library("bcmaps")
library("ggplot2")
library("envreportutils")
library("sf")

if (!exists("az_final")) load("tmp/analysed.RData")

# Create output directory:
dir.create("out", showWarnings = FALSE)

rep_year <- 2018 # reporting year 

## @knitr pre

az <- st_intersection(airzones(), st_geometry(bc_bound())) %>% 
  group_by(airzone = Airzone) %>% 
  summarize()

stations_sf <- stations_clean %>% 
  select(ems_id, lat, lon) %>%
  semi_join(pm25_clean, by = "ems_id") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  transform_bc_albers()

az_mgmt_sf <- az %>%
  left_join(az_mgmt) %>% 
  mutate_at("mgmt_level", ~ replace_na(.x, "Insufficient Data"))

az_pm24h_sf <- az %>% 
  left_join(airzone_caaqs_pm24h) %>% 
  mutate_at("caaqs_ambient", ~ replace_na(.x, "Insufficient Data"))

az_pm_annual_sf <- az %>% 
  left_join(airzone_caaqs_pm_annual) %>% 
  mutate_at("caaqs_ambient", ~ replace_na(.x, "Insufficient Data"))

stations_caaqs_pm_24h_sf <- right_join(stations_sf, pm_24h_caaqs_results)
stations_caaqs_pm_annual_sf <- right_join(stations_sf, pm_annual_caaqs_results)

summary_pipe <- . %>% 
  summarise(n = length(ems_id), 
            n_achieved = length(.$ems_id[.$caaqs_ambient == "Achieved"]), 
            percent_achieved = round(n_achieved / n() * 100))

station_summary_annual <- summary_pipe(pm_annual_caaqs_results)
station_summary_24h <- summary_pipe(pm_24h_caaqs_results)

pm_caaqs_stations_all <- bind_rows(pm_annual_caaqs_results, pm_24h_caaqs_results)

## @knitr pm_ambient_summary_plot

pm_ambient_summary_plot <- summary_plot(
  pm_caaqs_stations_all, 
  metric_val = "metric_value_ambient", 
  airzone = "airzone", station = "station_name", 
  parameter = "metric", pt_size = 2, 
  az_labeller = label_wrap_gen(10)
) + 
  theme(strip.text.y = element_text(angle = 0))

## @knitr achievement_map_24h
achievement_map_24h <- ggplot() + 
  geom_sf(data = az_pm24h_sf, aes(fill = caaqs_ambient), colour = "white") + 
  scale_fill_manual(values = get_colours(type = "achievement", drop_na = FALSE), 
                    drop = FALSE, 
                    guide = guide_legend(order = 1, title.position = "top")) + 
  geom_sf(data = stations_caaqs_pm_24h_sf, aes(colour = metric_value_ambient), 
          size = 3) + 
  scale_colour_gradient(high = "#252525", low = "#f0f0f0", 
                        guide = guide_colourbar(order = 2,
                                                title.position = "top",
                                                barwidth = 10)) + 
  coord_sf(datum = NA) +
  labs(colour = "Monitoring Stations:\nPM2.5 24h Metric (ug/m3)",
       fill = "Airzones:\nPM2.5 24h Air Quality Standard") + 
  theme_minimal() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "bottom",
        legend.box.just = "left")

## @knitr achievement_map_annual
achievement_map_annual <- ggplot() + 
  geom_sf(data = az_pm_annual_sf, aes(fill = caaqs_ambient), colour = "white") + 
  scale_fill_manual(values = get_colours(type = "achievement", drop_na = FALSE), 
                    drop = FALSE, 
                    guide = guide_legend(order = 1, title.position = "top")) + 
  geom_sf(data = stations_caaqs_pm_annual_sf, aes(colour = metric_value_ambient), 
          size = 3) + 
  scale_colour_gradient(high = "#252525", low = "#f0f0f0", 
                        guide = guide_colourbar(order = 2,
                                                title.position = "top",
                                                barwidth = 10)) + 
  coord_sf(datum = NA) +
  labs(colour = "Monitoring Stations:\nPM2.5 Annual Metric (ug/m3)",
       fill = "Airzones:\nPM2.5 Annual Air Quality Standard") + 
  theme_minimal() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "bottom",
        legend.box.just = "left")

# Individual Station Plots ------------------------------------------------

## @knitr stn_plots

emsids <- unique(pm_24h_caaqs_results$ems_id)
stn_plots <- vector("list", length(emsids))
names(stn_plots) <- emsids

for (emsid in emsids) {
  
  # Create plots
  p_24 <- plot_ts(pm25_caaqs_24h, id = emsid, id_col = "ems_id", 
                  rep_yr = max_year, plot_caaqs = TRUE, plot_exceedances = FALSE, 
                  base_size = 14)
  
  p_annual <- plot_ts(pm25_caaqs_annual, id = emsid, id_col = "ems_id", 
                      rep_yr = max_year, plot_caaqs = TRUE, plot_exceedances = FALSE, 
                      base_size = 14)
  
  p_annual <- p_annual + coord_cartesian(ylim = c(0, 80))
  p_24 <- p_24 + coord_cartesian(ylim = c(0, 100))
  
  
  stn_plots[[emsid]]$daily <- p_24
  stn_plots[[emsid]]$annual <- p_annual
  message("creating plots for ", emsid, "\n")
}


# Management map and bar chart --------------------------------------------

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
pm_mgmt_chart <- ggplot(data = bind_rows(pm_annual_caaqs_results, pm_24h_caaqs_results),
                     aes(x = metric, fill = mgmt_level)) + 
  geom_bar(alpha = 1, width = 0.8) +
  facet_wrap(~airzone, ncol = 1) +
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

# Output data, maps, and charts ------------------------------------------------

st_transform(stations_caaqs_pm_24h_sf, 4326) %>% 
  st_write("out/pm_caaqs_24h.geojson", delete_dsn = TRUE)

st_transform(stations_caaqs_pm_annual_sf, 4326) %>% 
  st_write("out/pm_caaqs_annual.geojson", delete_dsn = TRUE)

st_transform(az_pm24h_sf, 4326) %>% 
  st_write("out/pm_airzone_24h.geojson", delete_dsn = TRUE)

st_transform(az_pm_annual_sf, 4326) %>% 
  st_write("out/pm_airzone_annual.geojson", delete_dsn = TRUE)

## SVG of airzone CAAQS mgmt level map
svg_px("out/pm_caaqs_mgmt_map.svg", width = 500, height = 500)
plot(pm_mgmt_map)
dev.off()

## SVG of airzone/station CAAQS mgmt achievement chart
svg_px("out/pm_caaqs_mgmt_chart.svg", width = 500, height = 500)
plot(pm_mgmt_chart)
dev.off()

pm_summary <- select(pm_24h_caaqs_results, ems_id, station_name, airzone, )

## Save line plots
line_dir <- "leaflet_map/station_plots/"
dir.create(line_dir, showWarnings = FALSE, recursive = TRUE)
lapply(list.files(line_dir, full.names = TRUE), file.remove)
width <- 778
height <- 254

for (i in seq_along(stn_plots)) {
  #i = 1
  emsid <- names(stn_plots[i])
  daily_plot <- stn_plots[[i]]$daily
  annual_plot <- stn_plots[[i]]$annual
  cat("saving plots for", emsid, "\n")
  # png_retina(filename = paste0(line_dir, emsid, "_24h_lineplot.png"), 
  #     width = 778, height = 254, units = "px", res = res)
  svg_px(paste0(line_dir, emsid, "_24h_lineplot.svg"), 
         width = width, height = height)
  plot(daily_plot)
  dev.off()
  # png_retina(filename = paste0(line_dir, emsid, "_annual_lineplot.png"), 
  #     width = 778, height = 254, units = "px", res = res)
  svg_px(paste0(line_dir, emsid, "_annual_lineplot.svg"),
         width = width, height = height)
  plot(annual_plot)
  dev.off()
}
graphics.off() # Kill any hanging graphics processes

## Save plot objects 
save(
  pm_ambient_summary_plot,
  pm_mgmt_map,
  pm_mgmt_chart,
  file = "tmp/plots.RData"
)
