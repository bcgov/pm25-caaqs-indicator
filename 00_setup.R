# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

# Setup renv ---------------------------
# - for reproducibility

# renv::init()     # - Only needs be done to get things started, no longer necessary
# renv::update()   # - Update renv packages - best done at the start of an analysis update
# renv::snapshot() # - As needed to keep renv packages up-to-date
# test
renv::restore()   # - When updating from GitHub etc. restore to packages in lockfile

library("magrittr")

# Setup ---------------------------------
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
dir.create("data/datasets", showWarnings = FALSE, recursive = TRUE)
dir.create("out", showWarnings = FALSE)
dir.create("leaflet_map/station_plots/", showWarnings = FALSE, recursive = TRUE)
dir.create("out/databc", showWarnings = FALSE)

rep_year <- 2021


# Functions ----------------------------

# Achievement maps - used in 04_output.R
achievement_map <- function(az_data, stn_data, az_labs, stn_labs) {
  ggplot() + 
    geom_sf(data = az_data, aes(fill = caaqs_ambient), colour = "white") + 
    geom_sf(data = stn_data, aes(colour = metric_value_ambient), size = 3) + 
    scale_fill_manual(
      values = get_colours(type = "achievement", drop_na = FALSE), 
      drop = FALSE, guide = guide_legend(order = 1, title.position = "top")) + 
    scale_colour_gradient(
      high = "#252525", low = "#f0f0f0", 
      guide = guide_colourbar(order = 2, title.position = "top", barwidth = 10)) + 
    coord_sf(datum = NA) +
    labs(fill = az_labs,
         colour = stn_labs) +
    theme_minimal() + 
    theme(axis.title = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          panel.grid = element_blank(), 
          legend.position = "bottom",
          legend.box.just = "top",
          legend.title = element_markdown(lineheight = 1.25)) # requires pkg 'ggtext'
}


add_caaqs_historic <- function(g, metric) {
  hist_caaqs <- tribble(
    ~parameter,     ~labels,                                            ~lower_breaks, ~upper_breaks,
    "o3",           "Actions for Keeping Clean Areas Clean",            0,             50,
    "o3",           "Actions for Preventing Air Quality Deterioration", 50,            56,
    "o3",           "Actions for Preventing CAAQS Exceedance",          56,            63,
    "o3",           "Actions for Achieving Air Zone CAAQS",             63,            Inf,
    
    "pm2.5_annual", "Actions for Keeping Clean Areas Clean",            0,             4.0,
    "pm2.5_annual", "Actions for Preventing Air Quality Deterioration", 4.0,           6.4,
    "pm2.5_annual", "Actions for Preventing CAAQS Exceedance",          6.4,           10.0,
    "pm2.5_annual", "Actions for Achieving Air Zone CAAQS",             10.0,          Inf,
    
    "pm2.5_24h",    "Actions for Keeping Clean Areas Clean",            0,             10,
    "pm2.5_24h",    "Actions for Preventing Air Quality Deterioration", 10,            19,
    "pm2.5_24h",    "Actions for Preventing CAAQS Exceedance",          19,            28,
    "pm2.5_24h",    "Actions for Achieving Air Zone CAAQS",             28,            Inf) %>%
    filter(parameter == .env$metric, !is.na(lower_breaks)) %>%
    left_join(select(rcaaqs::management_levels, labels, colour) %>% distinct(), 
              by = "labels")
  
  #' #added to customized legend order and labels
  mgmt_breaks <- c(
    'Insufficient Data', 
    'Actions for Achieving Air Zone CAAQS' ,
    'Actions for Preventing CAAQS Exceedance',
    'Actions for Preventing Air Quality Deterioration',
    'Actions for Keeping Clean Areas Clean',
    "No Adjustment",  
    "TF/EE Adjusted"
  )
  mgmt_labels <- c(
    'Insufficient Data', 
    'Actions for Achieving Air Zone CAAQS' ,
    'Actions for Preventing CAAQS Exceedance',
    'Actions for Preventing Air Quality Deterioration',
    'Actions for Keeping Clean Areas Clean',
    "No Adjustment",  
    "TF/EE Adjusted"
  )
  mgmt_values <- c('Insufficient Data' = '#dbdbdb',
                   'Actions for Preventing Air Quality Deterioration' = '#FEE08B',
                   'Actions for Keeping Clean Areas Clean' = '#A6D96A',
                   'Actions for Preventing CAAQS Exceedance' = '#F46D43',
                   'Actions for Achieving Air Zone CAAQS' = '#A50026',
                   "No Adjustment" = "#b4acb3", 
                   "TF/EE Adjusted" = "#8f94a6")
  
  
  years <- select(g$data, caaqs_year) %>%
    distinct() %>%
    nrow() 
  
  current_caaqs <- rcaaqs::management_levels %>%
    filter(parameter == .env$metric, !is.na(lower_breaks))
  
  ylim <- max(g$data$raw, na.rm = TRUE) * 1.1
  if(ylim < (max(hist_caaqs$lower_breaks, na.rm = TRUE) * 1.1)) {
    g <- g + 
      ggplot2::scale_y_continuous(
        expand = c(0,0), limits = c(NA, max(hist_caaqs$lower_breaks, na.rm = TRUE) * 1.1),
        breaks = scales::breaks_extended(n = 7))
  }
  
  g <- g + geom_rect(
    data = hist_caaqs, xmin = -Inf, xmax = years - 0.5, 
    aes(ymin = lower_breaks, ymax = upper_breaks, fill = labels), 
    inherit.aes = FALSE, alpha = 0.55)
  
  g <- g + 
    geom_rect(data = current_caaqs, xmin = years - 0.5, xmax = Inf,
              aes(ymin = lower_breaks, ymax = upper_breaks, fill = labels), 
              inherit.aes = FALSE, alpha = 0.55)
  
  g$layers <- list(g$layers[[2]], g$layers[[3]], g$layers[[1]])
  
  line_df <- data.frame(y = c(max(hist_caaqs$lower_breaks, na.rm = TRUE),
                              max(hist_caaqs$lower_breaks, na.rm = TRUE),
                              max(current_caaqs$lower_breaks, na.rm = TRUE),
                              max(current_caaqs$lower_breaks, na.rm = TRUE)),
                        x = c(0, years-0.5, years-0.5, Inf))
  
  g +
    geom_line(data = line_df, aes(x = x, y = y, colour = "CAAQS Achievement"), 
              inherit.aes = FALSE, linetype = "dashed", size = 1) +
    scale_fill_manual(breaks = mgmt_breaks,
                      labels = mgmt_labels,
                      values = mgmt_values) +
    scale_colour_manual(values = last(hist_caaqs$colour))
}



