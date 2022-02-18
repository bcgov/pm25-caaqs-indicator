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

renv::restore()   # - When updating from GitHub etc. restore to packages in lockfile

library("magrittr")

# Setup ---------------------------------
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
dir.create("data/datasets", showWarnings = FALSE, recursive = TRUE)
dir.create("out", showWarnings = FALSE)
dir.create("leaflet_map/station_plots/", showWarnings = FALSE, recursive = TRUE)
dir.create("out/databc", showWarnings = FALSE)

rep_year <- 2020


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
          legend.box.just = "left")
}




