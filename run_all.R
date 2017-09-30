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

source("01_load.R")
source("02_clean.R")
source("03_analysis.R")
source("04_output.R")

mon_year <- format(Sys.Date(), "%B%Y")
outfile <- paste0("envreportbc_pm25_", mon_year, ".pdf")

rmarkdown::render("print_ver/pm25.Rmd", output_file = outfile)
extrafont::embed_fonts(file.path("print_ver/", outfile))
## You will likely want to 'optimize pdf' in Acrobat to make the print version smaller


###############################################################################################
## copy files to web dev folder
air_indicators_path <- "/Volumes/envwwwd/soe/indicators/air"
air_viz_data_path <- file.path(air_indicators_path, "pm25_viz/data/")
air_indicators_station_plots <- file.path(air_viz_data_path, "station_plots/")

web_data_files <- list.files("out", "*.geojson", full.names = TRUE)
web_viz_plots <- list.files("out/station_plots/", full.names = TRUE)

over_copy <- function(...) {
  file.copy(..., overwrite = TRUE)
}

## Copy print version
over_copy(file.path("print_ver", outfile), 
          file.path(air_indicators_path, "print_ver/"))

## Copy the management viz map and bar chart plots
over_copy("out/pm_caaqs_mgmt_map.svg", 
          file.path(air_indicators_path, "images/"))

over_copy("out/pm_caaqs_mgmt_chart.svg", 
          file.path(air_indicators_path, "images/"))

## Copy geojson files for viz
lapply(web_data_files, over_copy, to = air_viz_data_path)

## Copy dataviz plots
lapply(list.files(air_indicators_station_plots, full.names = TRUE), file.remove)
lapply(web_viz_plots, over_copy, to = air_indicators_station_plots)
