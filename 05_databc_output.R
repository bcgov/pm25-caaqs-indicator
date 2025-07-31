# Copyright 2025 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and 
# limitations under the License.


# DataBC 

# Create output files for the BC data catalogue, combining with previous years' data

source("00_setup.R")

library("readr")
library("dplyr")
library("stringr")
library("tidyr")
library("purrr")

library("bcdata")
library('rcaaqs')
library("assertr")

# Join old and new ------------------------




# Testing -----------------------------------------------------------------

pm25_24h_mgmt <- read_rds("data/datasets/pm25_24h_mgmt.rds")
pm25_annual_mgmt <- read_rds("data/datasets/pm25_annual_mgmt.rds")

stations_clean <- read_rds("data/datasets/stations_clean.rds")

pm25_results <- bind_rows(get_caaqs(pm25_24h_mgmt),
                          get_caaqs(pm25_annual_mgmt)) %>%
  left_join(stations_clean, by = "site") %>% 
  ungroup() %>%
  distinct() %>%
  # Ensure only 1 analysis per site
  add_count(site, caaqs_year, metric, instrument_type) %>%
  assert(in_set(1), n) %>%
  # Clean up
  select(caaqs_year,
         airzone, 
         station_name = site, 
         region, 
         latitude = lat, 
         longitude = lon, 
         instrument_type,
         metric,	
         n_years,	
         min_year,	
         max_year,	
         metric_value_ambient,	
         caaqs_ambient,	
         excluded,	
         metric_value_mgmt,
         mgmt_level,
         everything(), -n, -flag_daily_incomplete, -flag_yearly_incomplete) %>% 
  arrange(airzone, caaqs_year)

write_csv(pm25_results, "out/databc/pm25_stations_summary.csv", na = "")

# Airzone results ---------------------------------------------------------
az_ambient_year <- pm25_results %>%
  nest(data = c(-metric, -caaqs_year)) %>%
  mutate(data = map(data, ~airzone_metric(., keep = "station_name", station_id = "station_name"))) %>%
  unnest(data) %>%
  select(airzone, metric, caaqs_year, everything())

az_mgmt_year <- az_ambient_year %>% 
  group_by(airzone, caaqs_year, metric) %>%   
  # Get which ever metric is worst (one per airzone)
  slice_max(mgmt_level, with_ties = FALSE) %>% 
  ungroup() %>%
  select(caaqs_year,	
         airzone, 
         metric,	
         n_years_ambient,
         metric_value_ambient,
         caaqs_ambient,
         rep_stn_name_ambient = rep_stn_id_ambient,	
         rep_stn_id_ambient,	
         excluded,	
         n_years_mgmt,
         metric_value_mgmt,
         mgmt_level,
         rep_stn_name_mgmt = rep_stn_id_mgmt,
         rep_stn_id_mgmt) %>%
  mutate(caaqs_ambient_no_tfees = map_int(mgmt_level, max),
         caaqs_ambient_no_tfees = case_when(
           caaqs_ambient_no_tfees == 5 ~ unique(achievement_levels$labels)[3],
           caaqs_ambient_no_tfees == 1 ~ unique(achievement_levels$labels)[1],
           TRUE ~ unique(achievement_levels$labels)[2]),
         caaqs_ambient_no_tfees = factor(
           caaqs_ambient_no_tfees, ordered = TRUE,
           levels = levels(caaqs_ambient))) %>%
  arrange(caaqs_year, airzone)

write_csv(az_mgmt_year, "out/databc/pm25_airzones_summary.csv", na = "")

# Old code - confirm with Jerome and delete section -----------------------

## Stations ----------------------------------------

# stations_old <- bcdc_get_data(
#   '699be99e-a9ba-403e-b0fe-3d13f84f45ab', 
#   resource = 'bfa3fdd8-2950-4d3a-b190-52fb39a5ffd4') %>%
#   rename(station_id = ems_id) %>%
#   select(-city)
# 
# stations_summary <- read_rds("data/datasets/pm25_results.rds") %>%
#   select(-flag_yearly_incomplete, -flag_two_of_three_years, 
#          -flag_daily_incomplete, -region) %>%
#   rename(station_name = site, latitude = lat, longitude = lon) %>%
#   mutate(caaqs_ambient = as.character(caaqs_ambient),
#          mgmt_level = as.character(mgmt_level),
#          station_id = station_name)
# 
# setdiff(names(stations_old), names(stations_summary))
# setdiff(names(stations_summary), names(stations_old))
# 
# bind_rows(stations_old, stations_summary) %>%
#   select(caaqs_year, airzone, station_name, station_id, 
#          latitude, longitude, instrument_type, metric, n_years, min_year, max_year, 
#          metric_value_ambient, caaqs_ambient,
#          excluded, metric_value_mgmt, mgmt_level) %>%
#   arrange(caaqs_year, airzone, station_name) %>% 
#   write_csv("out/databc/pm25_stations_summary.csv", na = "")
# 
# 
# ## Airzones ----------------------
# 
# az_ambient_old <- bcdc_get_data(
#   '699be99e-a9ba-403e-b0fe-3d13f84f45ab', 
#   resource = '5dd4fcf9-f7c6-49cf-90a0-0a5c9bc00334') %>%
#   
#   # I think that several columns should be combined: 
#   #  - n_years / n_years_ambient
#   #  - metric_value / metric_value_ambient
#   #  - caaqs / caaqs_ambient
#   #  - rep_stn_name / station_name_ambient
#   #  - rep_stn_id / rep_stn_id_ambient
#   
#   # - Update 2023: removed n_years_ambient from coalesce
#   
#   mutate(n_years_ambient = coalesce(n_years),
#          metric_value_ambient = coalesce(metric_value),
#          caaqs_ambient = coalesce(caaqs),
#          rep_stn_name_ambient = coalesce(rep_stn_name),
#          rep_stn_id_ambient = coalesce(rep_stn_id)) %>%
#   select(airzone, caaqs_year, metric, 
#          n_years_ambient, metric_value_ambient, caaqs_ambient, 
#          rep_stn_id_ambient, rep_stn_name_ambient)
# 
# az_mgmt_old <- bcdc_get_data('699be99e-a9ba-403e-b0fe-3d13f84f45ab', 
#                              resource = '700a7155-0b68-4e5a-bbe0-11d4b844ec57') %>%
#   # rename("metric" = "rep_metric") %>%
#   rename_with(~paste0(., "_mgmt"),
#               .cols = -c("airzone", "caaqs_year", "mgmt_level", "metric")) %>%
#   mutate(n_years_mgmt = NA, excluded = NA)
# 
# 
# airzones_old <- full_join(az_ambient_old, az_mgmt_old, 
#                           by = c("airzone", "caaqs_year", "metric"))
# 
# airzones_summary <- read_rds("data/datasets/az_ambient.rds") %>%
#   mutate(rep_stn_name_ambient = rep_stn_id_ambient,
#          rep_stn_name_mgmt = rep_stn_id_mgmt, 
#          caaqs_ambient = as.character(caaqs_ambient), 
#          mgmt_level = as.character(mgmt_level),
#          caaqs_year = .env$rep_year)
# 
# # Check for additional or missing columns
# setdiff(names(airzones_old), names(airzones_summary))
# setdiff(names(airzones_summary), names(airzones_old))
# 
# bind_rows(airzones_old, airzones_summary) %>%
#   select(caaqs_year, airzone, metric, 
#          n_years_ambient, metric_value_ambient, caaqs_ambient, 
#          rep_stn_name_ambient, rep_stn_id_ambient,
#          excluded, n_years_mgmt, metric_value_mgmt, mgmt_level, 
#          rep_stn_name_mgmt, rep_stn_id_mgmt) %>%
#   arrange(caaqs_year, airzone) %>% 
#   write_csv("out/databc/pm25_airzones_summary.csv", na = "")
# 
