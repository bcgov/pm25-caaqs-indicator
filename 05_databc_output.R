# Copyright 2019 Province of British Columbia
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
## Create output files for databc, combining with previous years' data:
library(envreportutils)
library(sf)
library(readr)
library(dplyr)
library(bcmaps)
library(tidyr)

if (!exists("az_final")) load("tmp/analysed.RData")

az <- st_intersection(airzones(), st_geometry(bc_bound())) %>% 
  group_by(airzone = Airzone) %>% 
  summarize()

az_mgmt_sf <- az %>%
  left_join(az_mgmt) %>% 
  mutate_at("mgmt_level", ~ replace_na(.x, "Insufficient Data"))

dir.create("out/databc", showWarnings = FALSE)

# Station-level results
pm_2013 <- read_csv(soe_path("Operations ORCS/Indicators/air/fine_pm/2015/pm25_site_summary.csv")) %>% 
  select(-regional_district) %>% 
  rename_all(tolower) %>% 
  rename(station_name = display_name, instrument_type = monitor, 
         caaqs_year = caaq_year, metric_value_ambient = metric_value,
         caaqs_ambient = caaqs, mgmt_level = mgmt) %>% 
  left_join(select(stations_clean, ems_id, city))

pm_2016 <- read_csv(soe_path("Operations ORCS/Indicators/air/fine_pm/2017/pm25_site_summary.csv")) %>% 
  rename_all(tolower) %>% 
  mutate(caaqs_year = 2016L) %>% 
  rename(metric_value_ambient = metric_value, caaqs_ambient = caaqs)

station_caaqs_combined <- pm_caaqs_combined_results %>% 
  select(-starts_with("flag")) %>% 
  rename(latitude = lat, longitude = lon) %>% 
  bind_rows(pm_2016, pm_2013) %>% 
  arrange(caaqs_year) %>% 
  select(ems_id, station_name, city, longitude, latitude, instrument_type, 
         airzone, metric, caaqs_year, min_year, max_year, n_years, 
         metric_value_ambient, caaqs_ambient, excluded, metric_value_mgmt, 
         mgmt_level)

# Ambient airzone caaqs
az_ambient_2016 <- read_csv(soe_path("Operations ORCS/Indicators/air/fine_pm/2017/pm25_ambient_airzone_caaqs_summary.csv")) %>% 
  rename_all(tolower)

az_ambient_2016_tidy <- select(az_ambient_2016, airzone, contains("annual")) %>% 
  mutate(metric = "pm2.5_annual") %>% 
  rename_all(function(x) gsub("_annual", "", x)) %>% 
  bind_rows(select(az_ambient_2016, airzone, contains("24")) %>% 
              mutate(metric = "pm2.5_24h") %>% 
              rename_all(function(x) gsub("_24h", "", x))) %>% 
  mutate(caaqs_year = 2016L) %>% 
  select(airzone, metric, caaqs_year, metric_value = pm2.5_metric, 
         caaqs, rep_stn_id = rep_id, rep_stn_name = rep_stn, n_years)

az_ambient_combined <- az_ambient %>% 
  select(airzone, metric, contains("ambient")) %>% 
  rename_all(function(x) gsub("_ambient", "", x)) %>% 
  rename(rep_stn_name = station_name) %>% 
  # join to fill out airzones with missing values (e.g., NW)
  right_join(select(az_ambient_2016_tidy, airzone, metric)) %>% 
  mutate(caaqs_year = 2017L) %>% 
  bind_rows(az_ambient_2016_tidy) %>% 
  replace_na(list(caaqs = "Insufficient Data")) %>% 
  select(airzone, metric, caaqs_year, everything()) %>% 
  arrange(caaqs_year, airzone, metric)

# Airzone management levels
az_mgmt_2016 <- read_csv(soe_path("Operations ORCS/Indicators/air/fine_pm/2017/pm25_mgmt_airzone_caaqs_summary.csv")) %>% 
  rename_all(tolower) %>% 
  rename_all(function(x) gsub("^caaq_mgmt_", "", x)) %>% 
  rename(mgmt_level = caaq_mgmt, rep_metric = metric) %>% 
  mutate(caaqs_year = 2016L)

az_mgmt_combined <- st_set_geometry(az_mgmt_sf, NULL) %>% 
  mutate(caaqs_year = 2017L) %>% 
  bind_rows(az_mgmt_2016) %>% 
  arrange(caaqs_year) %>% 
  replace_na(list(mgmt_level = "Insufficient Data"))

write_csv(station_caaqs_combined, "out/databc/pm25sitesummary.csv", na = "")
write_csv(az_ambient_combined, "out/databc/pm25-airzone-caaqs.csv", na = "")
write_csv(az_mgmt_combined, "out/databc/pm25-airzone-management-levels.csv", na = "")

