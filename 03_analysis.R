# Copyright 2015 Province of British Columbia
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

source("00_setup.R")

library("readr")
library("dplyr")
library("tidyr")
library("purrr")
library("lubridate")
library("assertr")

library("rcaaqs")

# Load Data ----------------------
pm25_clean <- read_rds("data/datasets/pm25_clean.rds")
stations_clean <- read_rds("data/datasets/stations_clean.rds")

# Transboundary Flows and Exceptional Events ------------------------------
tfee <- pm25_clean %>%
  filter(flag_tfee) %>%
  # Ceiling TFEE to capture original dates
  mutate(date = ceiling_date(date_time, unit = "hour"),
         date = as_date(date)) %>%
  select(site, instrument_type, date) %>%
  distinct()

# Calculate CAAQs --------------------------------------

# PM25 24 Hour
pm25_24h_caaqs <- pm_24h_caaqs(pm25_clean, by = c("site", "instrument_type"))
pm25_24h_mgmt <- caaqs_management(pm25_24h_caaqs, 
                                  exclude_df = tfee, 
                                  exclude_df_dt = "date")

# PM25 Annual
pm25_annual_caaqs <- pm_annual_caaqs(pm25_clean, by = c("site", "instrument_type"))
pm25_annual_mgmt <- caaqs_management(pm25_annual_caaqs, 
                                     exclude_df = tfee, 
                                     exclude_df_dt = "date")

# Station results -----------------------------------------
# Combine and filter
pm25_results <- bind_rows(get_caaqs(pm25_24h_mgmt),
                          get_caaqs(pm25_annual_mgmt)) %>%
  filter(caaqs_year == .env$rep_year, n_years > 1) %>% 
  left_join(stations_clean, by = "site") %>% 
  # Ensure only 1 analysis per site
  add_count(site, metric) %>%
  assert(in_set(1), n) %>%
  # Clean up
  select(airzone, site, region, lat, lon, everything(), -n)

# Stations with TFEEs -----------------------------------------------------
# Almost certainly the same as unique(tfee$site), 
# but just in case check against data...

# Get breaks
pm25_24h_lvl <- achievement_levels %>%
  filter(parameter == "pm2.5_24h", lower_breaks > 0) %>%
  pull(lower_breaks) 

tfee_sites <- get_daily(pm25_24h_caaqs) %>% 
  filter(avg_24h >= pm25_24h_lvl) %>% 
  semi_join(tfee, by = c("site", "date")) %>%
  pull(site) %>%
  unique()

# Majority occurred in... 2018 and 2020 (considering 2018-2020)
tfee %>% 
  pull(date) %>% 
  year() %>% 
  table()


# Airzone results ---------------------------------------------------------
# Get airzone results by metric
az_ambient <- pm25_results %>%
  nest(data = c(-metric)) %>%
  mutate(data = map(data, ~airzone_metric(., keep = "site", station_id = "site"))) %>%
  unnest(data) %>%
  select(airzone, metric, everything())

az_mgmt <- az_ambient %>% 
  group_by(airzone, metric) %>%    # Groupby airzone AND metric? (orig no metric)
  slice(which.max(mgmt_level)) %>% 
  mutate(caaqs_year = .env$rep_year) %>% 
  ungroup() %>%
  select(caaqs_year, airzone, mgmt_level, rep_metric = metric, 
         metric_value = metric_value_mgmt, 
         rep_stn_id = rep_stn_id_mgmt)

write_rds(pm25_results, "data/datasets/pm25_results.rds")
write_rds(az_ambient, "data/datasets/az_ambient.rds")
write_rds(az_mgmt, "data/datasets/az_mgmt.rds")
write_rds(pm25_24h_mgmt, "data/datasets/pm25_24h_mgmt.rds")
write_rds(pm25_annual_mgmt, "data/datasets/pm25_annual_mgmt.rds")
write_rds(tfee_sites, "data/datasets/print_tfee_sites.rds")

write_csv(pm25_results, "out/pm2.5_caaqs_combined_results.csv", na = "")
write_csv(az_ambient, "out/pm2.5_airzone_results.csv" , na = "")
write_csv(az_mgmt, "out/pm2.5_airzone_management_levels.csv", na = "")

