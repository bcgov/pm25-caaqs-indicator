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
tfee_dates <- pm25_clean %>%
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
                                  exclude_df = tfee_dates, 
                                  exclude_df_dt = "date")

# PM25 Annual
pm25_annual_caaqs <- pm_annual_caaqs(pm25_clean, by = c("site", "instrument_type"))
pm25_annual_mgmt <- caaqs_management(pm25_annual_caaqs, 
                                     exclude_df = tfee_dates, 
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

# Airzone results ---------------------------------------------------------
az_ambient <- pm25_results %>%
  nest(data = c(-metric)) %>%
  mutate(data = map(data, ~airzone_metric(., keep = "site", station_id = "site"))) %>%
  unnest(data) %>%
  select(airzone, metric, everything())

az_mgmt <- az_ambient %>% 
  group_by(airzone) %>%   
  # Get which ever metric is worst (one per airzone)
  slice_max(mgmt_level, with_ties = FALSE) %>% 
  mutate(caaqs_year = .env$rep_year) %>% 
  ungroup() %>%
  select(caaqs_year, airzone, mgmt_level, metric, 
         metric_value_mgmt,
         rep_stn_id = rep_stn_id_mgmt, n_years = n_years_mgmt, 
         caaqs_ambient) %>%
  # Mgmt level reflects the WORST station with TFEE adjustment, 
  # That should reflect a CAAQS Achievement (had there been no TFEEs)
  mutate(caaqs_ambient_no_tfees = map_int(mgmt_level, max),
         caaqs_ambient_no_tfees = case_when(
           caaqs_ambient_no_tfees == 5 ~ unique(achievement_levels$labels)[3],
           caaqs_ambient_no_tfees == 1 ~ unique(achievement_levels$labels)[1],
           TRUE ~ unique(achievement_levels$labels)[2]),
         caaqs_ambient_no_tfees = factor(
           caaqs_ambient_no_tfees, ordered = TRUE,
           levels = levels(caaqs_ambient)))

# For print version --------------------------------------------------------

# Get breaks
pm25_24h_lvl <- achievement_levels %>%
  filter(parameter == "pm2.5_24h", lower_breaks > 0) %>%
  pull(lower_breaks) 

# Get reporting period tfee numbers for print version
print_tfee <- get_daily(pm25_24h_caaqs) %>% 
  ungroup() %>%
  filter(avg_24h >= pm25_24h_lvl, year(date) >= rep_year - 2) %>% 
  semi_join(tfee_dates, by = c("site", "date")) %>%
  arrange(date) %>%
  group_by(year = year(date)) %>%
  mutate(n_year = n_distinct(date)) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = n_year, names_prefix = "n_tfee_days_") %>%
  summarize(n_sites = n_distinct(site),
            tfee_occurred_in_months = paste0(sort(unique(month(date))), collapse = ", "),
            across(starts_with("n_tfee_days_"), .fns = ~na.omit(unique(.))))

write_rds(pm25_results, "data/datasets/pm25_results.rds")
write_rds(az_ambient, "data/datasets/az_ambient.rds")
write_rds(az_mgmt, "data/datasets/az_mgmt.rds")
write_rds(pm25_24h_mgmt, "data/datasets/pm25_24h_mgmt.rds")
write_rds(pm25_annual_mgmt, "data/datasets/pm25_annual_mgmt.rds")
write_rds(print_tfee, "data/datasets/print_tfee.rds")

write_csv(pm25_results, "out/pm2.5_caaqs_combined_results.csv", na = "")
write_csv(az_ambient, "out/pm2.5_airzone_results.csv" , na = "")
write_csv(az_mgmt, "out/pm2.5_airzone_management_levels.csv", na = "")

