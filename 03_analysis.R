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
library("dplyr")
library("sf")
library("bcmaps")
library("lubridate")

source("R/utils.R")

if (!exists("pm25_clean")) load("tmp/pm25_clean.rda")

site_group_vars <- c("ems_id", "station_name")

## Compute average daily pm25 value for each site (used for both 24h and annual metric)
avgdaily <- pm_daily_avg(pm25_clean, by = site_group_vars) %>% 
  filter(valid_avg_24h | flag_avg_24hr_incomplete)

# PM25 24 Hour ------------------------------------------------------------

## Compute annual 98th percentile of 24h averages for each site
pm_98 <- pm_yearly_98(avgdaily, by = site_group_vars)

pm_98_just_valid <- pm_98[pm_98$exceed | pm_98$valid_year,]
pm_caaq_daily <- pm_24h_caaq(pm_98_just_valid, by = site_group_vars, cyear = 2016)

# PM25 Annual -------------------------------------------------------------

annual_avg <- pm_yearly_avg(avgdaily, by = site_group_vars)
annual_avg_just_valid <- annual_avg[annual_avg$valid_year,]
pm_caaq_annual <- pm_annual_caaq(annual_avg_just_valid, by = site_group_vars, cyear = 2016)


# Transboundary Flows and Exceptional Events ------------------------------

## All May-September daily concentrations >28 ug/m3 were  associated with wildfire 
## influences and were therefore excluded from the calculation of management levels

## Find the days that exceeded 28 ug/m3 and create a data frame of the grouping vars and dates
excludes <- filter(avgdaily, avg_24h > 28, between(month(date), 5, 9)) %>% 
  select(site_group_vars, date)

avgdaily_mgmt <- pm_daily_avg(pm25_clean, by = site_group_vars, 
                              exclude_df = excludes, exclude_df_dt = "date") %>% 
  filter(valid_avg_24h | flag_avg_24hr_incomplete)

# PM25 24 Hour - Management -----------------------------------------------

## Compute annual 98th percentile of 24h averages for each site
pm_98_mgmt <- pm_yearly_98(avgdaily_mgmt, by = site_group_vars, 
                           exclude_df = excludes, exclude_df_dt = "date")

pm_98_mgmt_just_valid <- pm_98_mgmt[pm_98_mgmt$exceed | pm_98_mgmt$valid_year,]
pm_caaq_daily_mgmt <- pm_24h_caaq(pm_98_mgmt_just_valid, by = site_group_vars, cyear = 2016)

# PM25 Annual - Management ------------------------------------------------

annual_avg_mgmt <- pm_yearly_avg(avgdaily_mgmt, by = site_group_vars, 
                                 exclude_df = excludes, exclude_df_dt = "date")
annual_avg_mgmt_just_valid <- annual_avg_mgmt[annual_avg_mgmt$valid_year,]
pm_caaq_annual_mgmt <- pm_annual_caaq(annual_avg_mgmt_just_valid, by = site_group_vars, cyear = 2016)


# Munging Results ---------------------------------------------------------

## Join the annual and daily tables and join to station data:
pm_stats <- bind_rows(pm_caaq_annual, pm_caaq_daily) %>% 
  left_join(stations_clean, by = "ems_id") %>% 
  select(-station_name.y, -mgmt) %>% 
  rename(station_name = station_name.x)

pm_stats <- st_as_sf(pm_stats, coords = c("longitude", "latitude"), crs = 4617, remove = FALSE)

## Load airzones map
airzone_map <- st_as_sf(bcmaps::airzones) %>% 
  st_transform(crs = st_crs(pm_stats))

## Get airzone into pm_stats
pm_stats$Airzone <- sf_over(pm_stats, airzone_map)$Airzone

## Determine ambient achievement status for airzones:
airzones_annual <- filter(pm_stats, metric == "pm2.5_annual") %>% 
  st_set_geometry(NULL) %>% 
  airzone_metric(val = "metric_value", 
                 n_years = "n_years", 
                 keep = c(rep_stn_annual = "station_name", 
                          rep_id_annual = "ems_id", 
                          caaqs_annual = "caaqs", 
                          n_years_annual = "n_years", 
                          pm2.5_annual_metric = "metric_value"))

airzones_24h <- filter(pm_stats, metric == "pm2.5_24h") %>% 
  st_set_geometry(NULL) %>% 
  airzone_metric(val = "metric_value", 
                 n_years = "n_years", 
                 keep = c(rep_stn_24h = "station_name", 
                          rep_id_24h = "ems_id", 
                          caaqs_24h = "caaqs",  
                          n_years_24h = "n_years", 
                          pm2.5_24h_metric = "metric_value"))

airzone_ambient_summary <- merge(airzones_annual, airzones_24h, by = "Airzone")

airzone_ambient_map <- left_join(airzone_map, airzone_ambient_summary, by = "Airzone")

# Format pm_stats ----------------------------------------------
pm_stats <- pm_stats %>% 
  select(ems_id, Airzone, city, longitude, latitude,
         station_name, min_year, max_year, n_years, metric, metric_value, 
         caaqs) %>%
  filter(!is.na(metric_value)) %>% 
  arrange(Airzone, station_name)

# Airzone management levels -----------------------------------------------

pm_mgmt_stats <- bind_rows(pm_caaq_annual_mgmt, pm_caaq_daily_mgmt) %>% 
  left_join(stations_clean, by = "ems_id") %>% 
  select(-station_name.y) %>% 
  rename(station_name = station_name.x) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4617, remove = FALSE)

pm_mgmt_stats$Airzone <- sf_over(pm_mgmt_stats, airzone_map)$Airzone

## Determine ambient achievement status for airzones:
airzones_annual_mgmt <- filter(pm_mgmt_stats, metric == "pm2.5_annual") %>% 
  st_set_geometry(NULL) %>% 
  airzone_metric(val = "metric_value", 
                 n_years = "n_years", 
                 keep = c(rep_stn_annual = "station_name", 
                          rep_id_annual = "ems_id", 
                          caaqs_annual = "caaqs", 
                          n_years_annual = "n_years", 
                          pm2.5_annual_metric = "metric_value"))

airzones_24h_mgmt <- filter(pm_mgmt_stats, metric == "pm2.5_24h") %>% 
  st_set_geometry(NULL) %>% 
  airzone_metric(val = "metric_value", 
                 n_years = "n_years", 
                 keep = c(rep_stn_24h = "station_name", 
                          rep_id_24h = "ems_id", 
                          caaqs_24h = "caaqs",  
                          n_years_24h = "n_years", 
                          pm2.5_24h_metric = "metric_value"))

airzone_mgmt_summary <- merge(airzones_annual_mgmt, airzones_24h_mgmt, by = "Airzone") %>% 
  mutate(mgmt_annual = cut_management(pm2.5_annual_metric, "pm2.5_annual"), 
         mgmt_24h = cut_management(pm2.5_24h_metric, "pm2.5_24h"), 
         caaq_mgmt = pmax(mgmt_annual, mgmt_24h), 
         caaq_mgmt_metric = ifelse(caaq_mgmt == mgmt_24h, "24h", "annual"),
         caaq_mgmt_metric_value = ifelse(caaq_mgmt_metric == "24h", pm2.5_24h_metric, pm2.5_annual_metric),
         caaq_mgmt_rep_stn_id = ifelse(caaq_mgmt_metric == "24h", rep_id_24h, rep_id_annual), 
         caaq_mgmt_rep_stn_name = ifelse(caaq_mgmt_metric == "24h", rep_stn_24h, rep_stn_annual)) %>% 
  select(-contains("annual"), -contains("24h"))

airzone_mgmt_map <- left_join(airzone_map, airzone_mgmt_summary, by = "Airzone")

# Format pm_mgmt_stats ----------------------------------------------
pm_mgmt_stats <- pm_mgmt_stats %>% 
  select(ems_id, Airzone, city, longitude, latitude,
         station_name, min_year, max_year, n_years, metric, metric_value, 
         caaqs, mgmt) %>%
  filter(!is.na(metric_value)) %>% 
  arrange(Airzone, station_name)

save(list = ls(), file = "tmp/analysed.RData")
