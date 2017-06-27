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
# library("magrittr")
# library("sp")
# library("rgdal")
library("sf")
library("bcmaps")
source("R/utils.R")

if (!exists("pm25_clean")) load("tmp/pm25_clean.rda")

site_group_vars <- c("ems_id", "station_name")

## Compute average daily pm25 value for each site (used for both 24h and annual metric)
avgdaily <- pm_daily_avg(pm25_clean, by = site_group_vars)

# PM25 24 Hour ------------------------------------------------------------

## Compute annual 98th percentile of 24h averages for each site
pm_98 <- pm_yearly_98(avgdaily, by = site_group_vars)

pm_98_just_valid <- pm_98[pm_98$exceed | pm_98$valid_year,]
pm_caaq_daily <- pm_24h_caaq(pm_98_just_valid, by = site_group_vars, cyear = 2016)

# PM25 Annual -------------------------------------------------------------

annual_avg <- pm_yearly_avg(avgdaily, by = site_group_vars)
annual_avg_just_valid <- annual_avg[annual_avg$valid_year,]
pm_caaq_annual <- pm_annual_caaq(annual_avg_just_valid, by = site_group_vars, cyear = 2016)

## Join the annual and daily tables and join to station data:
pm_stats <- bind_rows(pm_caaq_annual, pm_caaq_daily) %>% 
  left_join(stations_clean, by = "ems_id") %>% 
  select(-station_name.y) %>% 
  rename(station_name = station_name.x)



pm_stats <- st_as_sf(pm_stats, coords = c("longitude", "latitude"), crs = 4617, remove = FALSE)

## Load airzones map
airzone_map <- st_as_sf(bcmaps::airzones)
airzone_map <- st_transform(airzone_map, crs = st_crs(pm_stats))

## Get airzone into pm_stats
pm_stats$Airzone <- sf_over(pm_stats, airzone_map)$Airzone

## Determine achievement status for airzones:
airzones_annual <- st_set_geometry(pm_stats[pm_stats$metric == "pm2.5_annual", ], NULL) %>% 
  airzone_metric(val = "metric_value", 
                 n_years = "n_years", 
                 keep = c(rep_stn_annual = "station_name", 
                          rep_id_annual = "ems_id", 
                          caaqs_annual = "caaqs", 
                          n_years_annual = "n_years", 
                          pm2.5_annual_metric = "metric_value"))

airzones_24h <- st_set_geometry(pm_stats[pm_stats$metric == "pm2.5_24h", ], NULL) %>% 
  airzone_metric(val = "metric_value", 
                 n_years = "n_years", 
                 keep = c(rep_stn_24h = "station_name", 
                          rep_id_24h = "ems_id", 
                          caaqs_24h = "caaqs",  
                          n_years_24h = "n_years", 
                          pm2.5_24h_metric = "metric_value"))

airzone_summary <- merge(airzones_annual, airzones_24h, by = "Airzone")

airzone_summary$caaq_mgmt <- pmax(cut_management(airzone_summary$pm2.5_annual_metric, 
                                                 "pm2.5_annual"), 
                                  cut_management(airzone_summary$pm2.5_24h_metric, 
                                                 "pm2.5_24h"))

airzone_map <- merge(airzone_map, airzone_summary, by = "Airzone")

# Format pm_stats ----------------------------------------------
pm_stats <- select(pm_stats, ems_id, Airzone, city, longitude, latitude,
                   station_name, min_year, max_year, n_years, metric, metric_value, 
                   caaqs, mgmt) %>%
  filter(!is.na(metric_value)) %>% 
  arrange(Airzone, station_name) %>% 
  st_set_geometry(NULL)

save(list = ls(), file = "tmp/analysed.RData")
