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
library("magrittr")
library("sp")
library("rgdal")
library("bcmaps")

if (!exists("pm25")) load("tmp/pm25-processed.RData")

## Compute average daily pm25 value for each site (used for both 24h and annual metric)
completeness <- pm_data_complete(pm25, by = c("ems_id", "site", "monitor", "instrument", "simple_monitor"))
avgdaily <- pm_daily_avg(pm25, by = c("ems_id", "site", "monitor", "instrument", "simple_monitor"))

# PM25 24 Hour ------------------------------------------------------------

## Compute annual 98th percentile of 24h averages for each site
ann_98_per <- pm_98_percentile(avgdaily, by = c("ems_id", "monitor", "instrument", "simple_monitor"))
ann_98_per <- left_join(ann_98_per, completeness, 
                        by = c("ems_id", "monitor", "instrument", "simple_monitor", "year"))
ann_98_per <- ann_98_per[ann_98_per$use_annual | (ann_98_per$exceed & ann_98_per$annual_valid), ]

pm_caaq_daily <- pm_24h_caaq(ann_98_per, by = c("ems_id", "monitor", "instrument", "simple_monitor"), 
                              cyear = 2014)

# PM25 Annual -------------------------------------------------------------

annual_avg <- pm_annual_average(avgdaily, by = c("ems_id", "monitor", "instrument", "simple_monitor"))
annual_avg <- left_join(annual_avg, completeness, 
                        by = c("ems_id", "monitor", "instrument", "simple_monitor", "year"))
annual_avg <- annual_avg[annual_avg$use_annual, ]

pm_caaq_annual <- pm_annual_caaq(annual_avg, by = c("ems_id", "monitor", "instrument", "simple_monitor"), 
                                 cyear = 2014)

## Join the annual and daily tables:
pm_stats <- bind_rows(pm_caaq_annual, pm_caaq_daily)

pm_stats <- stn_data %>% 
  select(ems_id, stationname, display_name, Longitude, Latitude) %>% 
  merge(pm_stats, by = "ems_id", all.x = FALSE, all.y = TRUE) %>% 
  filter(!grepl("Ucluelet|Rumble", stationname)) %>%  # remove Ucluelet and Rumble Beach (not well QA/QC'd and/or instrument issues until recently)
  filter(!(grepl("Whistler", display_name) & simple_monitor == "TEOM")) # Use the Whistler FEM monitor

## If there are more than one monitor for a station, select the one with most valid years
pm_stats %<>% group_by(ems_id, metric) %>% 
  slice(which.max(n_years)) %>% 
  as.data.frame(stringsAsFactors = FALSE)

## Do a spatial join to get airzones:
coordinates(pm_stats) <- ~Longitude + Latitude
proj4string(pm_stats) <- CRS("+init=epsg:4617")

## Load airzones map
data("airzone_map")
rd_map <- regional_districts_analysis
airzone_map <- spTransform(airzone_map, CRS(proj4string(pm_stats)))
rd_map <- spTransform(rd_map, CRS(proj4string(pm_stats)))

## Get airzone and rd info into pm_stats
pm_stats$Airzone <- over(pm_stats, airzone_map)[[1]]
pm_stats$regional_district <- over(pm_stats, rd_map)[[1]]

## stringsAsFactors argument only works if using sp >= 1.1-1
pm_stats %<>% as.data.frame(stringsAsFactors = FALSE)

## Determine achievement status for airzones:
airzones_annual <- airzone_metric(pm_stats[pm_stats$metric == "pm2.5_annual", ], 
                                  val = "metric_value", 
                                  n_years = "n_years", 
                                  keep = c(rep_stn_annual = "display_name", 
                                           rep_id_annual = "ems_id", 
                                           caaqs_annual = "caaqs", 
                                           n_years_annual = "n_years", 
                                           pm2.5_annual_metric = "metric_value"))

airzones_24h <- airzone_metric(pm_stats[pm_stats$metric == "pm2.5_24h", ], 
                               val = "metric_value", 
                               n_years = "n_years", 
                               keep = c(rep_stn_24h = "display_name", 
                                        rep_id_24h = "ems_id", 
                                        caaqs_24h = "caaqs",  
                                        n_years_24h = "n_years", 
                                        pm2.5_24h_metric = "metric_value"))

airzone_summary <- merge(airzones_annual, airzones_24h, by = "Airzone")

airzone_summary$caaq_mgmt <- pmax(cut_management(airzone_summary$pm2.5_annual_metric, 
                                                 "pm2.5_annual"), 
                                  cut_management(airzone_summary$pm2.5_24h_metric, 
                                                 "pm2.5_24h"))

airzone_map %<>% 
  merge(airzone_summary, by = "Airzone")

# Format pm_stats ----------------------------------------------
pm_stats %<>%
  select(ems_id, Airzone, regional_district, stationname, display_name, 
         Longitude, Latitude, simple_monitor:mgmt) %>%
  filter(!is.na(metric_value)) %>% 
  arrange(Airzone, display_name) %>%
  rename(monitor = simple_monitor)

save(list = ls(), file = "tmp/analysed.RData")
