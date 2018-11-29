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
library("lubridate")

if (!exists("pm25_clean")) load("tmp/pm25_clean.rda")

site_group_vars <- c("ems_id", "station_name", "instrument")

# PM25 Annual -------------------------------------------------------------
pm25_caaqs_annual <- pm_annual_caaqs(pm25_clean, by = site_group_vars)

# PM25 24 Hour ------------------------------------------------------------
pm25_caaqs_24h <- pm_24h_caaqs(pm25_clean, by = site_group_vars)

# Transboundary Flows and Exceptional Events ------------------------------

## All May-September daily concentrations >28 ug/m3 were  associated with wildfire 
## influences and were therefore excluded from the calculation of management levels

## Find the days that exceeded 28 ug/m3 and create a data frame of the grouping vars and dates
excludes <- filter(get_daily(pm25_caaqs_24h), avg_24h > 28, 
                   between(month(date), 5, 9)) %>% 
  select(site_group_vars, date)

# PM25 24 Hour - Management -----------------------------------------------

pm25_24h_caaqs_mgmt <- caaqs_management(pm25_caaqs_24h, exclude_df = excludes, 
                                    exclude_df_dt = "date")


# PM25 Annual - Management ------------------------------------------------

pm25_annual_caaqs_mgmt <- caaqs_management(pm25_caaqs_annual, exclude_df = excludes, 
                                           exclude_df_dt = "date")
# PM25 24 Hour - Airzone caaqs -----------------------------------------

pm_24h_caaqs_2017 <- get_caaqs(pm25_24h_caaqs_mgmt) %>% 
  group_by(ems_id) %>% 
  filter(caaqs_year == max(caaqs_year),
         n_years > 1) %>% 
  left_join(select(stations_clean, ems_id, airzone, lat, lon), 
            by = c("ems_id")) %>% 
  ungroup() %>% 
  airzone_metric()


# PM25 Annual - Airzone caaqs -----------------------------------------

pm_annual_caaqs_2017 <- get_caaqs(pm25_annual_caaqs_mgmt) %>% 
  group_by(ems_id) %>% 
  filter(caaqs_year == max(caaqs_year),
         n_years > 1) %>% 
  left_join(select(stations_clean, airzone, ems_id, lat, lon), 
            by = c("ems_id")) %>% 
  ungroup() %>% 
  airzone_metric()

save(list = ls(), file = "tmp/analysed.RData")
