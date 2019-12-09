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
library("readr")

if (!exists("pm25_clean")) load("tmp/pm25_clean.rda")

site_group_vars <- c("ems_id", "station_name", "instrument_type")

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


# this needs to be fixed still : 
no.site.with.extreme.fire <- filter(get_daily(pm25_caaqs_24h), avg_24h > 28, 
                   between(month(date), 5, 9), 
                   between(year(date),2016,2018)) %>% 
  select(site_group_vars, date) %>%
  ungroup() %>%
  select(ems_id) %>%
  distinct %>%
  pull

#ggplot(no.site.with.extreme.fire , aes(x = ems_id, y = date)) + 
#  geom_bar(stat = "identity") + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# PM25 24 Hour - Management -----------------------------------------------

pm25_24h_caaqs_mgmt <- caaqs_management(pm25_caaqs_24h, exclude_df = excludes, 
                                        exclude_df_dt = "date")


# PM25 Annual - Management ------------------------------------------------

pm25_annual_caaqs_mgmt <- caaqs_management(pm25_caaqs_annual, 
                                           exclude_df = excludes, 
                                           exclude_df_dt = "date")

## Station results -----------------------------------------
station_results_pipeline <- . %>% 
  get_caaqs() %>% 
  group_by(ems_id) %>% 
  filter(caaqs_year == !!max_year, n_years > 1) %>% 
  ungroup() %>% 
  select(ems_id, everything()) %>% 
  select(-station_name) %>% 
  left_join(select(stations_clean, airzone, ems_id, station_name, city, lat, lon), 
            by = c("ems_id")) %>% 
  ungroup() %>% 
  select(airzone, ems_id, station_name, city, lat, lon, 
         everything())

# PM25 24 Hour

pm_24h_caaqs_results <- station_results_pipeline(pm25_24h_caaqs_mgmt)

# PM25 Annual

pm_annual_caaqs_results <- station_results_pipeline(pm25_annual_caaqs_mgmt)

# Combo reporting df of annual and 24h caaqs results
pm_caaqs_combined_results <- bind_rows(pm_annual_caaqs_results, pm_24h_caaqs_results)

# Airzone results ---------------------------------------------------------

airzone_caaqs_pm_annual <- filter(pm_caaqs_combined_results, metric == "pm2.5_annual") %>% 
  airzone_metric(keep = c("station_name")) %>% 
  mutate(metric = "pm2.5_annual")

airzone_caaqs_pm24h <- filter(pm_caaqs_combined_results, metric == "pm2.5_24h") %>% 
  airzone_metric(keep = "station_name") %>% 
  mutate(metric = "pm2.5_24h")

az_ambient <- bind_rows(airzone_caaqs_pm24h, airzone_caaqs_pm_annual) %>% 
  select(airzone, metric, everything())

az_mgmt <- az_ambient %>% 
  group_by(airzone) %>% 
  slice(which.max(mgmt_level)) %>% 
  mutate(caaqs_year = 2017L) %>% 
  select(caaqs_year, airzone, mgmt_level, rep_metric = metric, 
         metric_value = metric_value_mgmt, rep_stn_id = rep_stn_id_mgmt, 
         rep_stn_name = station_name_mgmt)


stations.with.exlusion <- pm_caaqs_combined_results %>%
  filter(ems_id %in% no.site.with.extreme.fire) %>%
  select(ems_id) %>%
  distinct()


no.stations.with.exclusion = length(stations.with.exlusion$ems_id)


save(list = ls(), file = "tmp/analysed.RData")

dir.create("out", showWarnings = FALSE)
write_csv(az_ambient, paste0("out/pm2.5_airzone_results_", max_year, ".csv"), na = "")
write_csv(pm_caaqs_combined_results, paste0("out/pm2.5_caaqs_combined_results_", max_year, ".csv"), na = "")
write_csv(az_mgmt, paste0("out/pm2.5_airzone_management_levels_", max_year, ".csv"), na = "")

