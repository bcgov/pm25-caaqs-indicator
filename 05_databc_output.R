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
library(bcdata)


if (!exists("az_final")) load("tmp/analysed.RData")
dir.create("out/databc", showWarnings = FALSE)

# station results 

stations_old <- bcdc_get_data('699be99e-a9ba-403e-b0fe-3d13f84f45ab', 
                            resource = 'bfa3fdd8-2950-4d3a-b190-52fb39a5ffd4')
                                         
stations_summary <- pm_caaqs_stations_all %>%
  select(-c(flag_yearly_incomplete, flag_two_of_three_years,flag_daily_incomplete)) %>%
  rename(latitude = lat, longitude = lon) %>%
  mutate(caaqs_ambient = as.character(caaqs_ambient),
         mgmt_level = as.character(mgmt_level))

bind_rows(stations_old, stations_summary) %>%
  arrange(caaqs_year) %>% 
  write_csv("out/databc/pm25sitesummary.csv", na = "")


# fine particulate matter - ambient caaqs by airzone 

az_ambient_old <- bcdc_get_data('699be99e-a9ba-403e-b0fe-3d13f84f45ab', 
                            resource = '5dd4fcf9-f7c6-49cf-90a0-0a5c9bc00334')                                      

az_ambient <- az_ambient %>%
  select(c(airzone, metric, n_years_ambient, metric_value_ambient, 
         caaqs_ambient, rep_stn_id_ambient, station_name_ambient)) %>%
  mutate(caaqs_ambient = as.character(caaqs_ambient), 
         caaqs_year = max_year)

bind_rows(az_ambient_old, az_ambient) %>%
  arrange(caaqs_year) %>% 
  write_csv("out/databc/pm25-airzone-caaqs.csv", na = "")


# air management zone 

az_mgmt_old <- bcdc_get_data('699be99e-a9ba-403e-b0fe-3d13f84f45ab', 
                            resource = '700a7155-0b68-4e5a-bbe0-11d4b844ec57')

az_mgmt <- az_mgmt %>%
  mutate(mgmt_level = as.character(mgmt_level))

bind_rows(az_mgmt_old, az_mgmt) %>%
  arrange(caaqs_year)%>% 
  write_csv("out/databc/pm25-airzone-management-levels.csv.csv", na = "")

