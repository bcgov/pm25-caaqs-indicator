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

library("dplyr")
library("lubridate")
library("rcaaqs")

options("rcaaqs.timezone" = "Etc/GMT+8")

if (!exists("pm25_all")) load("tmp/pm25_raw.RData")

min_year <- 2014
max_year <- 2016

## Format dates, extract 2011-2013, set variable names

pm25 <- mutate(pm25_all, 
               date_time = format_caaqs_dt(DATE_PST), 
               year = year(date_time)) %>% 
  filter(year >= min_year, year <= max_year) %>% 
  select(-DATE_PST) %>% 
  rename_all(tolower) %>% 
  rename(value = raw_value) %>% 
  mutate(value = clean_neg(value, type = "pm25"))

## Plot deployments of different instruments at each station
plot_station_instruments(pm25)

## Summarise the dates that different PM2.5 monitoring instrumnets were deployed 
## at each station so we can get the most data
instrument_deployments <- mutate(pm25, date = as.Date(date_time)) %>% 
  select(ems_id, station_name, instrument, date) %>% 
  distinct() %>% 
  group_by(ems_id, station_name, instrument) %>% 
  summarise(min_date = min(date), 
            max_date = max(date),
            n_days = n()) %>%
  ungroup

## Select the monitor at each station that hast the most days
max_deployment_by_station <- group_by(instrument_deployments, ems_id, station_name) %>% 
  summarise(which_instrument = instrument[which.max(n_days)])

## There are two stations for which we are using data from different instruments for 
## different years:
teom_fem_combos <- tribble(
  ~ems_id,    ~year, ~instrument, 
  "E249492",  2014,  "PM25_R&P_TEOM",
  "E249492",  2015,  "PM2.5 SHARP5030",
  "E249492",  2016,  "PM2.5 SHARP5030",
  "0500886",  2014,  "PM25_R&P_TEOM",
  "0500886",  2015,  "PM2.5 SHARP5030",
  "0500886",  2016,  "PM2.5 SHARP5030"
)

## Select the data for the two special cases above:
teom_fem_pm25 <- inner_join(pm25, teom_fem_combos, by = c("ems_id", "year", "instrument"))

## Now select the rest based on max deployments:
pm25_clean <- filter(pm25, !ems_id %in% unique(teom_fem_combos$ems_id)) %>% 
  inner_join(max_deployment_by_station, 
             by = c("ems_id", "station_name", "instrument" = "which_instrument")) %>% 
  bind_rows(teom_fem_pm25)

## As a check, plot them - there should be only one monitor per station, 
## except for theo two where they were combined (Kelowna College and Vernon Science Centre)
plot_station_instruments(pm25_clean)
