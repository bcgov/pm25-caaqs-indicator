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

pm25 <- mutate(pm25_all, date_time = format_caaqs_dt(DATE_PST)) %>% 
  filter(year(date_time) >= min_year, year(date_time) <= max_year) %>% 
  select(-DATE_PST) %>% 
  rename_all(tolower) %>% 
  rename(value = raw_value) %>% 
  mutate(value = clean_neg(value, type = "pm25"))

## Plot deployments of different instruments at each station
plot_station_instruments(pm25)

