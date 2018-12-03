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
library(bcmaps)

options("rcaaqs.timezone" = "Etc/GMT+8")

if (!exists("pm25_all")) load("tmp/pm25_raw.RData")

min_year <- 2015
max_year <- 2017

## Set stations to exclude from analyis (those in indsutrial settings):
excluded_stations <- stations$EMS_ID[grepl("industr", stations$STATION_ENVIRONMENT, ignore.case = TRUE)]

## Exclude Valemount due to too much missing data and Kitimat Smeltersite 
## which is industrial but not labelled as such in the stations metadata
excluded_stations <- c(excluded_stations, "E234293", "E290529")

## Format dates, extract 2014-2016, set variable names

pm25 <- pm25_all %>% 
  filter(!EMS_ID %in% excluded_stations) %>% 
  mutate(date_time = format_caaqs_dt(DATE_PST), 
         year = year(date_time)) %>% 
  filter(year >= min_year, year <= max_year) %>% 
  select(-DATE_PST) %>% 
  rename_all(tolower) %>% 
  rename(value = raw_value) %>% 
  mutate(value = clean_neg(value, type = "pm25"),) %>% 
  group_by(ems_id, station_name, instrument) %>% 
  do(., date_fill(., date_col = "date_time",
                  fill_cols = c("ems_id", "station_name", "instrument", "parameter"),
                  interval = "1 hour")) %>% 
  mutate(instrument_type = 
           case_when(grepl("TEOM", instrument) ~ "TEOM",
                     grepl("SHARP|BAM", instrument) ~ "FEM", 
                     is.na(instrument) ~ NA_character_,
                     TRUE ~ "Unknown"), 
         year = year(date_time)) %>% 
  ungroup() %>% 
  distinct()

## Plot deployments of different instruments at each station
plot_station_instruments(pm25)
plot_station_instruments(pm25, instrument = "instrument_type")

## Summarise the dates that different PM2.5 monitoring instrumnet types were 
## deployed at each station so we can get the most data
instrument_deployments <- mutate(pm25, date = as.Date(date_time)) %>% 
  select(ems_id, station_name, instrument_type, date) %>% 
  distinct() %>% 
  group_by(ems_id, station_name, instrument_type) %>% 
  summarise(min_date = min(date), 
            max_date = max(date),
            n_days = n()) %>%
  ungroup()

## Select the monitor at each station that hast the most days
max_deployment_by_station <- group_by(instrument_deployments, ems_id, station_name) %>% 
  summarise(which_instrument = instrument_type[which.max(n_days)])


# Combine two squamish stations (both FEM, one in 2015 and the other in 2016-17)
# for a complete record
squamish_ems_ids <- c("0310172", "E304570")
combo_squamish_id <- paste(squamish_ems_ids, collapse = "-")

squamish <- bind_rows(
  filter(pm25, instrument_type == "FEM", ems_id == squamish_ems_ids[1], 
         year == 2015), 
  filter(pm25, instrument_type == "FEM", ems_id == squamish_ems_ids[2], 
         year %in% 2016:2017) 
) %>% 
  mutate(ems_id = combo_squamish_id, 
         station_name = "Squamish")

## Now select the rest based on max deployments and combine with Squamish
pm25_clean <- pm25 %>% 
  inner_join(max_deployment_by_station, 
             by = c("ems_id", "station_name", 
                    "instrument_type" = "which_instrument")) %>% 
  filter(!ems_id %in% squamish_ems_ids) %>% 
  bind_rows(squamish)

## As a check, plot them - there should be only one monitor per station, 
## except for Kamloops Federal Building, where two types of FEMs were combined
plot_station_instruments(pm25_clean)
plot_station_instruments(pm25_clean, instrument = "instrument_type")

## Clean station data - lowercase column names, remove pseudo-duplicates, subset to those 
## stations analysed
## OLD == closed stns; 
## _60 == meteorological stns;
## Met == meteorological stns using Campbell loggers; 
## BAM == Beta Attenuation Monitoring for PM measurement.
select_pattern <- "_60$|Met$|OLD$|BAM$|Squamish Gov't Bldg"
stations_clean <- rename_all(stations, tolower) %>% 
  mutate(ems_id = case_when(ems_id %in% squamish_ems_ids ~ combo_squamish_id, 
                            TRUE ~ ems_id)) %>% 
  group_by(ems_id) %>%
  filter(n() == 1 | 
          !grepl(select_pattern, station_name) | 
           all(grepl(select_pattern, station_name))) %>% 
  mutate(station_name = gsub(select_pattern, "", station_name), 
         station_name = gsub("(Squamish).+", "\\1", station_name)) %>% 
  semi_join(pm25_clean, by = "ems_id") %>% 
  top_n(1, station_name)

stations_clean <- assign_airzone(stations_clean, airzones = airzones(), 
                                 station_id = "ems_id", 
                                 coords = c("longitude", "latitude"))

save(pm25_clean, stations_clean, file = "tmp/pm25_clean.rda")

