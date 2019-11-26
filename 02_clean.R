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
library("readr")
library("bcmaps")
library("stringr")
library("tsibble")

options("rcaaqs.timezone" = "Etc/GMT+8")

if (!exists("pm25_all")) load("tmp/pm25_raw.RData")

stn_names <- read_csv("data/stn_names_reporting.csv") %>% 
  mutate(ems_id = str_pad(ems_id, 7, "left", "0")) %>% 
  rename(orig_stn_name = station_name)

max_year <- 2018

## Set stations to exclude from analyis (those in indsutrial settings):
excluded_stations <- stations$EMS_ID[grepl("industr", stations$STATION_ENVIRONMENT, ignore.case = TRUE)]

## Exclude Kitimat Smeltersite which is industrial but not labelled as such in 
## the stations metadata
excluded_stations <- unique(c(excluded_stations, "E290529"))

# Combine two squamish stations (both FEM, one in 2015 and the other in 2016-17)
# for a complete record
squamish_ems_ids <- c("0310172", "E304570")
combo_squamish_id <- paste(squamish_ems_ids, collapse = "-")

## Clean station data - lowercase column names, remove pseudo-duplicates, subset to those 
## stations analysed
## OLD == closed stns; 
## _60 == meteorological stns;
## Met == meteorological stns using Campbell loggers; 
## BAM == Beta Attenuation Monitoring for PM measurement.
select_pattern <- "_60$|Met$|OLD$|BAM$|Squamish Gov't Bldg"
stations_clean <- rename_all(stations, tolower) %>% 
  mutate(ems_id = case_when(ems_id %in% squamish_ems_ids ~ combo_squamish_id, 
                            TRUE ~ gsub("_.+$", "", ems_id))) %>% 
  group_by(ems_id) %>%
  filter(n() == 1 | 
           !grepl(select_pattern, station_name) | 
           all(grepl(select_pattern, station_name))) %>% 
  filter(!is.na(latitude), !is.na(longitude)) %>% 
  mutate(station_name = gsub(select_pattern, "", station_name), 
         station_name = gsub("(Squamish).+", "\\1", station_name)) %>% 
  # semi_join(pm25_clean, by = "ems_id") %>% 
  top_n(1, station_name) %>% 
  ungroup() %>% 
  left_join(select(stn_names, -airzone), by = "ems_id") %>% 
  mutate(station_name = case_when(is.na(reporting_name) ~ station_name,
                                  TRUE ~ reporting_name)) %>% 
  assign_airzone(airzones = airzones(), 
                 station_id = "ems_id", 
                 coords = c("longitude", "latitude"))

## Format dates, extract time period , set variable names.

pm25 <- pm25_all %>% 
  filter(!EMS_ID %in% excluded_stations) %>% 
  mutate(date_time = format_caaqs_dt(DATE_PST), 
         year = year(date_time)) %>% 
  filter(year <= max_year) %>% 
  select(-DATE_PST) %>% 
  rename_all(tolower) %>% 
  rename(value = raw_value) %>% 
  mutate(ems_id = gsub("_1$", "", ems_id), # remove _1 from ems_id (Smithers St Josephs)
         value = clean_neg(value, type = "pm25"),) %>% 
  group_by(ems_id, instrument) %>% 
  do(., date_fill(., date_col = "date_time",
                  fill_cols = c("ems_id", "instrument", "parameter"),
                  interval = "1 hour")) %>% 
  mutate(instrument_type = 
           case_when(grepl("TEOM", instrument) ~ "TEOM",
                     grepl("SHARP|BAM", instrument) ~ "FEM", 
                     is.na(instrument) ~ NA_character_,
                     TRUE ~ "Unknown"), 
         year = year(date_time)) %>% 
  ungroup() %>% 
  # Filter NAs out of Kamloops Fed building from two overlapping monitors
  filter(!(ems_id == "0605008" & 
             instrument == "BAM1020" & 
             date_time > as.POSIXct("2017/06/19 14:59:59")) & 
           !(ems_id == "0605008" & 
               instrument == "PM25 SHARP5030" & 
               date_time <= as.POSIXct("2017/06/19 14:59:59"))
  ) %>% 
  # Remove TEOM from Grand forks in 2017, as there was also FEM running at the 
  # same time, and combine TEOM and FEM for that station
  filter(!(ems_id == "E263701" & 
           instrument_type == "TEOM" & 
           date_time >= as.POSIXct("2016-12-31 23:59:59")
           )) %>% 
  mutate(
    instrument_type = ifelse(ems_id == "E263701", "TEOM (2017 FEM)", instrument_type)
  ) %>% 
  distinct()

## Plot deployments of different instruments at each station


plot_station_instruments(pm25)
plot_station_instruments(pm25, instrument = "instrument_type")

library(ggplot2)
# Temporary check outputs -------------------------------------------------
# follow up with checks on instrument type. 
st_nms <- unique(pm25$station_name)
st_nms <- st_nms[1:25]
pm25a <- pm25 %>%
  filter(station_name %in% st_nms)
p1_25 <- plot_station_instruments(pm25a)
p1_25
ggsave( "tmp/pm25_p1_25.jpg", plot = last_plot())

st_nms <- unique(pm25$station_name)
st_nms <- st_nms[26:50]
pm25a <- pm25 %>%
  filter(station_name %in% st_nms)
p26_50 <- plot_station_instruments(pm25a)
p26_50 
ggsave( "tmp/pm25_p26_50.jpg", plot = last_plot())

st_nms <- unique(pm25$station_name)
st_nms <- st_nms[51:75]
pm25a <- pm25 %>%
  filter(station_name %in% st_nms)
p51_75 <- plot_station_instruments(pm25a)
p51_75
ggsave( "tmp/pm25_p51_75.jpg", plot = last_plot())

st_nms <- unique(pm25$station_name)
st_nms <- st_nms[76:100]
pm25a <- pm25 %>%
  filter(station_name %in% st_nms)
p76_100 <- plot_station_instruments(pm25a)
p76_100
ggsave( "tmp/pm25_p76_100.jpg", plot = last_plot())


##------------------------------------------------------------------------

## Summarise the dates during the most recent three years (caaqs timeframe)
## that different PM2.5 monitoring instrument types were deployed at each 
## station so we can get the most data
instrument_deployments <- mutate(pm25, date = as.Date(date_time)) %>% 
  filter(between(year(date_time), max_year - 2L, max_year)) %>% 
  select(ems_id, instrument_type, date) %>% 
  distinct() %>% 
  group_by(ems_id, instrument_type) %>% 
  summarise(min_date = min(date), 
            max_date = max(date),
            n_days = n()) %>%
  ungroup()

## Select the monitor at each station that has the most days
max_deployment_by_station <- group_by(instrument_deployments, ems_id) %>% 
  summarise(which_instrument = instrument_type[which.max(n_days)])

squamish <- filter(
  pm25, instrument_type == "FEM", 
  (ems_id == squamish_ems_ids[1] & year == 2015) | 
    (ems_id == squamish_ems_ids[2] & year %in% 2016:2017)
) %>%
  # If BAM and SHARP were running in concert - take BAM
  group_by(date_time) %>%
  filter(if (n() == 2) grepl("BAM", instrument) else TRUE) %>% 
  ungroup() %>% 
  mutate(ems_id = combo_squamish_id)

## Now select the rest based on max deployments and combine with Squamish
pm25_clean <- pm25 %>% 
  inner_join(max_deployment_by_station, 
             by = c("ems_id", 
                    "instrument_type" = "which_instrument")) %>% 
  filter(!ems_id %in% squamish_ems_ids) %>% 
  bind_rows(squamish) %>% 
  select(-station_name) %>% 
  distinct() %>% #remove duplicate records if any
  inner_join(select(stations_clean, ems_id, station_name), 
             by = "ems_id") %>% 
  # tsibble time series package to make sure hourly data
  as_tsibble(key = c(ems_id, station_name, instrument, instrument_type), 
             regular = FALSE) %>% 
  group_by(ems_id, station_name, instrument, instrument_type) %>% 
  index_by(date_hr = ceiling_date(date_time, "hour") - 1) %>% 
  summarise(value = last(value)) %>% 
  as_tibble() %>% 
  rename(date_time = date_hr)

## As a check, plot them - there should be only one monitor per station, 
## except for Kamloops Federal Building, where two types of FEMs were combined
plot_station_instruments(pm25_clean)
plot_station_instruments(pm25_clean, instrument = "instrument_type")

save(pm25_clean, stations_clean, max_year, file = "tmp/pm25_clean.rda")

