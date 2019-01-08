# Copyright 2017 Province of British Columbia
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

library(readr)
## Download the pm2.5 and station data from the BC Data Catalogue:
## pm2.5 data from http://catalogue.data.gov.bc.ca/dataset/air-quality-monitoring-raw-hourly-data-and-station-data
## station data from https://catalogue.data.gov.bc.ca/dataset/air-quality-monitoring-unverified-hourly-air-quality-and-meteorological-data
databc_pm25 <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2009-LatestVerified/PM25.csv"
databc_stations <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Air_Monitoring_Stations/bc_air_monitoring_stations.csv"
path <- "data"
pm25_file <- "PM25.csv"
stn_file <- "bc_air_monitoring_stations.csv"

dir.create(path, showWarnings = FALSE)

download.file(databc_pm25, destfile = file.path(path,pm25_file))
download.file(databc_stations, destfile = file.path(path, stn_file))

## Load stations and data from files
stations <- read_csv(file.path(path, stn_file), na = c("", "N/A"))
pm25_all <- read_csv(file.path(path, pm25_file), 
                     col_types = cols(DATE_PST = col_datetime(), 
                                      DATE = col_character(), 
                                      TIME = col_character(), 
                                      STATION_NAME = col_character(),
                                      STATION_NAME_FULL = col_character(),
                                      EMS_ID = col_character(), 
                                      NAPS_ID = col_character(),
                                      RAW_VALUE = col_double()))

## store data in local repository
dir.create("tmp", showWarnings = FALSE)
save(pm25_all, stations, file = "tmp/pm25_raw.RData")
