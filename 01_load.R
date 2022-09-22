# Copyright 2017 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

source("00_setup.R")

library("readr")
library("bcmaps")
library("curl")

# Download data -------------------------------

# Catalogue Data 
# - Placeholder

# FTP URL
ftp <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/CAAQS/data/"

# See File list
curl(url = ftp) %>% read_lines()
curl(url = file.path(ftp, "!Readme.txt")) %>% read_lines()

# Get relevant files
file_pm25 <- file.path("data", "raw", "pm25_caaqs.Rds")
file_stn <- file.path("data", "raw", "caaqs_stationlist.csv")

# Download files
if(!file.exists(file_pm25)) curl_download(file.path(ftp, "pm25_caaqs.Rds"), 
                                          destfile = file_pm25, quiet = FALSE)
if(!file.exists(file_stn)) curl_download(file.path(ftp, "caaqs_stationlist.csv"), 
                                                   destfile = file_stn)

# Update cached version of airzones
airzones(ask = FALSE, force = TRUE) # Make sure up-to-date
