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

library("readr")

path <- "data"
csvfile <- "PM25_hourly.csv"
zipfile <- file.path(path, "PM25_hourly.zip")
dir.create(path, showWarnings = FALSE)

## Get data from DataBC: 
## http://catalogue.data.gov.bc.ca/dataset/air-quality-monitoring-verified-hourly-data-and-station-data
if (!file.exists(zipfile)) {
  download.file("http://pub.data.gov.bc.ca/datasets/77eeadf4-0c19-48bf-a47a-fa9eef01f409/PM25_hourly.zip", 
                destfile = zipfile)
}
pm25 <- read_csv(zipfile, col_types = "ccccccidc")

## Get station metadata:
stn_data_url <- "http://catalogue.data.gov.bc.ca/dataset/77eeadf4-0c19-48bf-a47a-fa9eef01f409/resource/b833311d-4126-4dad-b57e-c9ce2c1133c2/download/bcairmonitoringstations.csv"
stn_data_csv <- "monitoring_stations.csv"
download.file(stn_data_url, destfile = file.path(path, stn_data_csv))

stn_data <- read.csv(file.path(path, stn_data_csv), stringsAsFactors = FALSE)

dir.create("tmp", showWarnings = FALSE)
save(pm25, stn_data, file = "tmp/pm-raw.RData")
