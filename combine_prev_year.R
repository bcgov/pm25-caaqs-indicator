library(magrittr)
library(tidyverse)

old <- read_csv("https://catalogue.data.gov.bc.ca/dataset/699be99e-a9ba-403e-b0fe-3d13f84f45ab/resource/bfa3fdd8-2950-4d3a-b190-52fb39a5ffd4/download/pm25sitesummary.csv")
new <- read_csv("out/pm25_site_summary_2016.csv")
stations <- read_csv("data/bc_air_monitoring_stations.csv")

setdiff(names(new), names(old))
setdiff(names(old), names(new))

old2 <- old %>% select(-regional_district, -monitor, -mgmt) %>% 
  rename(station_name = display_name) %>% 
  left_join(unique(select(stations, EMS_ID, CITY)), by = c("ems_id" = "EMS_ID")) %>% 
  rename_at(c("Latitude", "Longitude", "CITY"), .funs = tolower)

bind_rows(old2, new) %>% 
  select(ems_id, station_name, Airzone, city, everything()) %>% 
  write_csv("out/pm25_site_summary.csv")

