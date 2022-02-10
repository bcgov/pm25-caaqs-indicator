# Copyright 2015 Province of British Columbia
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
library("dplyr")
library("tidyr")
library("purrr")
library("forcats")
library("lubridate")
library("stringr")
library("ggplot2")
library("patchwork")

library("rcaaqs")
library("bcmaps")

library("janitor")
library("assertr")


options("rcaaqs.timezone" = "Etc/GMT+8")

# Load Data ---------------------------------
stations <- read_csv("data/raw/caaqs_stationlist.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  rename(lon = long)

pm25 <- read_rds("data/raw/pm25_caaqs.Rds") %>%
  as_tibble()

az <- airzones() %>%
  sf::st_make_valid()


# Clean Stations -------------------------------------------------------------

# - lowercase column names
# - remove pseudo-duplicates
# - subset to those stations analysed

stations_clean <- stations %>%

  # Look for problems
  assert(within_bounds(-90, 90), lat) %>%
  assert(within_bounds(-180, 180), lon) %>%
  assert(not_na, airzone) %>%

  # Check Airzones
  assign_airzone(airzones = az, 
                 station_id = "site", 
                 coords = c("lon", "lat"))
  
  # verify(airzone.x == airzone.y) #%>%
  
# Report non-matching airzones
stations_clean %>% 
  filter(airzone.x != airzone.y) %>%
  select(site, lat, lon, region, 
         airzone_stn = airzone.x, airzone_bcmaps = airzone.y)


# Use stations airzones for now, and only stations for pm25
stations_clean <- stations_clean %>%
  filter(pm25) %>%
  select(site, region, airzone = airzone.x, lat, lon)

# Check distances -------------------

# instrument_index <- pm25_clean %>%
#   select(station_name, instrument, instrument_type) %>%
#   distinct()

dist_mat <- stations_clean %>%
  select(site, lat, lon) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs  = 4326) %>%
  sf::st_distance(., .)

dist <- expand_grid(stn1 = stations_clean$site,
                    stn2 = stations_clean$site) %>%
  mutate(dist = as.numeric(c(dist_mat))) %>%
  filter(stn1 != stn2) %>%
  mutate(pair = map2_chr(stn1, stn2, ~paste(sort(c(.x, .y)), collapse = " vs.\n")),
         pair = fct_reorder(pair, dist)) %>%
  select("dist", "pair") %>%
  distinct() %>%
  separate(col = pair, into = c("stn1", "stn2"), sep = " vs.\n", remove = FALSE) %>%
  arrange(dist)
  
ggplot(data = filter(dist, dist < 2000), aes(x = pair, y = dist)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(end = 0.8) +
  geom_hline(yintercept = c(500, 1000, 1500), linetype = "dotted",
             colour = "grey20") +
  labs(x = "", y = "Distance (m)", 
       title = "Distance between stations", 
       subtitle = "Where distance < 2km")


# Fix Squamish stations (??)
# Original note: (both FEM, one in 2015 and the other in 2016-17)

# fix <- c("0310172", "E304570")
# filter(stations_clean, ems_id %in% c("0310172", "E304570"))
# filter(pm25, ems_id %in% c("0310172", "E304570")) %>%
#   group_by(ems_id) %>%
#   summarize(n = n(), min = min(date_pst, na.rm = TRUE), max = max(date_pst, na.rm = TRUE))
# 
# stations_clean <- stations_clean %>%
#   mutate(ems_id = if_else(ems_id %in% fix, paste(fix, collapse = "-"), ems_id))
# 
# filter(stations_clean, ems_id %in% fix)
# filter(stations_clean, ems_id %in% paste(fix, collapse = "-"))


# Clean pm25 -----------------------------------------------------------------

## Overall clean -------------
pm25_clean <- pm25 %>% 

  # Format dates, only keep dates in range
  mutate(date_time = format_caaqs_dt(date_time), 
         year = year(date_time)) %>% 
  filter(year <= rep_year) %>% 
  
  # Clean values
  mutate(value = clean_neg(value, type = "pm25")) %>% 
  
  # Fill dates
  nest(data = c(-site, -instrument)) %>%
  mutate(data = map(
    data, ~date_fill(., date_col = "date_time", interval = "1 hour"))) %>%
  
  # Categorize instrument types
  mutate(instrument_type = 
           case_when(str_detect(instrument, "TEOM") ~ "TEOM",
                     str_detect(instrument, "SHARP|BAM") ~ "FEM", 
                     is.na(instrument) ~ NA_character_,
                     TRUE ~ "Unknown")) %>% 
  assert(not_na, instrument_type) %>%
  
  # Clean up
  unnest(data)


## Overlapping --------------
# - Check for overlapping instruments
# - Check dates/patterns explicitly

overlaps_plot <- pm25_clean %>%
  group_by(site) %>%
  filter(n_distinct(instrument) > 1) %>%
  ungroup() %>%
  filter(!is.na(value))

g1 <- plot_station_instruments(overlaps_plot, station = "site") +
  geom_vline(xintercept = ymd(rep_year - 2, truncated = 2))
g2 <- plot_station_instruments(overlaps_plot, station = "site", 
                               instrument = "instrument_type") +
  geom_vline(xintercept = ymd(rep_year - 2, truncated = 2))

g <- g1 + g2 + 
  plot_annotation(title = "Sites with multiple instruments",
                  subtitle = "Exluding missing data") +
  plot_layout(guides = "collect")

ggsave(filename = "out/pm25_instrument_overlap.pdf",width = 14, height = 10)

# All recent years (2018 - 2020) have only one instrument


## Check timeseries -----------------------

pm25_clean %>%
  nest(ts = c(year, date_time, value)) %>%
  mutate(n = map_int(ts, nrow),
         n_expect = map_dbl(ts, ~as.numeric(difftime(max(.$date_time), 
                                                     min(.$date_time), 
                                                     units = "hours")))) %>%
  filter(n_expect != n - 1) 

# None!


# Last details -----------------------

# Only keep stations with data
stations_clean <- semi_join(stations_clean, pm25_clean, by = "site")

# Write data ------------------------------
write_rds(stations_clean, "data/datasets/stations_clean.rds")
write_rds(pm25_clean, "data/datasets/pm25_clean.rds", compress = "gz")
