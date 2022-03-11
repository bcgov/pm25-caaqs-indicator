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
  st_make_valid() # fixes invalid geometry error in assign_airzones function below


# Clean Stations -------------------------------------------------------------

# - lowercase column names
# - subset to those stations analysed

stations_clean <- stations %>%

  # Look for problems
  assert(within_bounds(-90, 90), lat) %>%
  assert(within_bounds(-180, 180), lon) %>%

  # Use airzones from bcmaps
  select(-airzone) %>%
  assign_airzone(airzones = az, 
                 station_id = "site", 
                 coords = c("lon", "lat")) %>%
  assert(not_na, airzone) %>%
  
  # Only keep stations for pm25
  filter(pm25) %>%
  select(site, region, airzone, lat, lon)

# Check distances -------------------

# (only if curious)
if(FALSE) {
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
  ggsave("out/stations_distance.png", width = 12)
}


# Clean pm25 -----------------------------------------------------------------

## Overall clean -------------
pm25_clean <- pm25 %>% 

  # Format dates, only keep dates in range
  mutate(date_time = format_caaqs_dt(date_time), 
         year = year(date_time)) %>% 
  filter(year <= rep_year) %>% 
  
  # Clean negative values
  mutate(value = clean_neg(value, type = "pm25")) %>% 
  
  # Omit NAs at at start/end of a site/instrument range
  nest(data = -c(site, instrument)) %>%
  mutate(data = map(data, ~ mutate(., na_before = cumall(is.na(value)))),
         data = map(data, ~ arrange(., desc(date_time))),
         data = map(data, ~ mutate(., na_after = cumall(is.na(value)))),
         data = map(data, ~ arrange(., date_time)),
         data = map(data, ~ filter(., !na_before, !na_after))) %>%
  
  # Fill dates
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
# - Only check if curious

if(FALSE) {
  overlaps_plot <- pm25_clean %>%
    group_by(site) %>%
    filter(n_distinct(instrument) > 1) %>%
    ungroup()
  
  g1 <- plot_station_instruments(overlaps_plot, station = "site") +
    geom_vline(xintercept = ymd(rep_year - 2, truncated = 2))
  g2 <- plot_station_instruments(overlaps_plot, station = "site", 
                                 instrument = "instrument_type") +
    geom_vline(xintercept = ymd(rep_year - 2, truncated = 2))
  
  g <- g1 + g2 + 
    plot_annotation(title = "Sites with multiple instruments",
                    subtitle = "Exluding missing data") +
    plot_layout(guides = "collect")
  
  ggsave(filename = "out/pm25_instrument_overlap.png", width = 14, height = 10)
  
  # Check by dist
  dist_site <- filter(dist, dist <= 2000) %>% 
    select(-dist, -pair) %>%
    pivot_longer(contains("stn"), names_to = "pair", values_to = "site") %>%
    pull(site) %>% 
    unique()
  
  overlaps_plot <- pm25_clean %>%
    filter(site %in% dist_site) %>%
    filter(!is.na(value))
  
  g1 <- plot_station_instruments(overlaps_plot, station = "site") +
    geom_vline(xintercept = ymd(rep_year - 2, truncated = 2))
  g2 <- plot_station_instruments(overlaps_plot, station = "site", 
                                 instrument = "instrument_type") +
    geom_vline(xintercept = ymd(rep_year - 2, truncated = 2))
  
  g <- g1 + g2 + 
    plot_annotation(title = "Sites near other sites",
                    subtitle = "Exluding missing data") +
    plot_layout(guides = "collect")
  
  ggsave(filename = "out/pm25_stn_nearby.png", width = 14, height = 10)
  
  # All recent years (2018 - 2020) have only one instrument
}

## Assign instrument deployments ----------------------------------------------

# Look for overlapping dates when 
# - Same site, same instrument TYPE, multiple instruments
# - Over entire data record (required for 3-yr-rolling later on)

deps_ovlp <- pm25_clean %>%
  mutate(date = as_date(date_time)) %>%
  distinct() %>%
  group_by(site, instrument, instrument_type) %>%
  summarize(min_date = min(date), 
            max_date = max(date),
            n_days = n(), .groups = "drop") %>%
  add_count(site, instrument_type) %>%
  # Only care when more than one instrument per type per site
  filter(n > 1) %>%
  group_by(site, instrument_type) %>%
  arrange(min_date, .by_group = TRUE) %>%
  mutate(overlap = max(min_date) <= min(max_date)) %>%
  # Only care when they overlap in dates
  filter(overlap)
  
# Harmac Cedar Woobank instrument BAM1020_2 has only one day of operation 
# (2014-08-01) and it overlaps with the first day of BAM1020
# Let's omit it

pm25_clean <- filter(pm25_clean, !(site == "Harmac Cedar Woobank" &
                                     instrument == "BAM1020_2"))


## Check timeseries problems -----------------------
# - Check for missing/extra observations

t <- pm25_clean %>%
  nest(ts = c(-site, -instrument_type)) %>%
  mutate(n_distinct = map_int(ts, ~n_distinct(.$date_time)),
         n = map_int(ts, nrow),
         n_expect = map_dbl(ts, ~as.numeric(difftime(max(.$date_time), 
                                                     min(.$date_time), 
                                                     units = "hours")))) %>%
  filter(n_expect != n - 1, 
         n_distinct != n) %>%
  verify(nrow(.) == 0)

# None!


# Last details -----------------------

# Only keep stations with data
stations_clean <- semi_join(stations_clean, pm25_clean, by = "site")

# Write data ------------------------------
write_rds(stations_clean, "data/datasets/stations_clean.rds")
write_rds(pm25_clean, "data/datasets/pm25_clean.rds", compress = "gz")
