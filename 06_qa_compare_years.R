# Copyright 2026 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and 
# limitations under the License.

source("00_setup.R")

library("readr")
library("dplyr")
library("stringr")
library("tidyr")
library("purrr")
library("lubridate")
library("assertr")
library("rcaaqs")
library("bcdata")


# Load Data ----------------------
pm25_24h_mgmt <- read_rds("data/datasets/rep_year_2024/pm25_24h_mgmt.rds")
pm25_annual_mgmt <- read_rds("data/datasets/rep_year_2024/pm25_annual_mgmt.rds")
stations_clean <- read_rds("data/datasets/rep_year_2024/stations_clean.rds")
print_summary <- read_rds("data/datasets/rep_year_2024/print_summary.rds")
pm25_clean <- read_rds("data/datasets/rep_year_2024/pm25_clean.rds")
az_ambient <- read_rds("data/datasets/rep_year_2024/az_ambient.rds")

pm25_results <- bind_rows(get_caaqs(pm25_24h_mgmt),
                          get_caaqs(pm25_annual_mgmt)) %>%
  left_join(stations_clean, by = "site") %>% 
  ungroup() %>%
  distinct() %>%
  # Ensure only 1 analysis per site
  add_count(site, caaqs_year, metric, instrument_type) %>%
  assert(in_set(1), n) %>%
  # Clean up
  select(caaqs_year,
         airzone, 
         station_name = site, 
         region, 
         latitude = lat, 
         longitude = lon, 
         instrument_type,
         metric,	
         n_years,	
         min_year,	
         max_year,	
         metric_value_ambient,	
         caaqs_ambient,	
         excluded,	
         metric_value_mgmt,
         mgmt_level,
         everything(), -n, -flag_daily_incomplete, -flag_yearly_incomplete) %>% 
  arrange(airzone, caaqs_year)


# Compare 2021-2023 and 2022-2024 results for consistency
year_prev <- 2023
year_curr <- 2024

yrs_present <- sort(unique(pm25_results$caaqs_year))
summarise_year <- function(df, y) {
  az_achieved <- df %>%
                  filter(caaqs_year == y) %>%
                  nest(data = c(-metric, -caaqs_year)) %>%
                  mutate(data = map(data, ~airzone_metric(., keep = "station_name", station_id = "station_name"))) %>%
                  unnest(data) %>%
                  select(airzone, metric, caaqs_year, everything()) %>%
                  group_by(metric) %>%
                  summarise(az_achieved = sum(caaqs_ambient == "Achieved", na.rm = TRUE))
  df %>%
    filter(caaqs_year == y) %>%
    group_by(metric) %>%
    filter(!is.na(metric_value_ambient)) %>%
    summarise(
      year = y,
      n_stations = n(),
      n_achieved = sum(caaqs_ambient == "Achieved", na.rm = TRUE),
      pct_achieved = round(100 * n_achieved / n_stations),
      .groups = "drop")  %>%
      left_join(az_achieved, by = "metric")
}

summary_prev <- summarise_year(pm25_results, year_prev)
summary_curr <- summarise_year(pm25_results, year_curr)

# Combine for side-by-side comparison
summary_compare <- full_join(
  summary_prev, summary_curr,
  by = "metric",
  suffix = c("_prev", "_curr")
) %>%
  mutate(
    delta_n = n_stations_curr - n_stations_prev,
    delta_achieved = n_achieved_curr - n_achieved_prev,
    delta_pct = pct_achieved_curr - pct_achieved_prev
  )

print(summary_compare, width = Inf)

# diagnose which stations appear/disappear by metric
coverage_by_metric <- function(df, y) {
  df %>%
    filter(caaqs_year == y, !is.na(metric_value_ambient)) %>%
    distinct(metric, station_name)
}

cov_prev <- coverage_by_metric(pm25_results, year_prev)
cov_curr <- coverage_by_metric(pm25_results, year_curr)

added_stations <- anti_join(cov_curr, cov_prev, by = c("metric", "station_name")) %>%
  arrange(metric, station_name)
dropped_stations <- anti_join(cov_prev, cov_curr, by = c("metric", "station_name")) %>%
  arrange(metric, station_name)

print(added_stations)

print(dropped_stations)

# Station-level comparisons for stations present in BOTH years (by metric)
station_compare <- pm25_results %>%
  filter(caaqs_year %in% c(year_prev, year_curr)) %>%
  filter(!is.na(metric_value_ambient)) %>%
  select(caaqs_year, metric, station_name,
         metric_value_ambient, caaqs_ambient,
         metric_value_mgmt, mgmt_level,
         min_year, max_year, n_years) %>%
  mutate(caaqs_ambient = as.character(caaqs_ambient),
         mgmt_level = as.character(mgmt_level)) %>%
  pivot_wider(
    names_from = caaqs_year,
    values_from = c(metric_value_ambient, caaqs_ambient,
                    metric_value_mgmt, mgmt_level,
                    min_year, max_year, n_years),
    names_sep = "_"
  ) %>%
  # keep only those with BOTH years
  filter(
    !is.na(.data[[paste0("metric_value_ambient_", year_prev)]]) &
      !is.na(.data[[paste0("metric_value_ambient_", year_curr)]])
  )


# Flag any changes in achievement status or management level
changes <- station_compare %>%
  mutate(
    achievement_changed =
      .data[[paste0("caaqs_ambient_", year_prev)]] != .data[[paste0("caaqs_ambient_", year_curr)]],
    mgmt_changed =
      .data[[paste0("mgmt_level_", year_prev)]] != .data[[paste0("mgmt_level_", year_curr)]]) %>%
  filter(achievement_changed | mgmt_changed) %>%
  print(n = Inf, width = Inf)

# Transboundary Flows and Exceptional Events: compare previous (2023) vs current (2024) results for consistency
pm25_24h_caaqs <- pm_24h_caaqs(pm25_clean, by = c("site", "instrument_type"))

pm25_24h_lvl <- achievement_levels %>%
  filter(parameter == "pm2.5_24h", lower_breaks > 0) %>%
  pull(lower_breaks) 

tfee_dates <- pm25_clean %>%
  filter(flag_tfee) %>%
  # Ceiling TFEE to capture original dates
  mutate(date = ceiling_date(date_time, unit = "hour"),
         date = as_date(date)) %>%
  select(site, instrument_type, date) %>%
  distinct()

summarise_tfee <- function(df, y) {
  get_daily(df) %>% 
    ungroup() %>%
    filter(avg_24h >= pm25_24h_lvl, year(date) >= y - 2) %>% 
    semi_join(tfee_dates, by = c("site", "date")) %>%
    arrange(date) %>%
    group_by(year = year(date)) %>%
    mutate(n_year = n_distinct(date)) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = n_year, names_prefix = "n_tfee_days_") %>%
    summarize(n_sites = n_distinct(site),
              tfee_occurred_in_months = paste0(sort(unique(month(date))), collapse = ", "),
              across(starts_with("n_tfee_days_"), .fns = ~na.omit(unique(.))))
}



summarise_tfee_prev <- summarise_tfee(pm25_24h_caaqs, year_prev)
summarise_tfee_curr <- summarise_tfee(pm25_24h_caaqs, year_curr)

summarise_tfee_prev
summarise_tfee_curr
