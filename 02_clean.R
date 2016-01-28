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
library("magrittr")
library("rcaaqs")
# only required for the monitor plot at the bottom
# library("ggplot2")
# library("scales")
# library("envreportutils")

if (!exists("pm25")) load("tmp/pm-raw.RData")

reporting_year <- 2014
min_year <- 2012

## Format dates, extract 2011-2013, and fill missing
pm25$date_time <- format_date(pm25$date_time) - 1 # Subtract 1 second so reading is assigned to previous hour
pm25 <- pm25[pm25$date_time >= as.POSIXct(paste0(min_year, "-01-01"), tz = "Etc/GMT+8") & 
               pm25$date_time < as.POSIXct(paste0(reporting_year + 1,"-01-01"), tz = "Etc/GMT+8"), ]

pm25 %<>% 
  group_by(ems_id) %>% 
  do(date_fill(., date_col = "date_time", interval = "hour", 
               fill_cols = c("ems_id", "site"))) %>%
  ungroup() %>%
  as.data.frame(stringsAsFactors = FALSE)

## Clean up negative pm25 readings
pm25$value <- clean_neg(pm25$value, type = "pm25")

## Parse columns to figure out the (simplified) type of pm25 monitor
pm25$simple_monitor <- parse_pm_monitors(pm25$monitor, pm25$instrument)

## Remove GRIMMs and FDMS
pm25 %<>% filter(!simple_monitor %in% c("GRIMM", "FDMS"))

# Summaries ---------------------------------------------------------------

## Function to choose which record in stn_data to use for each station
pick_which <- function(x, target, match) {
  op <- target == "OPERATIONAL" # First choose if operational
  if (sum(op) == 1) return(op) # if only one operational use that
  
  matches <- grepl(match, x) # Then choose on a regex match
  op_matches <- op + matches
  op_matches_log <- as.logical(op_matches - 1)
  
  if (sum(op_matches_log) == 1) return(op_matches_log)
  
  # Then choose the one with the shortest name
  count_chars <- nchar(x) == min(nchar(x))
  all_tests <- count_chars + op_matches_log
  all_tests_log <- as.logical(all_tests - 1)
  
  # if still more than one match, pick the first
  if (sum(all_tests_log) > 1) {
    trues <- which(all_tests_log)
    all_tests_log <- rep(FALSE, length(all_tests_log))
    all_tests_log[trues[1]] <- TRUE
  }
  all_tests_log
}

data_summary <- pm25 %>%
  filter(!is.na(value)) %>% 
  group_by(ems_id, site, monitor, instrument, simple_monitor) %>%
  summarise(min_date = min(date_time), 
            max_date = max(date_time)) %>% 
  group_by(ems_id, site, add = FALSE) %>% 
  slice(which.max(max_date - min_date))

## Clean up station file
stn_data %<>% 
  filter(ems_id %in% data_summary$ems_id) %>% 
  group_by(ems_id) %>% 
  mutate(pick = pick_which(display_name, target, "60")) %>% 
  filter(pick) %>%
  mutate(display_name = gsub("_\\d+$|(_|\\s)Met.*", "", display_name)) %>% 
  as.data.frame(stringsAsFactors = FALSE)

## Save the clean processed file
dir.create("tmp", showWarnings = FALSE)
save(pm25, data_summary, stn_data, file = "tmp/pm25-processed.RData")

##############

## Plot monitor deployments
# monitor_plot <- data_summary %>% 
#   #filter(ems_id %in% data_summary$ems_id[which(duplicated(data_summary$ems_id))]) %>%
#   mutate(mon_inst = paste0(monitor, ": ", instrument)) %>%
#   ggplot(aes(x = mon_inst, ymin = min_date, ymax = max_date, colour = simple_monitor)) + 
#   facet_grid(site ~ ., scales = "free_x", labeller = label_wrap_gen(15)) + 
#   geom_line() + 
#   scale_y_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) + 
#   geom_hline(yintercept = as.numeric(as.POSIXct("2011-01-01"))) + 
#   coord_flip() + 
#   guides(colour = guide_legend("Simplified Instrument Type")) + 
#   labs(title = "Dates PM2.5 monitors were deployed at sites where there has been more than one monitor", 
#        x = "Monitor and Instrument", y = "Date") + 
#   theme_soe_facet() + 
#   theme(text = element_text(size = 5), axis.text.y = element_text(hjust = 0, size = 3), 
#         axis.title.y = element_text(angle = 90), 
#         line = element_line(size = 0.2), legend.position = "top", axis.line = element_blank(), 
#         legend.direction = "horizontal", strip.text.y = element_text(angle = 0))
# 
# dir.create("out", showWarnings = FALSE)
# pm25_monitors_pdf <- "out/pm25_monitors.pdf"
# ggsave(pm25_monitors_pdf, monitor_plot, width = 7, height = 60, 
#        units = "in", limitsize = FALSE)
# embed_fonts(pm25_monitors_pdf)

