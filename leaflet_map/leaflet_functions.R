
create_popup <- function(data, caaqs = "o3", type = "polygon") {
  
  data %>%
    # Define individual elements
    title_popup(., type) %>%
    metric_popup(., caaqs) %>%
    standard_popup(., caaqs) %>%
    mutate(popup_svg = paste0("./station_plots/", p_station_id, "_lineplot.svg"),
    # Create the rows
    popup_row1 = paste0("<div class = 'popup-row'>\n",
                        "  <div class = 'title'>\n", popup_title, "  </div>\n",
                        "</div>\n"),
    popup_row2 = paste0("<div class = 'popup-row'>\n",
                        "  <div class = 'section-metric'>\n", popup_metric, "  </div>\n",
                        "  <div class = 'section-standard' ",
                        "style = 'background-color: ", popup_standard_col, "'>\n",
                        popup_standard, "  </div>\n",
                        "</div>\n"),
    popup_row3 = paste0("<img src = ", popup_svg, ">"),
    
    # Assemble them all together
    popup = pmap_chr(list(popup_row1, popup_row2, popup_row3),
                     ~HTML(paste0(..1, ..2, ..3))))
  
}

title_popup <- function(data, type) {
  if(type == "polygon") {
    data <- mutate(data, popup_title = paste0("    <h2>Air Zone: ", p_az, "</h2>\n",
                                              "    <h4>Station: ", p_station, "</h4>\n"))
  } else if(type == "markers") {
    data <- mutate(data, popup_title = paste0("    <h2>Station: ", p_station, "</h2>\n",
                                              "    <h4>Air Zone: ", p_az, "</h4>\n"))
  }
  data
}

metric_popup <- function(data, caaqs) {
  if(caaqs == "o3") {
    m <- "Ozone Metric"
    units <- "ppm"
  } else if (caaqs == "pm2.5_annual") {
    m <- "PM<sub>2.5</sub> Metric (annual)"
    units <- "&mu;g/m&sup3;"
  } else if (caaqs == "pm2.5_24h") {
    m <- "PM<sub>2.5</sub> Metric (24h)"
    units <- "&mu;g/m&sup3;"
  }
  
  data <- mutate(data,
                 popup_metric = if_else(caaqs == "Insufficient Data", 
                                        caaqs, paste(metric_value, units)),
                 popup_metric = paste0("    <h4>", m, "</h4>\n",
                                       "    <h3>", popup_metric, "</h3>\n"),
                 popup_metric = if_else(caaqs == "Insufficient Data",
                                        popup_metric,
                                        paste0(popup_metric, 
                                               "    <span>(", n_years, 
                                               " year average)</span>\n")))
}

standard_popup <- function(data, caaqs) {
  s <- case_when(caaqs == "o3" ~ "Ozone Air Quality Standard",
                 caaqs == "pm2.5_annual" ~ "PM<sub>2.5</sub> Air Quality Standard (annual)",
                 caaqs == "pm2.5_24h" ~ "PM<sub>2.5</sub> Air Quality Standard (24h)")
  
  data <- mutate(data, 
                 popup_standard = paste0("    <h4>", s, "</h4>\n",
                                         "    <h2>", caaqs, "</h2>\n"),
                 popup_standard_col = case_when(caaqs == "Achieved" ~ "#377EB8",
                                                caaqs == "Not Achieved" ~ "#B8373E",
                                                caaqs == "Insufficient Data" ~ "#CCCCCC",
                                                TRUE ~ as.character(NA)))
}

