# Load required libraries 
library(jsonlite)
library(tidyverse)

 
# Data source: https://climatereanalyzer.org/clim/sst_daily/
# Retrieve current data and clean up
json_path <- "https://climatereanalyzer.org/clim/sst_daily/json_2clim/oisst2.1_world2_sst_day.json"

oisst2_data <- fromJSON(url(json_path), flatten = FALSE) %>%
  select(-data_source) %>%
  mutate(data = str_remove(data, "c"),
         data = str_remove(data, "\\("),
         data = str_remove(data, "\\)")) %>%
  separate(col = data,
           sep = ", ",
           into = paste("Day", 1:366)) %>%
  mutate_at(c(2:367), ~str_remove(., "NA")) %>%
  mutate_at(c(2:367), as.numeric) %>%
  drop_na("Day 1")

# Current year
current_yr <- lubridate::year(today())
current_yrQ <- as.character(current_yr)

# Pivot to long format, select relevant variables
oisst2_long <- oisst2_data %>%
  pivot_longer(-name, names_to = "day", values_to = "temp C") %>%
  mutate(day = factor(day, levels = c(paste("Day", 1:366)))) %>%
  filter(name %in% c("1982-2010", current_yr, current_yr-1, current_yr-2)) 

# Find the anomalies for the current year
oisst2_anomaly <- oisst2_long %>%
  pivot_wider(names_from = name, values_from = `temp C`) %>%
  mutate(anomaly = `2025`-`1982-2010`)

# Define the range of 2x standard deviation from the 1982-2010 mean
sd_bands <- 
  left_join(
    oisst2_data %>%
      pivot_longer(-name, names_to = "day", values_to = "temp C") %>%
      mutate(day = factor(day, levels = c(paste("Day", 1:366)))) %>%
      filter(name == "1982-2010"), 
    oisst2_data %>%
      pivot_longer(-name, names_to = "day", values_to = "temp C") %>%
      mutate(day = factor(day, levels = c(paste("Day", 1:366)))) %>%
      filter(name %in% c(1982:2010)) %>%
      group_by(day) %>%
      summarize(SD = sd(`temp C`)),
    by = "day") %>%
  rowwise() %>%
  mutate(
    `plus 2σ` = `temp C`+(SD*2),
    `minus 2σ` = `temp C`-(SD*2)) %>%
  select(day, `plus 2σ`, `minus 2σ`)

# Set parameters - Current reading, anomaly, breaks 
current <- oisst2_anomaly %>%
  select(day, "temp C" = current_yrQ, anomaly) %>%
  na.omit() %>%
  tail(n = 1)

max <- oisst2_anomaly %>%
  select(day, "temp C" = current_yrQ, anomaly) %>%
  na.omit() %>%
  slice_max(`anomaly`) %>%
  filter(row_number()==ceiling(n()/2))

DayBreak <- factor(paste("Day", 1:366)[seq(1, 366, by = 14)], levels = c(paste("Day", 1:366)))

# Plot in ggplot2
oisst2_long %>%
  ggplot(aes(x = day, y = `temp C`, group = name, color = name)) +
  # Create the mean + sigma ribbon
  geom_ribbon(aes(
    ymin = rep(sd_bands$`minus 2σ`, 4),
    ymax = rep(sd_bands$`plus 2σ`, 4)),
    fill = "#dfe6e3",
    color = "#dfe6e3",
    show.legend = FALSE) +
  # Add lines for the historic mean, current year, and 2x sigmas
  # geom_line(linetype = ifelse(oisst2_long$name == "1982-2011 mean", "solid", "dashed"), size = 1) +
  geom_line(linewidth = 1) +
  # Color the lines
  scale_color_manual(values = c("#43464a", "#8bc48b", "#4ca889", "#146edb", "#dfe6e3", "#dfe6e3")) +
  # Adjust the x-axis to show every 14 days
  scale_x_discrete(breaks = DayBreak, guide = guide_axis(angle = 90)) +
  # Create a label for the current anomaly 
  annotate(geom = "text", x = current$day, y = current$`temp C`, 
           label = paste0("current anomaly\nis ", round(current$anomaly, digits = 2), "°C"),
           size = 3, vjust = -0.9, color = "#146edb") +
  annotate(geom = "point", x = current$day, y = current$`temp C`, 
           size = 6, shape = 21, fill = "transparent", color = "#146edb") +
  # Create a label for the maximum anomaly during the current year
  annotate(geom = "text", x = max$day, y = max$`temp C`, 
           label = paste0("max anomaly\nwas ", round(max$anomaly, digits = 2), "°C"),
           size = 3, vjust = -0.9, color = "#146edb") +
  annotate(geom = "point", x = max$day, y = max$`temp C`, 
           size = 6, shape = 21, fill = "transparent", color = "#146edb") +
  # Expand the y-axis limits to create room for the above annotations 
  ylim(c(min(sd_bands$`minus 2σ`)*.99, max(oisst2_long$`temp C`, na.rm = T)*1.01)) +
  # Adjust the labels of the graph
  labs(
    title = "Daily Sea Surface Temperature",
    subtitle = "World 60S-60N",
    x = "",
    y = "Temperature (°C)",
    color = "Key",
    caption = "Data Source: NOAA OISST V2.1") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#fafffd",
                                        colour = "#fafffd",
                                        linetype = "solid")) +
  coord_cartesian(clip = 'off')
