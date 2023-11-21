# Load required libraries 
library(jsonlite)
library(tidyverse)

 
# Data source: https://climatereanalyzer.org/clim/sst_daily/
# Download current data and load
json_path <- "oisst2.1_world2_sst_day.json"


# Load data path and clean data
oisst2_data <- fromJSON(json_path, flatten = FALSE) %>%
  mutate(data = str_remove(data, "c"),
         data = str_remove(data, "\\("),
         data = str_remove(data, "\\)")) %>%
  separate(col = data,
           sep = ", ",
           into = paste("Day", 1:366)) %>%
  mutate_at(c(2:367), ~str_remove(., "NA")) %>%
  mutate_at(c(2:367), as.numeric) 


# Pivot to long format, select relevant variables
oisst2_long <- oisst2_data %>%
  filter(name %in% c("1982-2011 mean", "plus 2σ", "minus 2σ", "2023", "2022", "2021")) %>%
  pivot_longer(-name, names_to = "day", values_to = "temp C") %>%
  mutate(day = factor(day, levels = c(paste("Day", 1:366))))


# Pivot to wide format, select relevant variables 
oisst2_wide <- setNames(data.frame(t(oisst2_data[, -1])), oisst2_data[, 1]) %>%
  rownames_to_column(var = "day") %>%
  select(day, `1982-2011 mean`, `plus 2σ`, `minus 2σ`, `2023`, `2022`) %>%
  mutate(anomaly = `2023`-`1982-2011 mean`) %>%
  mutate(max_anomoly = ifelse(anomaly == max(anomaly), `2023`, NA))


# Define vectors and points that will be used in the plot
"minus 2σ" <- oisst2_long %>% filter(name == "minus 2σ") %>% .$`temp C`
"plus 2σ" <- oisst2_long %>% filter(name == "plus 2σ") %>% .$`temp C`

current <- oisst2_wide %>%
  select(day, `2023`, anomaly) %>%
  na.omit() %>%
  tail(n = 1)

max <- oisst2_wide %>%
  select(day, `2023`, anomaly) %>%
  na.omit() %>%
  slice_max(`2023`) %>%
  filter(row_number()==ceiling(n()/2))


# Plot in ggplot2
oisst2_long %>%
  ggplot(aes(x = day, y = `temp C`, group = name, color = name)) +
  # Create the mean + sigma ribbon
  geom_ribbon(aes(
    ymin = rep(`minus 2σ`, 6),
    ymax = rep(`plus 2σ`, 6)),
    fill = "#dfe6e3",
    show.legend = FALSE) +
  # Add lines for the historic mean, current year, and 2x sigmas
  # geom_line(linetype = ifelse(oisst2_long$name == "1982-2011 mean", "solid", "dashed"), size = 1) +
  geom_line(linewidth = 1) +
  # Color the lines
  scale_color_manual(values = c("#43464a", "#8bc48b", "#4ca889", "#146edb", "#dfe6e3", "#dfe6e3")) +
  # Adjust the x-axis to show every 14 days
  scale_x_discrete(breaks = oisst2_wide$day[seq(1, length(oisst2_wide$day), by = 14)],
                   guide = guide_axis(angle = 90)) +
  # Create a label for the current anomaly 
  annotate(geom = "text", x = current$day, y = current$`2023`, 
           label = paste0("current anomaly\nis ", round(current$anomaly, digits = 2), "°C"),
           size = 3, vjust = -0.9, color = "#146edb") +
  annotate(geom = "point", x = current$day, y = current$`2023`, 
           size = 6, shape = 21, fill = "transparent", color = "#146edb") +
  # Create a label for the maximum anomaly during the current year
  annotate(geom = "text", x = max$day, y = max$`2023`, 
           label = paste0("max anomaly\nwas ", round(max$anomaly, digits = 2), "°C"),
           size = 3, vjust = -0.9, color = "#146edb") +
  annotate(geom = "point", x = max$day, y = max$`2023`, 
           size = 6, shape = 21, fill = "transparent", color = "#146edb") +
  # Expand the y-axis limits to create room for the above annotations 
  ylim(c(min(`minus 2σ`)*.99, max(max$`2023`)*1.01)) +
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
                                        linetype = "solid"))


  