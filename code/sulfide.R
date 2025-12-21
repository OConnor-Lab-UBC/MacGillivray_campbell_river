# Load required libraries
library(ggplot2)
library(tidyverse)

# load data
data<- read.csv("raw_data/sulfide.csv")

# Create the plot
# Load required libraries
library(ggplot2)
library(dplyr)

# First, standardize NG/ng to be consistent
data <- data %>%
  mutate(type = case_when(
    type %in% c("ng", "NG") ~ "ng",
    TRUE ~ type))

# Create averaged data for each depth, type, and site
data_avg <- data %>%
  group_by(site, type, depth_cm) %>%
  summarise(h2s_avg = mean(h2s, na.rm = TRUE), .groups = "drop")

# Create the plot
ggplot() +
  # All individual data points (colored by type)
  geom_point(data = data, 
             aes(x = h2s, y = depth_cm, color = type),
             size = 2, alpha = 0.5) +
  # Lines connecting averages (by type within each site)
  geom_line(data = data_avg, 
            aes(x = h2s_avg, y = depth_cm, color = type, linetype = type),
            linewidth = 0.8) +
  # Average points (shaped by site)
  geom_point(data = data_avg, 
             aes(x = h2s_avg, y = depth_cm, shape = type, color = type),
             size = 3) +
  facet_wrap(~ site, ncol = 3) +
  scale_y_reverse() +
  coord_cartesian(ylim = c(max(data$depth_cm, na.rm = TRUE), 0)) +
  scale_color_manual(
    values = c("g" = "black", "ng" = "green", "away" = "blue"),
    labels = c("g" = "Galvanized", 
               "ng" = "Non-galvanized",
               "away" = "Away")) +
  scale_linetype_manual(
    values = c("g" = "solid", "ng" = "dashed", "away" = "dotted"),
    labels = c("g" = "Galvanized", 
               "ng" = "Non-galvanized",
               "away" = "Away")) +
  labs(x = expression(H[2]*S~(mu*M)),
       y = "Depth (cm)",
       color = "Treatment",
       shape = "type",
       linetype = "Treatment") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 14))

