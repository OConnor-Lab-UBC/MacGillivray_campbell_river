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

#create a columne that has unique_core_id
data <- data %>%
  mutate(unique_core_id = paste(site, time_point, core, sep = "_"))

# Create averaged data with standard error for each depth, type, site, and timepoint
data_avg <- data %>%
  group_by(site, type, depth_cm, time_point) %>%
  summarise(
    h2s_avg = mean(h2s, na.rm = TRUE),
    h2s_se = sd(h2s, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

#look at data
# Count cores for each combination
core_counts <- data %>%
  group_by(site, type, depth_cm, time_point) %>%
  summarise(n_cores = n_distinct(unique_core_id),
            .groups = 'drop')
print(core_counts)

# Or if you want to see it as a wider table
core_counts_wide <- core_counts %>%
  pivot_wider(names_from = c(time_point, type), 
              values_from = n_cores)
core_counts_wide

# Create the plot
ggplot() +
  # All individual data points (colored by type)
  geom_point(data = data, 
             aes(x = h2s, y = depth_cm, color = type), 
             size = 2, alpha = 0.5) +
  # Error bars for averages
  geom_errorbarh(data = data_avg, 
                 aes(xmin = h2s_avg - h2s_se, 
                     xmax = h2s_avg + h2s_se, 
                     y = depth_cm, 
                     color = type),
                 height = 0.5, 
                 linewidth = 0.8) +
  # Average points (shaped by type)
  geom_point(data = data_avg, 
             aes(x = h2s_avg, y = depth_cm, shape = type, color = type), 
             size = 3) +
  #add core counts
  geom_text(data = core_counts, 
            aes(x = depth_cm, y = Inf, label = paste0("n=", n_cores)),
            vjust = 1.5, size = 3, color = "black") +
  facet_grid(time_point ~ site) +
  scale_y_reverse() +
  coord_cartesian(ylim = c(max(data$depth_cm, na.rm = TRUE), 0)) +
  scale_color_manual(
    values = c("g" = "black", "ng" = "green", "away" = "blue"),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  scale_shape_manual(
    values = c("g" = 16, "ng" = 17, "away" = 15),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  labs(x = expression(H[2]*S~(mu*M)), 
       y = "Depth (cm)", 
       color = "Treatment", 
       shape = "Treatment") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 12))



##only T3
# Load required libraries
library(ggplot2)
library(dplyr)

# First, standardize NG/ng to be consistent and filter for T3 only
data <- data %>%
  mutate(type = case_when(
    type %in% c("ng", "NG") ~ "ng",
    TRUE ~ type)) %>%
  filter(timepoint == "T3")  # Filter for T3 only

# Create averaged data with standard error for each depth, type, and site
data_avg <- data %>%
  group_by(site, type, depth_cm) %>%
  summarise(
    h2s_avg = mean(h2s, na.rm = TRUE),
    h2s_se = sd(h2s, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Create the plot
ggplot() +
  # All individual data points (colored by type)
  geom_point(data = data, 
             aes(x = h2s, y = depth_cm, color = type), 
             size = 2, alpha = 0.5) +
  # Error bars for averages
  geom_errorbarh(data = data_avg, 
                 aes(xmin = h2s_avg - h2s_se, 
                     xmax = h2s_avg + h2s_se, 
                     y = depth_cm, 
                     color = type),
                 height = 0.5, 
                 linewidth = 0.8) +
  # Average points (shaped by type)
  geom_point(data = data_avg, 
             aes(x = h2s_avg, y = depth_cm, shape = type, color = type), 
             size = 3) +
  facet_wrap(~ site, ncol = 3) +
  scale_y_reverse() +
  coord_cartesian(ylim = c(max(data$depth_cm, na.rm = TRUE), 0)) +
  scale_color_manual(
    values = c("g" = "black", "ng" = "green", "away" = "blue"),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  scale_shape_manual(
    values = c("g" = 16, "ng" = 17, "away" = 15),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  labs(x = expression(H[2]*S~(mu*M)), 
       y = "Depth (cm)", 
       color = "Treatment", 
       shape = "Treatment") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 14))

