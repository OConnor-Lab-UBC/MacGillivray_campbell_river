# Load required libraries
library(ggplot2)
library(tidyverse)

# load data
data<- read.csv("raw_data/sulfide.csv")

# clean data
## First, standardize NG/ng to be consistent
data <- data %>%
  mutate(type = case_when(
    type %in% c("ng", "NG") ~ "ng",
    TRUE ~ type)) 

##create a column that has core_id 
###(we have core number but these were reused across sites and time points, so to get number of cores we need unique core ids, here we use site and time point with core number to make unique core id)
data <- data %>%
  mutate(unique_core_id = paste(site, time_point, core, sep = "_")) %>%
  mutate(site = factor(site, 
                       levels = c("donor", "low", "high"),
                       labels = c("Donor", "Low", "High")))

## Create averaged data with standard error for each depth, type, site, and time point
data_avg <- data %>%
  group_by(site, type, depth_cm, time_point) %>%
  summarise(
    h2s_avg = mean(h2s, na.rm = TRUE),
    h2s_se = sd(h2s, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

#look at data
##Count cores for each combination
core_counts <- data %>%
  group_by(site, type, depth_cm, time_point) %>%
  summarise(n_cores = n_distinct(unique_core_id),
            .groups = 'drop')%>%
  pivot_wider(names_from = c(time_point, type), 
              values_from = n_cores)
core_counts
###**only have one low away T3 core, should have 3! need to look into this**


#Plots

##separated by time, site and treatment
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
  #geom_text(data = core_counts, 
           # aes(x = depth_cm, y = Inf, label = paste0("n=", n_cores)),
           # vjust = 1.5, size = 3, color = "black") +
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

##Plot for only T3
### Filter for T3 only
dataT3 <- data %>%
  filter(time_point == "T3")  # Filter for T3 only

# Create the plot
ggplot() +
  # All individual data points (colored by type)
  geom_point(data = dataT3, 
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
  theme_bw(base_size = 14) +
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

##Plot only away at T3
dataaway <- data %>%
  filter(type == "away")

### Calculate averages for away data only
dataaway_avg <- dataaway %>%
  group_by(site, depth_cm) %>%
  summarise(
    h2s_avg = mean(h2s, na.rm = TRUE),
    h2s_se = sd(h2s, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

### Create the plot
ggplot() +
  # Individual away data points
  geom_point(data = dataaway, 
             aes(x = h2s, y = depth_cm), 
             color = "grey9",
             size = 2, alpha = 0.5) +
  # Error bars for averages
  geom_errorbarh(data = dataaway_avg, 
                 aes(xmin = h2s_avg - h2s_se, 
                     xmax = h2s_avg + h2s_se, 
                     y = depth_cm),
                 color = "blue",
                 height = 0.5, 
                 linewidth = 0.8) +
  # Average points
  geom_point(data = dataaway_avg, 
             aes(x = h2s_avg, y = depth_cm), 
             color = "blue",
             shape = 15,
             size = 3) +
  facet_wrap(~ site, ncol = 3) +
  scale_y_reverse() +
  coord_cartesian(ylim = c(max(dataaway$depth_cm, na.rm = TRUE), 0)) +
  labs(x = expression(H[2]*S~(mu*M)), 
       y = "Depth (cm)") +
  theme_bw() +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 20))


# Plot all together, ignoring treatment - **DOnt use this one** just looking
##this is because g was more like away and with sampling errors gives better picture (a little janky so not sure about this)
data_combined <- data %>%
  mutate(type_combined = "all")

# Calculate averages across all treatments
data_combined_avg <- data_combined %>%
  group_by(site, depth_cm) %>%
  summarise(
    h2s_avg = mean(h2s, na.rm = TRUE),
    h2s_se = sd(h2s, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

# Create the plot
ggplot() +
  # All individual data points
  geom_point(data = data_combined, 
             aes(x = h2s, y = depth_cm), 
             color = "black",
             size = 2, alpha = 0.5) +
  # Error bars for averages
  geom_errorbarh(data = data_combined_avg, 
                 aes(xmin = h2s_avg - h2s_se, 
                     xmax = h2s_avg + h2s_se, 
                     y = depth_cm),
                 color = "black",
                 height = 0.5, 
                 linewidth = 0.8) +
  # Average points
  geom_point(data = data_combined_avg, 
             aes(x = h2s_avg, y = depth_cm), 
             color = "black",
             shape = 16,
             size = 3) +
  facet_wrap(~ site, ncol = 3) +
  scale_y_reverse() +
  coord_cartesian(ylim = c(max(data$depth_cm, na.rm = TRUE), 0)) +
  labs(x = expression(H[2]*S~(mu*M)), 
       y = "Depth (cm)") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 14))


