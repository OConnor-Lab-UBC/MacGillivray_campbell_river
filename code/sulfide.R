# Load required libraries
library(ggplot2)
library(tidyverse)

# load data
data_h2s<- read.csv("raw_data/sulfide.csv")
data_fe <- read.csv("raw_data/Fe.csv")

# clean data
## h2s
data_h2s <- data_h2s %>%
  mutate(type = case_when(
    type %in% c("ng", "NG") ~ "ng",
    TRUE ~ type)) %>%
  mutate(unique_core_id = paste(site, time_point, core, sep = "_")) %>%
  mutate(site = factor(site, 
                       levels = c("donor", "low", "high"),
                       labels = c("Donor", "Low", "High"))) %>%
  rename(conc = h2s) %>%  # Rename fe column 
  mutate(variable = "H2S",
         rep = as.character(rep))  # Convert rep to character

# Clean Fe data (adjust column names as needed)
data_fe <- data_fe %>%
  mutate(type = case_when(
    type %in% c("ng", "NG") ~ "ng",
    TRUE ~ type)) %>%
  mutate(unique_core_id = paste(site, time_point, core, sep = "_")) %>%
  mutate(site = factor(site, 
                       levels = c("donor", "low", "high"),
                       labels = c("Donor", "Low", "High"))) %>%
  rename(conc = total_Fe_uM) %>%  # Rename fe column 
  mutate(variable = "Fe",
         rep = as.character(rep))  # Convert rep to character

#combine datasets
data_combined <- bind_rows(data_h2s, data_fe)

##_________________________________
#**H2S**

## Create averaged data with standard error for each depth, type, site, and time point
data_avg_h2s<- data_h2s %>%
  group_by(site, type, depth_cm, time_point) %>%
  summarise(
    h2s_avg = mean(conc, na.rm = TRUE),
    h2s_se = sd(conc, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

#look at data
##Count cores for each combination
core_counts_h2s <- data_h2s %>%
  group_by(site, type, depth_cm, time_point) %>%
  summarise(n_cores = n_distinct(unique_core_id),
            .groups = 'drop')%>%
  pivot_wider(names_from = c(time_point, type), 
              values_from = n_cores)
core_counts_h2s
###**only have one low away T3 core, should have 3! need to look into this**


#Plots

##separated by time, site and treatment
ggplot() +
  # All individual data points (colored by type)
  geom_point(data = data_h2s, 
             aes(x = conc, y = depth_cm, color = type), 
             size = 2, alpha = 0.5) +
  # Error bars for averages
  geom_errorbarh(data = data_avg_h2s, 
                 aes(xmin = h2s_avg - h2s_se, 
                     xmax = h2s_avg + h2s_se, 
                     y = depth_cm, 
                     color = type),
                 height = 0.5, 
                 linewidth = 0.8) +
  # Average points (shaped by type)
  geom_point(data = data_avg_h2s, 
             aes(x = h2s_avg, y = depth_cm, shape = type, color = type), 
             size = 3) +
  #add core counts
  #geom_text(data = core_counts, 
           # aes(x = depth_cm, y = Inf, label = paste0("n=", n_cores)),
           # vjust = 1.5, size = 3, color = "black") +
  facet_grid(time_point ~ site) +
  scale_y_reverse() +
  coord_cartesian(ylim = c(max(data_h2s$depth_cm, na.rm = TRUE), 0)) +
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
dataT3_h2s <- data_h2s %>%
  filter(time_point == "T3")  # Filter for T3 only

# Create the plot
ggplot() +
  # All individual data points (colored by type)
  geom_point(data = dataT3_h2s, 
             aes(x = conc, y = depth_cm, color = type), 
             size = 2, alpha = 0.5) +
  # Error bars for averages
  geom_errorbarh(data = data_avg_h2s, 
                 aes(xmin = h2s_avg - h2s_se, 
                     xmax = h2s_avg + h2s_se, 
                     y = depth_cm, 
                     color = type),
                 height = 0.5, 
                 linewidth = 0.8) +
  # Average points (shaped by type)
  geom_point(data = data_avg_h2s, 
             aes(x = h2s_avg, y = depth_cm, shape = type, color = type), 
             size = 3) +
  facet_wrap(~ site, ncol = 3) +
  theme_bw(base_size = 14) +
  scale_y_reverse() +
  coord_cartesian(ylim = c(max(data_h2s$depth_cm, na.rm = TRUE), 0)) +
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
dataaway_h2s <- data_h2s %>%
  filter(type == "away")

### Calculate averages for away data only
dataaway_avg_h2s <- dataaway_h2s %>%
  group_by(site, depth_cm) %>%
  summarise(
    h2s_avg = mean(conc, na.rm = TRUE),
    h2s_se = sd(conc, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

### Create the plot
ggplot() +
  # Individual away data points
  geom_point(data = dataaway_h2s, 
             aes(x = conc, y = depth_cm), 
             color = "grey9",
             size = 2, alpha = 0.5) +
  # Error bars for averages
  geom_errorbarh(data = dataaway_avg_h2s, 
                 aes(xmin = h2s_avg - h2s_se, 
                     xmax = h2s_avg + h2s_se, 
                     y = depth_cm),
                 color = "blue",
                 height = 0.5, 
                 linewidth = 0.8) +
  # Average points
  geom_point(data = dataaway_avg_h2s, 
             aes(x = h2s_avg, y = depth_cm), 
             color = "blue",
             shape = 15,
             size = 3) +
  facet_wrap(~ site, ncol = 3) +
  scale_y_reverse() +
  coord_cartesian(ylim = c(max(dataaway_h2s$depth_cm, na.rm = TRUE), 0)) +
  labs(x = expression(H[2]*S~(mu*M)), 
       y = "Depth (cm)") +
  theme_bw() +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 20))

##_________________________________
#**Fe**

## Create averaged data with standard error for each depth, type, site, and time point
data_avg_fe<- data_fe %>%
  group_by(site, type, depth_cm, time_point) %>%
  summarise(
    fe_avg = mean(conc, na.rm = TRUE),
    fe_se = sd(conc, na.rm = TRUE) / sqrt(n()),
    .groups = "drop") %>% 
  filter(!is.na(site))

#look at data
##Count cores for each combination
core_counts_fe <- data_fe %>%
  group_by(site, type, depth_cm, time_point) %>%
  summarise(n_cores = n_distinct(unique_core_id),
            .groups = 'drop')%>%
  pivot_wider(names_from = c(time_point, type), 
              values_from = n_cores)
core_counts_fe

#Plots
##separated by time, site and treatment
ggplot() +
  # All individual data points (colored by type)
  geom_point(data = data_fe, 
             aes(x = conc, y = depth_cm, color = type), 
             size = 2, alpha = 0.5) +
  # Error bars for averages
  geom_errorbarh(data = data_avg_fe, 
                 aes(xmin = fe_avg - fe_se, 
                     xmax = fe_avg + fe_se, 
                     y = depth_cm, 
                     color = type),
                 height = 0.5, 
                 linewidth = 0.8) +
  # Average points (shaped by type)
  geom_point(data = data_avg_fe, 
             aes(x = fe_avg, y = depth_cm, shape = type, color = type), 
             size = 3) +
  #add core counts
  #geom_text(data = core_counts, 
  # aes(x = depth_cm, y = Inf, label = paste0("n=", n_cores)),
  # vjust = 1.5, size = 3, color = "black") +
  facet_grid(time_point ~ site) +
  scale_y_reverse() +
  coord_cartesian(ylim = c(max(data_fe$depth_cm, na.rm = TRUE), 0)) +
  scale_color_manual(
    values = c("g" = "black", "ng" = "green", "away" = "blue"),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  scale_shape_manual(
    values = c("g" = 16, "ng" = 17, "away" = 15),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  labs(x = expression("Total Fe " * (mu*M)),
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
dataT3_fe <- data_fe %>%
  filter(time_point == "T3")  %>% 
  filter(!is.na(site))

# Create the plot
ggplot() +
  # All individual data points (colored by type)
  geom_point(data = dataT3_fe, 
             aes(x = conc, y = depth_cm, color = type), 
             size = 2, alpha = 0.5) +
  # Error bars for averages
  geom_errorbarh(data = data_avg_fe, 
                 aes(xmin = fe_avg - fe_se, 
                     xmax = fe_avg + fe_se, 
                     y = depth_cm, 
                     color = type),
                 height = 0.5, 
                 linewidth = 0.8) +
  # Average points (shaped by type)
  geom_point(data = data_avg_fe, 
             aes(x = fe_avg, y = depth_cm, shape = type, color = type), 
             size = 3) +
  facet_wrap(~ site, ncol = 3) +
  theme_bw(base_size = 14) +
  scale_y_reverse() +
  coord_cartesian(ylim = c(max(data_fe$depth_cm, na.rm = TRUE), 0)) +
  scale_color_manual(
    values = c("g" = "black", "ng" = "green", "away" = "blue"),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  scale_shape_manual(
    values = c("g" = 16, "ng" = 17, "away" = 15),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  labs(x= expression("Total Fe " * (mu*M)), 
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
dataaway_fe <- data_fe %>%
  filter(type == "away")

### Calculate averages for away data only
dataaway_avg_fe <- dataaway_fe %>%
  group_by(site, depth_cm) %>%
  summarise(
    fe_avg = mean(conc, na.rm = TRUE),
    fe_se = sd(conc, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

### Create the plot
ggplot() +
  # Individual away data points
  geom_point(data = dataaway_fe, 
             aes(x = conc, y = depth_cm), 
             color = "grey9",
             size = 2, alpha = 0.5) +
  # Error bars for averages
  geom_errorbarh(data = dataaway_avg_fe, 
                 aes(xmin = fe_avg - fe_se, 
                     xmax = fe_avg + fe_se, 
                     y = depth_cm),
                 color = "blue",
                 height = 0.5, 
                 linewidth = 0.8) +
  # Average points
  geom_point(data = dataaway_avg_fe, 
             aes(x = fe_avg, y = depth_cm), 
             color = "blue",
             shape = 15,
             size = 3) +
  facet_wrap(~ site, ncol = 3) +
  scale_y_reverse() +
  coord_cartesian(ylim = c(max(dataaway_fe$depth_cm, na.rm = TRUE), 0)) +
  labs(x= expression("Total Fe " * (mu*M)), 
       y = "Depth (cm)") +
  theme_bw() +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 20))

