#load libraries
library(tidyverse)
library(ggplot2)

data<- read_csv("raw_data/plant_data.csv")

# Remove non-numeric IDs, keep only numeric plant IDs
data_id_only <- data %>%
  filter(!is.na(id), id != "", !grepl("[^0-9]", id))

data_natural <- data %>%
  filter(site == "natural")

# Filter for collected plants with measurement values
collected_id <- data_id_only %>%
  filter(!is.na(date_collected)) %>%
  # Check which measurement columns have data
  mutate(
    has_length = !is.na(length_measured) | !is.na(length_calculated >0),
    has_weight = !is.na(shoot_weight >0) | !is.na(rhizome_weight >0),
    has_blade_measurements = !is.na(b1_full),
    has_epiphytes = !is.na(epi_weight) & epi_weight > 0)

# Process natural shoots similarly
collected_natural <- data_natural %>%
  filter(!is.na(date_collected)) %>%
  mutate(
    has_length = !is.na(length_measured) | !is.na(length_calculated),
    has_weight = !is.na(shoot_weight) | !is.na(rhizome_weight),
    has_blade_measurements = !is.na(b1_full),
    has_epiphytes = !is.na(epi_weight) & epi_weight > 0,
    g_ng = "natural")  # Ensure natural shoots are marked

# Combine both datasets
collected_with_measurements <- bind_rows(collected_natural, collected_id)

length_data_all <- collected_with_measurements %>%
  filter(!is.na(site), site %in% c("donor", "high", "low", "natural")) %>%
  mutate(
    total_length = as.numeric(ifelse(!is.na(length_measured), length_measured, length_calculated))) %>%
  filter(!is.na(total_length))

# Calculate length/sheath ratio and leaf area (LA)
# Prepare data with calculated metrics
length_analysis <- collected_with_measurements %>%
  filter(!is.na(site), site %in% c("donor", "high", "low", "natural")) %>%
  mutate(
    # Use length_measured if available, otherwise use length_calculated
    # Convert to numeric immediately
    total_length = as.numeric(ifelse(!is.na(length_measured), length_measured, length_calculated)),
    # Convert sheath_length to numeric (it's currently character)
    sheath_length_num = as.numeric(sheath_length),
    # Calculate length/sheath ratio
    length_sheath_ratio = total_length / sheath_length_num,
    # Calculate leaf area: length * width * number of blades
    leaf_area = total_length * shoot_width * blade_number_tx)

# Summary of what we have
calc_summary <- length_analysis %>%
  summarise(
    total = n(),
    has_length = sum(!is.na(total_length)),
    has_sheath = sum(!is.na(sheath_length_num)),
    has_ratio = sum(!is.na(length_sheath_ratio)),
    has_leaf_area = sum(!is.na(leaf_area)))

print(calc_summary)

# Summary statistics for length/sheath ratio by site
ratio_summary_site <- length_analysis %>%
  filter(!is.na(length_sheath_ratio)) %>%
  group_by(site) %>%
  summarise(
    n = n(),
    mean_ratio = mean(length_sheath_ratio, na.rm = TRUE),
    sd_ratio = sd(length_sheath_ratio, na.rm = TRUE),
    median_ratio = median(length_sheath_ratio, na.rm = TRUE),
    .groups = 'drop')

print("Length/Sheath Ratio summary by site:")
print(ratio_summary_site)

# Summary statistics for leaf area by site
la_summary_site <- length_analysis %>%
  filter(!is.na(leaf_area)) %>%
  group_by(site) %>%
  summarise(
    n = n(),
    mean_LA = mean(leaf_area, na.rm = TRUE),
    sd_LA = sd(leaf_area, na.rm = TRUE),
    median_LA = median(leaf_area, na.rm = TRUE),
    .groups = 'drop')

print("Leaf Area (LA) summary by site:")
print(la_summary_site)

# Summary by site and treatment
ratio_summary_treatment <- length_analysis %>%
  filter(!is.na(length_sheath_ratio), !is.na(g_ng), g_ng %in% c("g", "ng")) %>%
  group_by(site, g_ng) %>%
  summarise(
    n = n(),
    mean_ratio = mean(length_sheath_ratio, na.rm = TRUE),
    sd_ratio = sd(length_sheath_ratio, na.rm = TRUE),
    .groups = 'drop')

print("Length/Sheath Ratio by site and treatment:")
print(ratio_summary_treatment)

la_summary_treatment <- length_analysis %>%
  filter(!is.na(leaf_area), !is.na(g_ng), g_ng %in% c("g", "ng")) %>%
  group_by(site, g_ng) %>%
  summarise(
    n = n(),
    mean_LA = mean(leaf_area, na.rm = TRUE),
    sd_LA = sd(leaf_area, na.rm = TRUE),
    .groups = 'drop')

print("Leaf Area by site and treatment:")
print(la_summary_treatment)

# Check what's in the data
length_analysis %>%
  select(total_length, sheath_length, sheath_length_num, length_sheath_ratio) %>%
  head(20)

# Check how many valid ratios we have
length_analysis %>%
  summarise(
    total_rows = n(),
    has_total_length = sum(!is.na(total_length)),
    has_sheath_length = sum(!is.na(sheath_length_num)),
    has_ratio = sum(!is.na(length_sheath_ratio)))

# Check the filtered data
ratio_data <- length_analysis %>%
  filter(!is.na(length_sheath_ratio), !is.na(g_ng), g_ng %in% c("g", "ng"))

print(nrow(ratio_data))
print(head(ratio_data))

# Plot length/sheath ratio by site, treatment, and timepoint
ratio_data <- length_analysis %>%
  filter(!is.na(length_sheath_ratio), !is.na(g_ng), g_ng %in% c("g", "ng", "none"), !is.na(collection_point))

# Calculate sample sizes and max values for positioning
n_labels_ratio <- ratio_data %>%
  group_by(site, g_ng, collection_point) %>%
  summarise(
    n = sum(!is.na(length_sheath_ratio)),
    max_val = max(length_sheath_ratio, na.rm = TRUE),
    .groups = "drop")

ggplot(ratio_data, aes(x = site, y = length_sheath_ratio, fill = g_ng)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, 
               position = position_dodge(width = 0.75)) +
  geom_point(aes(color = g_ng), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
             alpha = 0.6, size = 2) +
  geom_text(data = n_labels_ratio, 
            aes(x = site, y = max_val, label = paste0("n=", n)),
            position = position_dodge(width = 0.75),
            vjust = -0.5,
            size = 3) +
  facet_grid(~ collection_point, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = c("g" = "#ef4444", "ng" = "#3b82f6"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  scale_color_manual(values = c("g" = "#dc2626", "ng" = "#2563eb"),
                     labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                     name = "Treatment") +
  labs(title = "Length/Sheath Ratio by Site, Treatment, and Timepoint",
       x = "Site",
       y = "Length/Sheath Ratio") +
  scale_x_discrete(labels = c("donor" = "Donor", "high" = "High", "low" = "Low")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold"))

# Plot leaf area by site, treatment, and timepoint
la_data <- length_analysis %>%
  filter(!is.na(leaf_area), !is.na(g_ng), g_ng %in% c("g", "ng"), !is.na(collection_point))

# Calculate sample sizes and max values for positioning
n_labels_la <- la_data %>%
  group_by(site, g_ng, collection_point) %>%
  summarise(
    n = sum(!is.na(leaf_area)),
    max_val = max(leaf_area, na.rm = TRUE),
    .groups = "drop")

ggplot(la_data, aes(x = site, y = leaf_area, fill = g_ng)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, 
               position = position_dodge(width = 0.75)) +
  geom_point(aes(color = g_ng), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
             alpha = 0.6, size = 2) +
  geom_text(data = n_labels_la, 
            aes(x = site, y = max_val, label = paste0("n=", n)),
            position = position_dodge(width = 0.75),
            vjust = -0.5,
            size = 3) +
  facet_grid(~ collection_point, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = c("g" = "#ef4444", "ng" = "#3b82f6"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  scale_color_manual(values = c("g" = "#dc2626", "ng" = "#2563eb"),
                     labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                     name = "Treatment") +
  labs(title = "Leaf Area (Length × Width × Blade Number) by Site, Treatment, and Timepoint",
       x = "Site",
       y = "Leaf Area (cm²)") +
  scale_x_discrete(labels = c("donor" = "Donor", "high" = "High", "low" = "Low")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold"))


