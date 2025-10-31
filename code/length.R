# Analysis of collected plants with measurements
# Load libraries
library(tidyverse)
library(ggplot2)

data<- read_csv("raw_data/plant_data.csv")

# Remove non-numeric IDs, keep only numeric plant IDs
data_id_only <- data %>%
  filter(!is.na(id), id != "", !grepl("[^0-9]", id))

# Filter for collected plants with measurement values
collected_with_measurements <- data_id_only %>%
  filter(!is.na(date_collected)) %>%
  # Check which measurement columns have data
  mutate(
    has_length = !is.na(length_measured) | !is.na(length_calculated >0),
    has_weight = !is.na(shoot_weight >0) | !is.na(rhizome_weight >0),
    has_blade_measurements = !is.na(b1_full),
    has_epiphytes = !is.na(epi_weight) & epi_weight > 0)

# Summary of what measurements we have
measurement_summary <- collected_with_measurements %>%
  summarise(
    total_collected = n(),
    has_length = sum(has_length, na.rm = TRUE),
    has_weight = sum(has_weight, na.rm = TRUE),
    has_blade_measurements = sum(has_blade_measurements, na.rm = TRUE),
    has_epiphytes = sum(has_epiphytes, na.rm = TRUE))

print(measurement_summary)

# Look at key measurement variables
key_measurements <- collected_with_measurements %>%
  select(id, site, collection_point, g_ng, 
         length_measured, length_calculated, 
         shoot_weight, rhizome_weight,
         blade_number_tx, epi_weight,
         b1_full, b2_full, b3_full, b4_full)

print(head(key_measurements, 20))

# Analyze length of collected plants per site

# First, let's see what length data we have
length_data <- collected_with_measurements %>%
  filter(!is.na(site), site %in% c("donor", "high", "low", "natural")) %>%
  mutate(
    # Use length_measured if available, otherwise use length_calculated
    total_length = ifelse(!is.na(length_measured), length_measured, length_calculated)) %>%
  filter(!is.na(total_length))

print(paste("Plants with length measurements:", nrow(length_data)))
head(length_data)

# First, convert total_length to numeric (this will coerce any non-numeric values to NA)
length_data <- length_data %>%
  mutate(total_length = as.numeric(total_length))

# Then run your summary
length_summary_site <- length_data %>%
  group_by(site) %>%
  summarise(
    n = n(),
    mean_length = mean(total_length, na.rm = TRUE),
    sd_length = sd(total_length, na.rm = TRUE),
    median_length = median(total_length, na.rm = TRUE),
    min_length = min(total_length, na.rm = TRUE),
    max_length = max(total_length, na.rm = TRUE))
print(length_summary_site)

# Summary by site and treatment
length_summary_site_treatment <- length_data %>%
  filter(!is.na(g_ng), g_ng %in% c("g", "ng")) %>%
  group_by(site, g_ng) %>%
  summarise(
    n = n(),
    mean_length = mean(total_length, na.rm = TRUE),
    sd_length = sd(total_length, na.rm = TRUE),
    median_length = median(total_length, na.rm = TRUE),
    .groups = 'drop')

print(length_summary_site_treatment)

# Boxplot of length by site
ggplot(length_data, aes(x = site, y = total_length)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  labs(title = "Plant Length Distribution by Site",
       x = "Site",
       y = "Total Length (cm)") +
  scale_x_discrete(labels = c("donor" = "Donor", "high" = "High", 
                              "low" = "Low", "natural" = "Natural")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11))

# Boxplot by site and treatment
length_data_treatment <- length_data %>%
  filter(!is.na(g_ng), g_ng %in% c("g", "ng"))

ggplot(length_data_treatment, aes(x = site, y = total_length, fill = g_ng)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("g" = "#ef4444", "ng" = "#3b82f6"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  labs(title = "Plant Length Distribution by Site and Treatment",
       x = "Site",
       y = "Total Length (cm)") +
  scale_x_discrete(labels = c("donor" = "Donor", "high" = "High", "low" = "Low")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "top")

# Calculate sample sizes and max values for positioning
n_labels <- length_data_treatment %>%
  group_by(site, g_ng, collection_point) %>%
  summarise(
    n = sum(!is.na(total_length)),
    max_val = max(total_length, na.rm = TRUE),
    .groups = "drop")

# Create the plot with points and n values above
ggplot(length_data_treatment, aes(x = site, y = total_length, fill = g_ng)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(color = g_ng), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
             alpha = 0.6, size = 2) +
  geom_text(data = n_labels, 
            aes(x = site, y = max_val, label = paste0("n=", n)),
            position = position_dodge(width = 0.75),
            vjust = -0.5,
            size = 3) +
  facet_wrap(~collection_point, nrow = 1) +
  scale_fill_manual(values = c("g" = "#ef4444", "ng" = "#3b82f6"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  scale_color_manual(values = c("g" = "#dc2626", "ng" = "#2563eb"),
                     labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                     name = "Treatment") +
  labs(title = "Plant Length Distribution by Site, Treatment, and Timepoint",
       x = "Site",
       y = "Total Length (cm)") +
  scale_x_discrete(labels = c("donor" = "Donor", "high" = "High", "low" = "Low")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold"))


# Calculate length/sheath ratio and leaf area (LA)

# Prepare data with calculated metrics
length_analysis <- collected_with_measurements %>%
  filter(!is.na(site), site %in% c("donor", "high", "low", "natural")) %>%
  mutate(
    # Use length_measured if available, otherwise use length_calculated
    total_length = ifelse(!is.na(length_measured), length_measured, length_calculated),
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

print("Summary of calculated metrics:")
print(calc_summary)

# Filter for plants with valid measurements
valid_measurements <- length_analysis %>%
  filter(!is.na(length_sheath_ratio) | !is.na(leaf_area))

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

# Plot length/sheath ratio by site and treatment
ratio_data <- length_analysis %>%
  filter(!is.na(length_sheath_ratio), !is.na(g_ng), g_ng %in% c("g", "ng"))

ggplot(ratio_data, aes(x = site, y = length_sheath_ratio, fill = g_ng)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("g" = "#ef4444", "ng" = "#3b82f6"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  labs(title = "Length/Sheath Ratio by Site and Treatment",
       x = "Site",
       y = "Length/Sheath Ratio") +
  scale_x_discrete(labels = c("donor" = "Donor", "high" = "High", "low" = "Low")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "top")

# Plot leaf area by site and treatment
la_data <- length_analysis %>%
  filter(!is.na(leaf_area), !is.na(g_ng), g_ng %in% c("g", "ng"))

ggplot(la_data, aes(x = site, y = leaf_area, fill = g_ng)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("g" = "#ef4444", "ng" = "#3b82f6"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  labs(title = "Leaf Area (Length × Width × Blade Number) by Site and Treatment",
       x = "Site",
       y = "Leaf Area (cm²)") +
  scale_x_discrete(labels = c("donor" = "Donor", "high" = "High", "low" = "Low")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "top")

# Calculate length/sheath ratio and leaf area separated by timepoint

# Prepare data with calculated metrics
length_analysis <- collected_with_measurements %>%
  filter(!is.na(site), site %in% c("donor", "high", "low", "natural"),
         !is.na(collection_point), collection_point %in% c("t0", "t1", "t2", "t3")) %>%
  mutate(
    # Use length_measured if available, otherwise use length_calculated
    total_length = ifelse(!is.na(length_measured), length_measured, length_calculated),
    # Convert sheath_length to numeric (it's currently character)
    sheath_length_num = as.numeric(sheath_length),
    # Calculate length/sheath ratio
    length_sheath_ratio = total_length / sheath_length_num,
    # Calculate leaf area: length * width * number of blades
    leaf_area = total_length * shoot_width * blade_number_tx)

# Summary by site, treatment, and timepoint for length/sheath ratio
ratio_summary <- length_analysis %>%
  filter(!is.na(length_sheath_ratio), !is.na(g_ng), g_ng %in% c("g", "ng")) %>%
  group_by(site, g_ng, collection_point) %>%
  summarise(
    n = n(),
    mean_ratio = mean(length_sheath_ratio, na.rm = TRUE),
    sd_ratio = sd(length_sheath_ratio, na.rm = TRUE),
    median_ratio = median(length_sheath_ratio, na.rm = TRUE),
    .groups = 'drop'
  )

print("Length/Sheath Ratio by site, treatment, and timepoint:")
print(ratio_summary)

# Summary by site, treatment, and timepoint for leaf area
la_summary <- length_analysis %>%
  filter(!is.na(leaf_area), !is.na(g_ng), g_ng %in% c("g", "ng")) %>%
  group_by(site, g_ng, collection_point) %>%
  summarise(
    n = n(),
    mean_LA = mean(leaf_area, na.rm = TRUE),
    sd_LA = sd(leaf_area, na.rm = TRUE),
    median_LA = median(leaf_area, na.rm = TRUE),
    .groups = 'drop'
  )

print("Leaf Area by site, treatment, and timepoint:")
print(la_summary)

# Plot length/sheath ratio by site, treatment, and timepoint
ratio_data <- length_analysis %>%
  filter(!is.na(length_sheath_ratio), !is.na(g_ng), g_ng %in% c("g", "ng"))

ggplot(ratio_data, aes(x = collection_point, y = length_sheath_ratio, fill = g_ng)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ site, labeller = labeller(site = c("donor" = "Donor", 
                                                  "high" = "High", 
                                                  "low" = "Low",
                                                  "natural" = "Natural"))) +
  scale_fill_manual(values = c("g" = "#ef4444", "ng" = "#3b82f6"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  labs(title = "Length/Sheath Ratio Over Time by Site and Treatment",
       x = "Collection Time Point",
       y = "Length/Sheath Ratio") +
  scale_x_discrete(labels = c("t0" = "T0", "t1" = "T1", "t2" = "T2", "t3" = "T3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold"))

# Plot leaf area by site, treatment, and timepoint
la_data <- length_analysis %>%
  filter(!is.na(leaf_area), !is.na(g_ng), g_ng %in% c("g", "ng"))

ggplot(la_data, aes(x = collection_point, y = leaf_area, fill = g_ng)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ site, labeller = labeller(site = c("donor" = "Donor", 
                                                  "high" = "High", 
                                                  "low" = "Low",
                                                  "natural" = "Natural"))) +
  scale_fill_manual(values = c("g" = "#ef4444", "ng" = "#3b82f6"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  labs(title = "Leaf Area Over Time by Site and Treatment",
       x = "Collection Time Point",
       y = "Leaf Area (cm²)") +
  scale_x_discrete(labels = c("t0" = "T0", "t1" = "T1", "t2" = "T2", "t3" = "T3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold"))

