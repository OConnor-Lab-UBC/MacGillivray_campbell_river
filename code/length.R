# Analysis of collected plants with measurements
# Load libraries
library(tidyverse)
library(ggplot2)

data<- read_csv("raw_data/plant_data.csv")

# Remove non-numeric IDs, keep only numeric plant IDs
data_id_only <- data %>%
  filter(!is.na(id), id != "", !grepl("[^0-9]", id))

data_natural <- data %>%
  filter(site == "natural")

# Filter for collected plants with measurement values
collected_with_measurements <- data_id_only %>%
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
collected_with_measurements <- bind_rows(collected_with_measurements, collected_natural)

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
# Create length data including natural shoots
length_data_all <- collected_with_measurements %>%
  filter(!is.na(site), site %in% c("donor", "high", "low", "natural")) %>%
  mutate(
    total_length = as.numeric(ifelse(!is.na(length_measured), length_measured, length_calculated))) %>%
  filter(!is.na(total_length))

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

##Add reference of natural plants
# Now create the treatment comparison with natural reference
# Create length data with proper filtering
length_data_with_reference <- length_data_all %>%
  filter(!is.na(collection_point)) %>%
  mutate(
    treatment_group = case_when(
      g_ng == "g" ~ "Galvanized",
      g_ng == "ng" ~ "Non-Galvanized",
      g_ng == "natural" | site == "natural" ~ "Reference (Natural)",
      TRUE ~ "Other"
    )
  ) %>%
  filter(treatment_group != "Other") %>%
  # Remove treatment groups from t0 (keep only natural)
  filter(!(collection_point == "t0" & site != "natural")) %>%
  # Remove the single non-treatment observation from high site at t2
  filter(!(collection_point == "t2" & site == "high" & treatment_group == "Reference (Natural)"))

# Calculate sample sizes and max values for positioning
n_labels <- length_data_with_reference %>%
  group_by(site, treatment_group, collection_point) %>%
  summarise(
    n = sum(!is.na(total_length)),
    max_val = max(total_length, na.rm = TRUE),
    .groups = "drop"
  )

# Create the plot
# Create the plot with fixed width boxplots
# Create the plot with consistent box widths
# Create the plot with consistent box widths using facet_grid
ggplot(length_data_with_reference, aes(x = site, y = total_length, fill = treatment_group)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, 
               position = position_dodge(width = 0.75)) +
  geom_point(aes(color = treatment_group), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
             alpha = 0.6, size = 2) +
  geom_text(data = n_labels, 
            aes(x = site, y = max_val, label = paste0("n=", n)),
            position = position_dodge(width = 0.75),
            vjust = -0.5,
            size = 3) +
  facet_grid(~ collection_point, scales = "free_x", space = "free_x") +
  scale_fill_manual(
    values = c("Galvanized" = "#ef4444", 
               "Non-Galvanized" = "#3b82f6",
               "Reference (Natural)" = "#22c55e"),
    name = "Treatment") +
  scale_color_manual(
    values = c("Galvanized" = "#dc2626", 
               "Non-Galvanized" = "#2563eb",
               "Reference (Natural)" = "#16a34a"),
    name = "Treatment") +
  labs(title = "Plant Length Distribution by Site, Treatment, and Timepoint",
       x = "Site",
       y = "Total Length (cm)") +
  scale_x_discrete(labels = c("donor" = "Donor", "high" = "High", "low" = "Low", "natural" = "Natural")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold"))