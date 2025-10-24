### LOAD PACKAGES
library(tidyverse)

#read in data
data<- read_csv("raw_data/plant_data.csv")

#peek at the data 
str(data)
head(data)
View(data)
names(data)


# First, let's see what values exist for collection_point and site
unique(data$collection_point)
unique(data$site)

# Count natural plants collected at T0
data %>%
  filter(
    !is.na(date_collected),           # Must have been collected
    tolower(collection_point) == "t0", # At T0 time point
    tolower(site) == "natural") %>%         # Natural site
  nrow()


# See all combinations of site and collection_point with counts
site_time_summary <- data %>%
  filter(!is.na(date_collected)) %>%
  count(site, collection_point) %>%
  arrange(site, collection_point)
site_time_summary

# Full summary with site, collection_point, and treatment
full_summary <- data %>%
  filter(!is.na(date_collected)) %>%
  count(site, collection_point, g_ng) %>%
  arrange(site, collection_point, g_ng)

print(full_summary)

# Alternative view: wider format for easier reading
full_summary_wide <- data %>%
  filter(!is.na(date_collected)) %>%
  count(site, collection_point, g_ng) %>%
  pivot_wider(
    names_from = g_ng,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(site, collection_point)

print(full_summary_wide)

# Calculate proportions collected at each site
# 60 galvinized (g) and 60 not galvanized (ng) planted at each experimental site

proportion_summary <- data %>%
  filter(!is.na(collection_point), 
         site %in% c("high", "low", "donor"),  # Only experimental sites
         collection_point %in% c("t1", "t2", "t3")) %>%
  count(site, g_ng) %>%
  mutate(
    planted = 60,
    proportion_collected = n / planted,
    percent_collected = (n / planted) * 100)

print(proportion_summary)

# By site and time point
proportion_by_time <- data %>%
  filter(!is.na(date_collected), 
         site %in% c("high", "low"),
         collection_point %in% c("t1", "t2", "t3")) %>%
  count(site, collection_point, g_ng) %>%
  mutate(
    planted = 60,
    proportion_collected = n / planted,
    percent_collected = (n / planted) * 100) %>%
  arrange(site, collection_point, g_ng)

print(proportion_by_time)

# Total by site (all time points combined)
total_by_site <- data %>%
  filter(!is.na(date_collected), 
         site %in% c("high", "low"),
         collection_point %in% c("t1", "t2", "t3")) %>%
  count(site, g_ng) %>%
  mutate(
    planted = 60,
    total_collected = n,
    proportion = n / planted,
    percent = (n / planted) * 100)
print(total_by_site)

# Remove non-numeric IDs, keep only numeric plant IDs (this is plants that keptt origonial tag)
data_numeric_only <- data %>%
  filter(!is.na(id), id != "", !grepl("[^0-9]", id))

# Check how many rows were removed
print(paste("Original rows:", nrow(data)))
print(paste("Rows with numeric IDs:", nrow(data_numeric_only)))
print(paste("Rows removed:", nrow(data) - nrow(data_numeric_only)))

# Verify the IDs are now all numeric
print("Sample of remaining IDs:")
print(head(data_numeric_only$id, 20))

# Now recalculate your proportion summary with cleaned data
proportion_by_time_clean <- data_numeric_only %>%
  filter(!is.na(date_collected), 
         site %in% c("high", "low", "donor"),
         collection_point %in% c("t1", "t2", "t3")) %>%
  count(site, collection_point, g_ng) %>%
  mutate(
    planted = 60,
    proportion_collected = n / planted,
    percent_collected = (n / planted) * 100) %>%
  arrange(site, collection_point, g_ng)

print(proportion_by_time_clean)

# Total by site (all time points combined)
total_by_site <- data_numeric_only %>%
  filter(!is.na(date_collected), 
         site %in% c("high", "low", "donor"),
         collection_point %in% c("t1", "t2", "t3")) %>%
  count(site, g_ng) %>%
  mutate(
    planted = 60,
    total_collected = n,
    proportion = n / planted,
    percent = (n / planted) * 100)
print(total_by_site)


# Create a plot to visualize this
# Filter out NA treatments and aggregate by site
proportion_plot_data <- proportion_by_time_clean %>%
  filter(!is.na(g_ng), g_ng != "NA") %>%
  group_by(site, g_ng) %>%
  summarise(
    n = sum(n),
    planted = 60,
    percent_collected = (n / planted) * 100,
    .groups = 'drop')

ggplot(proportion_plot_data, aes(x = site, y = percent_collected, fill = g_ng)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(aes(label = paste0(n, "/60")), 
            position = position_dodge(width = 0.9), 
            vjust = 1.5, 
            size = 3.5,
            color = "white",
            fontface = "bold") +
  geom_text(aes(label = paste0("(", round(percent_collected, 1), "%)")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  scale_fill_manual(values = c("g" = "#ef4444", "ng" = "#3b82f6"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  labs(title = "Plant Collection Success by Site and Treatment",
       subtitle = "Proportion of plants collected across three time points (n=60 planted per treatment per site)",
       x = "Site",
       y = "Percent Collected (%)") +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
  scale_x_discrete(labels = c("donor" = "Donor", "high" = "High", "low" = "Low")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "top",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank())

# Prepare data for time point visualization
time_point_data <- proportion_by_time_clean %>%
  filter(!is.na(g_ng), g_ng != "NA", 
         site %in% c("donor", "high", "low"),
         collection_point %in% c("t1", "t2", "t3"))

# Plot collection over time points
ggplot(time_point_data, aes(x = collection_point, y = percent_collected, fill = g_ng)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(aes(label = paste0(n, "/60")), 
            position = position_dodge(width = 0.9), 
            vjust = 1.5, 
            size = 3,
            color = "white",
            fontface = "bold") +
  geom_text(aes(label = paste0("(", round(percent_collected, 1), "%)")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 2.5) +
  facet_wrap(~ site, labeller = labeller(site = c("donor" = "Donor", 
                                                  "high" = "High", 
                                                  "low" = "Low"))) +
  scale_fill_manual(values = c("g" = "#ef4444", "ng" = "#3b82f6"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  labs(title = "Plant Collection Success Over Time by Site",
       subtitle = "Number collected at each time point (n=60 planted per treatment per site)",
       x = "Collection Time Point",
       y = "Percent Collected (%)") +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_x_discrete(labels = c("t1" = "T1", "t2" = "T2", "t3" = "T3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank())


# Calculate cumulative collection properly
# At T1: collect some plants
# At T2: collect from what remains (60 - T1 collected)
# At T3: collect from what remains (60 - T1 - T2 collected)

# Calculate cumulative collection properly
cumulative_collection <- proportion_by_time_clean %>%
  filter(!is.na(g_ng), g_ng != "NA", 
         site %in% c("donor", "high", "low"),
         collection_point %in% c("t1", "t2", "t3")) %>%
  arrange(site, g_ng, collection_point) %>%
  group_by(site, g_ng) %>%
  mutate(
    remaining = 60 - cumsum(lag(n, default = 0)),
    percent_of_remaining = (n / remaining) * 100
  ) %>%
  ungroup()

# Create a combined variable for time point and treatment for different colors
cumulative_collection <- cumulative_collection %>%
  mutate(
    time_treatment = paste0(collection_point, "_", g_ng)
  )

# Plot with different shades for each time point
ggplot(cumulative_collection, aes(x = collection_point, y = percent_of_remaining, fill = time_treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(aes(label = paste0(n, "/", remaining)), 
            position = position_dodge(width = 0.9), 
            vjust = 1.5, 
            size = 3,
            color = "white",
            fontface = "bold") +
  geom_text(aes(label = paste0("(", round(percent_of_remaining, 1), "%)")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 2.5) +
  facet_wrap(~ site, labeller = labeller(site = c("donor" = "Donor", 
                                                  "high" = "High", 
                                                  "low" = "Low"))) +
  scale_fill_manual(
    values = c(
      "t1_g" = "#fca5a5",    # Dark red for T1 galvanized
      "t1_ng" = "#93c5fd",   # Dark blue for T1 non-galvanized
      "t2_g" = "#f87171",    # Medium red for T2 galvanized
      "t2_ng" = "#60a5fa",   # Medium blue for T2 non-galvanized
      "t3_g" = "#dc2626",    # Light red for T3 galvanized
      "t3_ng" = "#2563eb"    # Light blue for T3 non-galvanized
    ),
    labels = c(
      "t1_g" = "T1 - Galvanized",
      "t1_ng" = "T1 - Non-Galvanized",
      "t2_g" = "T2 - Galvanized",
      "t2_ng" = "T2 - Non-Galvanized",
      "t3_g" = "T3 - Galvanized",
      "t3_ng" = "T3 - Non-Galvanized"),
    name = "Time Point & Treatment") +
  labs(title = "Plant Collection Success Over Time by Site",
       subtitle = "Percentage of remaining plants collected at each time point (started with n=60 per treatment per site)",
       x = "Collection Time Point",
       y = "Percent of Remaining Plants Collected (%)") +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
  scale_x_discrete(labels = c("t1" = "T1", "t2" = "T2", "t3" = "T3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank())

# Calculate what was missed (not collected)
missed_collection <- proportion_by_time_clean %>%
  filter(!is.na(g_ng), g_ng != "NA", 
         site %in% c("donor", "high", "low"),
         collection_point %in% c("t1", "t2", "t3")) %>%
  arrange(site, g_ng, collection_point) %>%
  group_by(site, g_ng) %>%
  summarise(
    total_collected = sum(n),
    total_missed = 60 - sum(n),
    percent_missed = (total_missed / 60) * 100,
    .groups = 'drop')

# Plot what was missed
ggplot(missed_collection, aes(x = site, y = percent_missed, fill = g_ng)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(aes(label = paste0(total_missed, "/60")), 
            position = position_dodge(width = 0.9), 
            vjust = 1.5, 
            size = 3.5,
            color = "white",
            fontface = "bold") +
  geom_text(aes(label = paste0("(", round(percent_missed, 1), "%)")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  scale_fill_manual(values = c("g" = "#dc2626", "ng" = "#2563eb"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  labs(title = "Plants Not Collected (Missed) by Site and Treatment",
       subtitle = "Total plants not collected across all three time points (n=60 planted per treatment per site)",
       x = "Site",
       y = "Percent Not Collected (%)") +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_x_discrete(labels = c("donor" = "Donor", "high" = "High", "low" = "Low")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "top",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank())

# Summary table of what was missed
print("Summary of Missed Collections:")
print(missed_collection)

# Plot what was missed
ggplot(missed_collection, aes(x = site, y = percent_missed, fill = g_ng)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(aes(label = paste0(total_missed, "/60")), 
            position = position_dodge(width = 0.9), 
            vjust = 1.5, 
            size = 3.5,
            color = "white",
            fontface = "bold") +
  geom_text(aes(label = paste0("(", round(percent_missed, 1), "%)")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  scale_fill_manual(values = c("g" = "#dc2626", "ng" = "#2563eb"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  labs(title = "Plants Not Collected (Missed) by Site and Treatment",
       subtitle = "Total plants not collected across all three time points (n=60 planted per treatment per site)",
       x = "Site",
       y = "Percent Not Collected (%)") +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_x_discrete(labels = c("donor" = "Donor", "high" = "High", "low" = "Low")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "top",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank())