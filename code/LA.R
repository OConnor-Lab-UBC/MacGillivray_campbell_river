#load libraries
library(tidyverse)
library(ggplot2)
library(gt)
library(emmeans)  
#install.packages("emmeans")
#install.packages("gt")

data<- read_csv("raw_data/plant_data.csv")

# Remove non-numeric IDs, keep only numeric plant IDs
#data_id_only <- data %>%
#  filter(!is.na(id), id != "", !grepl("[^0-9]", id))

#data_natural <- data %>%
#  filter(site == "natural")

# Filter for collected plants with measurement values
collected <- data %>%
  filter(!is.na(date_collected)) %>%
  # Check which measurement columns have data
  mutate(
    has_length = !is.na(length_measured) | !is.na(length_calculated >0),
    has_weight = !is.na(shoot_weight >0) | !is.na(rhizome_weight >0),
    has_blade_measurements = !is.na(b1_full),
    has_epiphytes = !is.na(epi_weight) & epi_weight > 0)

# Process natural shoots similarly
#collected_natural <- data_natural %>%
#  filter(!is.na(date_collected)) %>%
#  mutate(
#    has_length = !is.na(length_measured) | !is.na(length_calculated),
#    has_weight = !is.na(shoot_weight) | !is.na(rhizome_weight),
#    has_blade_measurements = !is.na(b1_full),
#    has_epiphytes = !is.na(epi_weight) & epi_weight > 0,
#    g_ng = "natural")  # Ensure natural shoots are marked

# Combine both datasets
#collected_with_measurements <- bind_rows(collected_natural, collected)

#filter out for ones with site and total length measurements
length_data_all <- collected %>%
  filter(!is.na(site), site %in% c("donor", "high", "low", "natural")) %>%
  mutate(
    total_length = as.numeric(ifelse(!is.na(length_measured), length_measured, length_calculated)),
    sheath_length = as.numeric(sheath_length),
    # Calculate length/sheath ratio
    length_sheath_ratio = total_length / sheath_length,
    # Calculate leaf area: length * width * number of blades
    leaf_area = total_length * shoot_width * blade_number_tx) %>%
  filter(!is.na(total_length))
head(length_data_all)

# Summary of what we have
calc_summary <- length_data_all %>%
  summarise(
    total = n(),
    has_length = sum(!is.na(total_length)),
    has_sheath = sum(!is.na(sheath_length)),
    has_ratio = sum(!is.na(length_sheath_ratio)),
    has_leaf_area = sum(!is.na(leaf_area)))
print(calc_summary)


#-------LA------------------------------

la_summary_treatment <- length_data_all %>%
  filter(!is.na(leaf_area), !is.na(g_ng), g_ng %in% c("g", "ng", "none")) %>%
  group_by(site, g_ng, collection_point) %>%
  summarise(
    n = n(),
    mean_LA = mean(leaf_area, na.rm = TRUE),
    sd_LA = sd(leaf_area, na.rm = TRUE),
    .groups = 'drop')
print(la_summary_treatment)


# Only looking at High and low sight LA!

la_data <- length_data_all %>%
  filter(!is.na(leaf_area), !is.na(g_ng), g_ng %in% c("g", "ng", "none"), !is.na(collection_point), site %in% c("high", "low", "natural"))


# Calculate sample sizes and max values for positioning
n_labels_la <- la_data %>%
  group_by(site, g_ng, collection_point) %>%
  summarise(
    n = sum(!is.na(leaf_area)),
    max_val = max(leaf_area, na.rm = TRUE),
    .groups = "drop")

ggplot(la_data,  aes(x = site, y = leaf_area, fill = g_ng)) +
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
  labs(title = "Leaf Area (Length × Width × Blade Number)\nby Site, Treatment, and Timepoint",
       x = "Site",
       y = "Leaf Area (cm²)") +
  scale_x_discrete(labels = c("high" = "High", "low" = "Low")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold"))
str(la_data)

mod1a <- lm(leaf_area ~ g_ng * site, data = la_data)
summary(mod1a)

mod2a<- lm(leaf_area ~ g_ng * collection_point * site, data = la_data)
summary(mod2a)

emm <- emmeans(mod2a, ~ g_ng | collection_point * site)
emm_t3 <- contrast(emm, method = "pairwise", by = c("collection_point", "site"))
emm_t3 <- summary(emm_t3)

mod3a <-lm(leaf_area ~ g_ng * collection_point, data = la_data)
summary(mod3a)

mod4a <-lm(leaf_area ~ site * collection_point, data = la_data)
summary(mod4a)

# Compare  models
model.sel(mod1a, mod2a, mod3a, mod4a)


# 3. Three-way ANOVA
# Testing effects of treatment, site, and collection_point on RGR
model_anova <- aov(leaf_area ~ g_ng * site * collection_point, 
                   data = la_data)

# Check assumptions
par(mfrow = c(2, 2))
plot(model_anova)

# Levene's test for homogeneity of variance
leveneTest(leaf_area ~ g_ng * site * collection_point, 
           data = la_data)

# Results
summary(model_anova)

# Kruskal-Wallis tests
kruskal.test(RGR ~ treatment, data = growtht_clean)
kruskal.test(RGR ~ site, data = growtht_clean)
kruskal.test(RGR ~ collection_point, data = growtht_clean)

# Separate analyses by factor
# Effect of treatment
wilcox.test(RGR ~ treatment, data = growtht_clean)

# Effect by site
growtht_clean %>%
  group_by(site) %>%
  summarise(
    p_value = wilcox.test(RGR ~ treatment)$p.value,
    .groups = "drop")

# Effect by site and collection point
growtht_clean %>%
  group_by(site, collection_point) %>%
  summarise(
    p_value = wilcox.test(RGR ~ treatment)$p.value,
    n_g = sum(treatment == "g"),
    n_ng = sum(treatment == "ng"),
    .groups = "drop")





#-----------legth sheath ratio---------------------
# Summary statistics for length/sheath ratio by site
ratio_summary_site <- length_data_all %>%
  filter(!is.na(length_sheath_ratio)) %>%
  group_by(site) %>%
  summarise(
    n = n(),
    mean_ratio = mean(length_sheath_ratio, na.rm = TRUE),
    sd_ratio = sd(length_sheath_ratio, na.rm = TRUE),
    median_ratio = median(length_sheath_ratio, na.rm = TRUE),
    .groups = 'drop')
print(ratio_summary_site)

# Summary statistics for leaf area by site
la_summary_site <- length_data_all %>%
  filter(!is.na(leaf_area)) %>%
  group_by(site) %>%
  summarise(
    n = n(),
    mean_LA = mean(leaf_area, na.rm = TRUE),
    sd_LA = sd(leaf_area, na.rm = TRUE),
    median_LA = median(leaf_area, na.rm = TRUE),
    .groups = 'drop')
print(la_summary_site)

# Summary by site and treatment and time point 
ratio_summary_treatment <- length_data_all %>%
  filter(!is.na(length_sheath_ratio), !is.na(g_ng), g_ng %in% c("g", "ng", "none")) %>%
  group_by(site, g_ng, collection_point) %>%
  summarise(
    n = n(),
    mean_ratio = mean(length_sheath_ratio, na.rm = TRUE),
    sd_ratio = sd(length_sheath_ratio, na.rm = TRUE),
    .groups = 'drop')
print(ratio_summary_treatment)

#print and save nice table for documents
ratio_summary_treatment %>%
  mutate(
    g_ng = case_when(
      g_ng == "g" ~ "Galvanized",
      g_ng == "ng" ~ "Non-Galvanized",
      g_ng == "none" ~ "Reference",
      TRUE ~ g_ng),
    site = str_to_title(site)) %>%
  gt() %>%
  tab_header(
    title = "Length/Sheath Ratio Summary",
    subtitle = "By Site, Treatment, and Timepoint") %>%
  cols_label(
    site = "Site",
    g_ng = "Treatment",
    collection_point = "Timepoint",
    n = "N",
    mean_ratio = "Mean Ratio",
    sd_ratio = "SD") %>%
  fmt_number(
    columns = c(mean_ratio, sd_ratio),
    decimals = 2) %>%
  gtsave("outputs/ratio_summary_table.html")

# Check how many valid ratios we have
length_data_all %>%
  summarise(
    total_rows = n(),
    has_total_length = sum(!is.na(total_length)),
    has_sheath_length = sum(!is.na(sheath_length)),
    has_ratio = sum(!is.na(length_sheath_ratio)))

#stats
#Treatment × Timepoint (pooling across sites)
mod1 <- lm(leaf_area ~ g_ng* site, data = length_data_all)
summary(mod1)

# Check assumptions
par(mfrow = c(2, 2))
plot(mod1)
par(mfrow = c(1, 1))
summary(mod1)

# Treatment × Timepoint × Site
mod2 <- lm(leaf_area ~ g_ng * collection_point * site, data = length_data_all)

# Check assumptions
par(mfrow = c(2, 2))
plot(mod2)
par(mfrow = c(1, 1))

#check that model 2 is better
summary(mod2)
model.sel(mod1, mod2)

# Plot length/sheath ratio by site, treatment, and timepoint
ratio_data <- length_data_all %>%
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

# Plot length/sheath ratio by site, treatment, and timepoint
ratio_data <- length_data_all %>%
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
