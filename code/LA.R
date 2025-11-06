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
collected

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


# Only looking at High and low sight and natural for refrence LA!

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
  scale_fill_manual(values = c("g" = "#F8766D", "ng" = "#00BFC4"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  scale_color_manual(values = c("g" = "#F8766D", "ng" = "#00BFC4"),
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

mod1 <- lm(leaf_area ~ g_ng, data = la_data)
mod1a <- lm(leaf_area ~ g_ng * site, data = la_data)
summary(mod1a)
mod1b <- lm(leaf_area ~ site, data = la_data)

mod2 <- lm(leaf_area ~ collection_point, data = la_data)
mod2a<- lm(leaf_area ~ g_ng * collection_point * site, data = la_data)
summary(mod2a)

mod3a <-lm(leaf_area ~ g_ng * collection_point, data = la_data)
summary(mod3a)

mod4a <-lm(leaf_area ~ site * collection_point, data = la_data)
summary(mod4a)

# Compare  models
model.sel(mod1a, mod2a, mod3a, mod4a, mod1, mod1b, mod2)
#model 4a and 3a top w2 within 2 AIC units
summary(mod4a)
summary(mod3a)


#Three-way ANOVA
# Testing effects of treatment, site, and collection_point on RGR
model_anova <- aov(leaf_area ~ g_ng * site * collection_point, 
                   data = la_data)
model.sel(mod1a, mod2a, mod3a, mod4a, mod1, mod1b, mod2, model_anova)
#model anova within 2 AIC units of top but 4th ranked

# Check assumptions
par(mfrow = c(2, 2))
plot(model_anova)

# Levene's test
leveneTest(leaf_area ~ g_ng * site * collection_point, 
           data = la_data)

# For Kruskal-Wallis test (non-parametric)
# Test treatment effect
kruskal.test(leaf_area ~ g_ng, data = la_data)

# Test site effect
kruskal.test(leaf_area ~ site, data = la_data)

# Test collection point effect
kruskal.test(leaf_area ~ collection_point, data = la_data)




