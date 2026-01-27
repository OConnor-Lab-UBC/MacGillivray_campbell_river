library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(Matrix)
library(lme4)


#load data
growth<- read_csv("raw_data/plant_data.csv")
head(growth)

cond <- read.csv("raw_data/photo_data.csv")
head(cond)

# Join growth with selected columns from cond
growth_combined <- growth %>%
  left_join(cond %>% select(id, condition_index, cut_index), by = "id")

# Check the result
head(growth_combined)
colnames(growth_combined)

# Remove rows where both condition_index and cut_index are NA
growth_clean <- growth_combined %>%
  filter(!is.na(collection_point), !is.na(g_ng)) %>%
  filter(site %in% c("donor", "high", "low"))

# Find duplicated IDs
duplicates <- growth_clean$id[duplicated(growth_clean$id)]
duplicates


# See all combinations of site and collection_point with counts
site_time_summary <- growth_clean %>%
  filter(!is.na(date_collected)) %>%
  count(site, collection_point) %>%
  arrange(site, collection_point)
site_time_summary

# Full summary with site, collection_point, and treatment
full_summary <- growth_clean %>%
  filter(!is.na(date_collected)) %>%
  count(site, collection_point, g_ng) %>%
  pivot_wider(
    names_from = g_ng,
    values_from = n,
    values_fill = 0) %>%
  arrange(site, collection_point)
print(full_summary)

# Calculate proportions collected at each site
# 60 galvinized (g) and 60 not galvanized (ng) planted at each experimental site

# Total by site (all time points combined)
total_by_site <- growth_clean %>%
  filter(!is.na(date_collected), !is.na(g_ng),
         site %in% c("high", "low", "donor"),
         collection_point %in% c("t1", "t2", "t3")) %>%
  count(site, g_ng) %>%
  mutate(
    planted = 60,
    total_collected = n,
    proportion = n / planted,
    percent = (n / planted) * 100) 
print(total_by_site)


# what about measurments
collection_summary <- growth_clean %>%
  group_by(site, g_ng) %>%
  summarise(
    n_collected = n(),
    n_with_length = sum(!is.na(length_measured)),
    n_rhizome_only = sum(length_measured == 0),
    .groups = "drop")
print(collection_summary)

#donor site had many problems so we will just look at High and Low

# Survival based on condition index > 2
total_collected <- growth_clean %>%
  filter(!is.na(date_collected), !is.na(g_ng),
         site %in% c("high", "low", "donor"),
         collection_point %in% c("t1", "t2", "t3")) %>%
  group_by(site, g_ng) %>%
  summarise(
    total_collected = n(),
    cond_survived = sum(condition_index > 2, na.rm = TRUE),
    collected_poor = sum(condition_index <= 2, na.rm = TRUE),
    with_shoots = sum(!is.na(length_measured) & length_measured > 0, na.rm = TRUE),
    rhizome_only = sum((is.na(length_measured) | length_measured == 0) & 
                         (!is.na(rhizome_weight) | !is.na(rhizome_length)), 
                       na.rm = TRUE),
    .groups = "drop") %>%
  mutate(
    not_collected = 60- total_collected, # there may be missing conditon photos so we will look purly at ones we have condition for for this
    not_survived = collected_poor + not_collected,
    survived_porportion = cond_survived / 60,
    survival_rate = survived_porportion * 100,
    not_survived_rate = (not_survived / 60) * 100,
    with_shoot_rate = (with_shoots / 60) * 100,
    collection_rate = (total_collected / 60) * 100)

print(total_collected)

#Survival plot
survival_plot <- ggplot(total_collected, 
                        aes(x = site, y = survival_rate, fill = g_ng)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(survival_rate, 1), "%\n(", 
                               cond_survived, "/60)")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.5) +
  labs(title = "Plant Survival Rate by Site and Treatment",
       subtitle = "Survival defined as condition index > 2",
       x = "Site",
       y = "Survival Rate (%)",
       fill = "Treatment") +
  ylim(0, 110) +
  theme_minimal()

plot(survival_plot)

# Filter out donor site and reshape data for stacked bar chart
# Filter donor
survival <- total_collected %>%
  filter(site != "donor")
survival

# --- Survival vs Mortality (100%) ---
survival_simple <- survival %>%
  mutate(
    mortality = 60 - cond_survived,  # all others are mortality
    survived_prop = cond_survived / 60 * 100,
    mortality_prop = mortality / 60 * 100) %>%
  select(site, g_ng, survived_prop, mortality_prop) %>%
  pivot_longer(cols = c(survived_prop, mortality_prop),
               names_to = "status", values_to = "proportion") %>%
  mutate(
    status = factor(status,
                    levels = c("mortality_prop", "survived_prop"),
                    labels = c("Mortality", "Survived")))

ggplot(survival_simple,
       aes(x = g_ng, y = proportion, fill = status)) +
  geom_col(width = 0.7, position = "stack") +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~ site) +
  scale_fill_manual(values = c("Mortality" = "#D32F2F",
                               "Survived" = "#2196F3")) +
  scale_x_discrete(labels = c("g" = "Galvanized", "ng" = "Not Galvanized")) +
  labs(title = "Overall Survival vs Mortality by Site and Treatment",
       subtitle = "100% scale — survival defined as condition index > 2",
       x = "Treatment", y = "Proportion (%)",
       fill = "Outcome") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

# Add NA condition category
survival_detailed <- survival %>%
  mutate(
    na_condition = 60 - (cond_survived + collected_poor + not_collected)) %>%
  select(site, g_ng, cond_survived, collected_poor, not_collected, na_condition) %>%
  pivot_longer(
    cols = c(cond_survived, collected_poor, not_collected, na_condition),
    names_to = "status",
    values_to = "count") %>%
  mutate(
    proportion = count / 60 * 100,
    status = factor(
      status,
      levels = c("na_condition", "not_collected", "collected_poor", "cond_survived"),
      labels = c("Condition NA", "Not Collected", "Poor Condition", "Survived")))

ggplot(survival_detailed,
       aes(x = g_ng, y = proportion, fill = status)) +
  geom_col(width = 0.7, position = "stack") +
  geom_text(aes(label = paste0(round(proportion, 1), "%\n(", count, "/60)")),
            position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~ site) +
  scale_fill_manual(values = c(
    "Condition NA" = "#CCCCCC",
    "Not Collected" = "#D32F2F",
    "Poor Condition" = "grey60",
    "Survived" = "#2196F3"
  )) +
  scale_x_discrete(labels = c("g" = "Galvanized", "ng" = "Not Galvanized")) +
  labs(title = "Detailed Plant Status by Site and Treatment",
       subtitle = "Survival, poor condition, not collected, and missing condition data",
       x = "Treatment", y = "Proportion (%)",
       fill = "Status") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))


# Background data (one per site × treatment)
bg_data <- survival %>%
  distinct(site, g_ng) %>%
  mutate(bg_color = ifelse(g_ng == "g", "#FF9999", "#99D6D6"))


# Long format for stacking
survival_long <- survival %>%
  select(site, g_ng, cond_survived, collected_poor, not_collected) %>%
  pivot_longer(cols = c(cond_survived, collected_poor, not_collected),
               names_to = "status", values_to = "count") %>%
  mutate(
    proportion = count / 60 * 100,
    status = factor(status,
                    levels = c("not_collected", "collected_poor", "cond_survived"),
                    labels = c("Not Collected", "Poor Condition", "Survived")))
survival_long

# Plot
survival_stacked_plot <- ggplot() +
  # Background color rectangles per treatment × site
  geom_rect(data = bg_data,
            aes(xmin = as.numeric(factor(g_ng)) - 0.4,
                xmax = as.numeric(factor(g_ng)) + 0.4,
                ymin = -Inf, ymax = Inf,
                fill = bg_color),
            inherit.aes = FALSE,
            alpha = 0.3) +
  # Actual stacked bars
  geom_col(data = survival_long,
           aes(x = g_ng, y = proportion, fill = status),
           width = 0.7, position = position_stack()) +
  # Labels
  geom_text(data = survival_long,
            aes(x = g_ng, y = proportion, label = paste0(round(proportion, 1), "%\n(", count, "/60)")),
            position = position_stack(vjust = 0.5), size = 3) +
  # Facet by site
  facet_wrap(~ site) +
  # Colors and labels
  scale_fill_manual(values = c("Not Collected" = "#D32F2F",
                               "Poor Condition" = "grey",
                               "Survived" = "#2196F3")) +
  scale_x_discrete(labels = c("g" = "Galvinized", "ng" = "Not Galvinized")) +
  labs(title = "Plant Survival and Mortality by Site and Treatment",
       subtitle = "Survival = condition index > 2; failure = not collected or condition ≤ 2",
       x = "Treatment",
       y = "Proportion (%)",
       fill = "Status") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        strip.background = element_rect(fill = NA, color = NA),
        strip.text = element_text(face = "bold", size = 11))

print(survival_stacked_plot)

# GLM with binomial family (logistic regression)
# Testing for effects of site, treatment, and their interaction
mod1 <- glm(cbind(cond_survived, collected_poor + not_collected) ~ 
              site * g_ng,
            data = survival,
            family = binomial(link = "logit"))

# View results
summary(mod1)

# Test significance of factors
Anova(mod1, type = "III")

# Check model fit
1 - pchisq(mod1$deviance, mod1$df.residual)  # p > 0.05 indicates good fit


mod2 <- glm(cbind(cond_survived, collected_poor + not_collected) ~ g_ng,
            data = survival,
            family = binomial(link = "logit"))
summary(mod2)

# Test significance of factors
Anova(mod2, type = "III")

# Check model fit
1 - pchisq(mod2$deviance, mod1$df.residual)  # p > 0.05 indicates good fit



#____
# Calculate survival data
survival_data <- growth_clean %>%
  filter(!is.na(date_collected), !is.na(g_ng),
         site %in% c("high", "low", "donor"),
         collection_point %in% c("t1", "t2", "t3")) %>%
  group_by(site, g_ng) %>%
  summarise(
    cond_survived = sum(condition_index >= 2, na.rm = TRUE),
    total_collected = n(),
    .groups = "drop"
  ) %>%
  mutate(
    not_survived = 60 - cond_survived,
    survived_pct = (cond_survived / 60) * 100,
    not_survived_pct = (not_survived / 60) * 100
  )

# Create the plot
survival_plot <- ggplot(survival_data, 
                        aes(x = site, y = survived_pct, fill = g_ng)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  geom_hline(yintercept = 60, linetype = "dotted", color = "gray20", linewidth = 1) +
  geom_text(aes(label = paste0(cond_survived, "/60")), 
            position = position_dodge(width = 0.9), 
            vjust = 1.5, 
            size = 3.5,
            color = "white",
            fontface = "bold") +
  geom_text(aes(label = paste0("(", round(survived_pct, 1), "%)")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  scale_fill_manual(values = c("g" = "#F8766D", "ng" = "#00BFC4"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  labs(title = "Plant Survival by Site and Treatment",
       subtitle = "Survival based on condition index ≥2 (n=60 planted per treatment per site)",
       x = "Site",
       y = "Survival Rate (%)") +
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
    panel.grid.minor = element_blank()
  )

plot(survival_plot)

# Alternative: Show both survived and not survived side by side
survival_comparison <- survival_data %>%
  select(site, g_ng, cond_survived, not_survived) %>%
  pivot_longer(
    cols = c(cond_survived, not_survived),
    names_to = "status",
    values_to = "count"
  ) %>%
  mutate(
    percentage = (count / 60) * 100,
    status = factor(status, 
                    levels = c("cond_survived", "not_survived"),
                    labels = c("Survived", "Not Survived"))
  )

comparison_plot <- ggplot(survival_comparison, 
                          aes(x = site, y = count, fill = g_ng)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5,
            fontface = "bold") +
  facet_wrap(~ status) +
  scale_fill_manual(values = c("g" = "#F8766D", "ng" = "#00BFC4"),
                    labels = c("g" = "Galvanized", "ng" = "Non-Galvanized"),
                    name = "Treatment") +
  labs(title = "Plant Survival Status by Site and Treatment",
       subtitle = "Survival = condition index ≥2, out of 60 planted per treatment per site",
       x = "Site",
       y = "Number of Plants") +
  scale_x_discrete(labels = c("donor" = "Donor", "high" = "High", "low" = "Low")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold")
  )

plot(comparison_plot)