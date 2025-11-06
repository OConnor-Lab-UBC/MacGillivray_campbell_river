library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

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

# First, let's understand what was collected
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
    cond_survived = sum(condition_index >= 2, na.rm = TRUE),
    collected_poor = sum(condition_index < 2, na.rm = TRUE),
    with_shoots = sum(!is.na(length_measured) & length_measured > 0, na.rm = TRUE),
    rhizome_only = sum((is.na(length_measured) | length_measured == 0) & 
                         (!is.na(rhizome_weight) | !is.na(rhizome_length)), 
                       na.rm = TRUE),
    .groups = "drop") %>%
  mutate(
    not_collected = 60 - total_collected,
    not_survived = collected_poor + not_collected,
    survival_rate = (cond_survived / 60) * 100,
    not_survived_rate = (not_survived / 60) * 100,
    with_shoot_rate = (with_shoots / 60) * 100,
    collection_rate = (total_collected / 60) * 100)

print(total_collected)





#Survival plot
survival_plot <- ggplot(total_collected, 
                        aes(x = site, y = survival_rate, fill = g_ng)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(survival_rate, 1), "%\n(", 
                               survived, "/60)")),
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
