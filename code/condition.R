#load libraries
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
  filter(!is.na(condition_index) | !is.na(cut_index)) %>%
  filter(site %in% c("donor", "high", "low"))


# Calculate summary statistics
condition_summary <- growth_clean %>%
  group_by(site, collection_point, g_ng) %>%
  summarise(
    mean_condition = mean(condition_index, na.rm = TRUE),
    se_condition = sd(condition_index, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop")

p1 <- ggplot(condition_summary, 
             aes(x = collection_point, y = mean_condition, fill = g_ng)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_condition - se_condition, 
                    ymax = mean_condition + se_condition),
                position = position_dodge(width = 0.8),
                width = 0.25) +
  geom_text(aes(label = paste0("n=", n), y = 0.2),
            position = position_dodge(width = 0.8),
            size = 3) +
  facet_wrap(~ site) +
  labs(title = "Condition Index by Site, Treatment, and Collection Point",
       x = "Collection Point",
       y = "Mean Condition Index",
       fill = "Treatment") +
  theme_minimal()
plot(p1)

p1b <- condition_summary %>%
  filter(site %in% c("high", "low")) %>%
  ggplot(aes(x = collection_point, y = mean_condition, fill = g_ng)) +  # Remove the data argument
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_condition - se_condition, 
                    ymax = mean_condition + se_condition),
                position = position_dodge(width = 0.8),
                width = 0.25) +
  geom_text(aes(label = paste0("n=", n), y = 0.2),
            position = position_dodge(width = 0.8),
            size = 3) +
  facet_wrap(~ site) +
  labs(title = "Condition Index by Site, Treatment, and Collection Point",
       x = "Collection Point",
       y = "Mean Condition Index",
       fill = "Treatment") +
  theme_minimal()

plot(p1b)

condition_data <- growth_clean %>%
  filter(site %in% c("high", "low"), !is.na(condition_index))

# Three-way ANOVA
condition_anova <- aov(condition_index ~ g_ng * site * collection_point, 
                       data = condition_data)

summary(condition_anova)
# Kruskal-Wallis test (non-parametric alternative to ANOVA)

# Test for treatment effect
kruskal.test(condition_index ~ g_ng, data = condition_data)

# Test for site effect
kruskal.test(condition_index ~ site, data = condition_data)

# Test for collection point effect
kruskal.test(condition_index ~ collection_point, data = condition_data)


#Cut index
# cut_summary
cut_summary <- growth_clean %>%
  filter(!is.na(cut_index)) %>%
  group_by(site, collection_point, g_ng) %>%
  summarise(
    mean_cut_index = mean(cut_index, na.rm = TRUE),
    se_cut = sd(cut_index, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    # Ensure error bars don't go below 0 or above 2
    ymin = pmax(0, mean_cut_index - se_cut),
    ymax = pmin(2, mean_cut_index + se_cut)
  )

# Now plot with correct ymin and ymax
p2 <- ggplot(cut_summary, 
             aes(x = collection_point, y = mean_cut_index, fill = g_ng)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
                position = position_dodge(width = 0.8),
                width = 0.25) +
  geom_text(aes(label = paste0("n=", n), y = 0.1),
            position = position_dodge(width = 0.8),
            size = 3) +
  facet_wrap(~ site) +
  labs(title = "Cut Index by Site, Treatment, and Collection Point",
       x = "Collection Point",
       y = "Mean Cut Index",
       fill = "Treatment") +
  scale_y_continuous(limits = c(0, 2.2), breaks = c(0, 1, 2)) +
  theme_minimal()

plot(p2)


