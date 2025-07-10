library(tidyverse)
library(ggplot2)

#load data
data<- read.csv("raw_data/Tansplant_data.csv")

#filter for what I have right now
df_filtered <- df_filtered %>%
  mutate(shoot_weight = as.numeric(shoot_weight),
    rhizome_weight = as.numeric(rhizome_weight))

df_filtered <- data %>%
  filter(site != "", collection_point !="") %>%
  filter(shoot_weight  >= 0) %>%
  filter(rhizome_weight >= 0)

head(df_filtered)
str(df_filtered)

df_filtered <- df_filtered %>%
  mutate(shoot_weight = as.numeric(shoot_weight),
         rhizome_weight = as.numeric(rhizome_weight))

#group by site, g_ng and collection point to have quick look at biomass and s
ddf_summary <- df_filtered %>%
  group_by(site, g_ng, collection_point) %>%
  summarise(
    mean_shoot_wieght = mean(shoot_weight, na.rm = TRUE),
    sd_shoot_wieght = sd(shoot_weight, na.rm = TRUE),
    mean_rhizome_wieght = mean(rhizome_weight, na.rm = TRUE),
    sd_shoot_wieght = sd(rhizome_weight, na.rm = TRUE),
    n = n(),.groups = "drop")

view(ddf_summary)

#  Filter and clean the data for t0, t1, t2 and g/ng/none only
df_plot <- df_filtered %>%
  mutate(
    collection_point = tolower(trimws(collection_point)),
    g_ng = tolower(trimws(g_ng))) %>%
  filter(collection_point %in% c("t0", "t1", "t2"), g_ng %in% c("g", "ng", "none"))

# Boxplot for shoot wieght quick and ugly
ggplot(df_plot, aes(x = collection_point, y = shoot_weight, fill = g_ng)) +
  geom_boxplot() +
  facet_wrap(~ site) +
  labs(
    title = "shoot wieght",
    x = "Collection Point",
    y = "weight g") +
  theme_minimal()

# Boxplot for rhizome wieght quick and ugly
ggplot(df_plot, aes(x = collection_point, y = rhizome_weight, fill = g_ng)) +
  geom_boxplot() +
  facet_wrap(~ site) +
  labs(
    title = "rhizome wieght",
    x = "Collection Point",
    y = "weight g") +
  theme_minimal()
