library(tidyverse)
library(ggplot2)

#load data
data<- read.csv("raw_data/transplant_data.csv")

#filter for what I have right now
df_filtered <- data %>%
  filter(site != "", collection_point !="")

head(df_filtered)
str(df_filtered)

#clean up rgr
df_filtered <- df_filtered %>%
  mutate(rgr_perday = as.numeric(rgr_perday))

#group by site, g_ng and collection point to have quick look at extension and relative growth rate per day

ddf_summary <- df_filtered %>%
  group_by(site, g_ng, collection_point) %>%
  summarise(
    mean_extention = mean(extention, na.rm = TRUE),
    sd_extention = sd(extention, na.rm = TRUE),
    mean_rgr = mean(rgr_perday, na.rm = TRUE),
    n = n(),
    .groups = "drop")

view(ddf_summary)

library(ggplot2)

# Boxplot for extention quixck and ugly
ggplot(df_filtered, aes(x = collection_point, y = extention, fill = g_ng)) +
  geom_boxplot() +
  facet_wrap(~ site) +
  labs(
    title = "Extension by Collection Point, Genotype, and Site",
    x = "Collection Point",
    y = "Extension"
  ) +
  theme_minimal()

# Boxplot for rgr_perday quick and ugly
ggplot(df_filtered, aes(x = collection_point, y = rgr_perday, fill = g_ng)) +
  geom_boxplot() +
  facet_wrap(~ site) +
  labs(
    title = "RGR per Day by Collection Point, Genotype, and Site",
    x = "Collection Point",
    y = "RGR per Day"
  ) +
  theme_minimal()

