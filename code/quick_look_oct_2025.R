library(tidyverse)
library(ggplot2)

#load data
data<- read.csv("raw_data/plant_data.csv")

#filter for what I have right now
df_filtered <- data %>%
  filter(site != "", collection_point !="") %>%
  mutate(LA = length_calculated * shoot_width * blade_number_tx)

head(df_filtered)
str(df_filtered)

length_hist <- df_filtered %>%
  ggplot(aes(x=length_calculated,group=g_ng,fill=factor(g_ng),color=factor(g_ng))) + geom_histogram( binwidth=55)+theme_classic()+xlab("Seagrass length by treatment (mm)")
length_hist

#group by site, g_ng and collection point to have quick look at extension and relative growth rate per day
df_summary <- df_filtered %>%
  group_by(site, g_ng, collection_point) %>%
  summarise(
    mean_length = mean(length_measured, na.rm = TRUE),
    sd_length = sd(length_measured, na.rm = TRUE),
    mean_stdextention = mean(as.numeric(standardized_extension_to_sheath), na.rm = TRUE),
    sd_stextention = sd(as.numeric(standardized_extension_to_sheath), na.rm = TRUE),
    mean_extention = mean(extention, na.rm = TRUE),
    sd_extention = sd(extention, na.rm = TRUE),
    n = n(),
    .groups = "drop")

view(df_summary)

# Boxplot for extention quick and ugly
ggplot(df_filtered, aes(x = collection_point, y = extention, fill = g_ng)) +
  geom_boxplot() +
  facet_wrap(~ site) +
  labs(
    title = "Extension by Collection Point, Genotype, and Site",
    x = "Collection Point",
    y = "Extension") +
  theme_minimal()

#  Filter and clean the data for t1, t2 and g/ng only
df_plot <- df_filtered %>%
  mutate(
    collection_point = tolower(trimws(collection_point)),
    g_ng = tolower(trimws(g_ng))) %>%
  filter(collection_point %in% c("t1", "t2"), g_ng %in% c("g", "ng"))

# Boxplot for extention: filtered to T1, T2 and g/ng only
df_plot %>%
  ggplot(aes(x = collection_point, y = extention, fill = g_ng)) +
  geom_boxplot() +
  facet_wrap(~ site) +
  labs(
    title = "Extension by Collection Point (T1 & T2), washer type (g & ng), and Site",
    x = "Collection Point",
    y = "Extension") +
  theme_minimal()


# Calculate n per group for label positioning
n_summary <- df_plot %>%
  group_by(site, collection_point, g_ng) %>%
  summarise(
    n = n(),
    y_pos = max(extention, na.rm = TRUE) * 1.05,  # Position label just above the box
    .groups = "drop")

# boxplot and n values
ggplot(df_plot, aes(x = collection_point, y = extention, fill = g_ng)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_text(
    data = n_summary,
    aes(label = paste0("n=", n), y = y_pos),
    position = position_dodge(width = 0.75),
    vjust = 0, size = 3
  ) +
  facet_wrap(~ site) +
  labs(
    title = "Extension by Collection Point (T1 & T2), treatment (g & ng), and Site",
    x = "Collection Point",
    y = "Extension") +
  theme_minimal()

# boxplot and n values for LA
df_plot2 <- df_filtered %>%
  mutate(
    collection_point = tolower(trimws(collection_point)),
    g_ng = tolower(trimws(g_ng))) %>%
  filter(collection_point %in% c("t0", "t1", "t2", "t3"), g_ng %in% c("g", "ng"))

ggplot(df_plot2, aes(x = collection_point, y = LA, fill = g_ng)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_text(
    data = n_summary,
    aes(label = paste0("n=", n), y = y_pos),
    position = position_dodge(width = 0.75),
    vjust = 0, size = 3) +
  facet_wrap(~ site) +
  labs(
    title = "Extension by Collection Point (T1 & T2), treatment (g & ng), and Site",
    x = "Collection Point",
    y = "LA") +
  theme_minimal()

# Boxplot for extention standardized quick and ugly
ggplot(df_summary, aes(x = collection_point, y = mean_stdextention, fill = g_ng)) +
  geom_boxplot() +
  facet_wrap(~ site) +
  labs(
    title = "Standardized extension to sheath by Collection Point, Genotype, and Site",
    x = "Collection Point",
    y = "standardized extension") +
  theme_minimal()

#filtered for T1 and T2 rgr_perday
df_plot %>%
  ggplot(aes(x = collection_point, y = rgr_perday, fill = g_ng)) +
  geom_boxplot() +
  facet_wrap(~ site) +
  labs(
    title = "Growth by Collection Point (T1 & T2), Genotype (g & ng), and Site",
    x = "Collection Point",
    y = "relative growth per day") +
  theme_minimal()

# Calculate n per group for label positioning
n_summary <- df_plot %>%
  group_by(site, collection_point, g_ng) %>%
  summarise(
    n = n(),
    y_pos = max(rgr_perday, na.rm = TRUE) * 1.05,  # Position label just above the box
    .groups = "drop")

# boxplot and n values
ggplot(df_plot, aes(x = collection_point, y = rgr_perday, fill = g_ng)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_text(
    data = n_summary,
    aes(label = paste0("n=", n), y = y_pos),
    position = position_dodge(width = 0.75),
    vjust = 0, size = 3
  ) +
  facet_wrap(~ site) +
  labs(
    title = "relative growth per day by Collection Point (T1 & T2), treatment (g & ng), and Site",
    x = "Collection Point",
    y = "growth per day") +
  theme_minimal()
