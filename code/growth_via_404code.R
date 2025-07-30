###### from BIOL402 Lab 4

# Getting seagrass data in shape for plotting -----------------------------------

### LOAD PACKAGES
library(tidyverse)
#install.packages("dplyr")
library(dplyr)
#install.packages("readr")
library(readr)
#install.packages("bit64")

#read in data
growth<- read_csv("raw_data/Tansplant.csv")


#peek at the data 
str(growth)
head(growth)
View(growth)

#calculation, with standardized to extention and sheath, sheath beacause many leaves were cut
growth2 <- growth %>%
  mutate(
    LA = (length_calculated * shoot_width * blade_number_tx),
    sheath_length = as.numeric(sheath_length)) %>%  
  group_by(site) %>%
  mutate(
    new_leaves_sum = rowSums(across(b1_new:b10_new), na.rm = TRUE),
    full_leaves_sum = rowSums(across(b1_full:b10_full), na.rm = TRUE),
    standardized_leaf_extension = new_leaves_sum / full_leaves_sum,
    standardized_to_sheath = new_leaves_sum / sheath_length,
    RGR = standardized_leaf_extension / duration) %>%
  mutate(rgr_sheath = standardized_to_sheath / duration) %>%
  ungroup()

head(growth2) #check to see that the new growth columns have been added

# Making plots -------------------------------------------------------

## Most of the plots in this code chunk make use of the package ggplot2(), which integrates nicely with the other tidyverse package. You can load it individually with:
library(ggplot2)

# Let's say we simply want to get a sense of the distribution of maximum shoot lengths among the clipped vs unclipped treatments. A histogram with colour codes for the two treatments would do:

growth2 <- growth2%>%
  mutate(treatment = g_ng)

length_hist <- growth2 %>%
  ggplot(aes(x=length_calculated,group=treatment,fill=factor(treatment),color=factor(treatment))) + geom_histogram( binwidth=55)+theme_classic()+xlab("Seagrass length by treatment (mm)")

# Note binwith changes the size of x axis bins thereby making the bars narrower or wider

# Fill and color arguments mean you want to group your data based on values in that column (in this case, treatment) . 

plot(length_hist)
print(plot(length_hist))
dev.new()

#  Filter and clean the data for only data I have 
filtered<- growth2 %>%
  mutate(collection_point = tolower(trimws(collection_point)),
    treatment = tolower(trimws(treatment))) %>%
  filter(collection_point %in% c("t1", "t2", "t0"), treatment %in% c("g", "ng", "none"), (!is.na(site)))


# Boxplot for RGR - remember this is extention and with filtered data 
rgr_plot <- filtered %>%
  ggplot(aes(x = collection_point, y = RGR, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6
  ) +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass relative growth rate",
    x = "Collection Point",
    y = "RGR") +
  theme_minimal()
plot(rgr_plot)

# Boxplot for rgr - remember this is standardized to sheath and with filtered data 
rgr_plot2 <- filtered %>%
  ggplot(aes(x = collection_point, y = rgr_sheath, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6
  ) +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass relative growth rate",
    x = "Collection Point",
    y = "RGR") +
  theme_minimal()
plot(rgr_plot2)

#boxplot of rgr only seperated by site and treatment as RGR has duration incorporated already
rgr_plot3 <- filtered %>%
  ggplot(aes(x = site, y = rgr_sheath, fill = treatment)) +
  geom_boxplot()+
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6
  ) +
  labs(
    title = "Seagrass relative growth rate",
    x = "Collection Point",
    y = "RGR") +
  theme_minimal()
plot(rgr_plot3)

#the black line indicates the mean RGR value for each group. It appears the two groups do not appear to differ significantly...
#you see some differences when separated by time point. Thinking of what this means ..
#Looking at rgr_plot2 growth slowed down at 2 weeks, maybe seeing slight difference in ng/g in t2 high and t1 low

# Boxplot for extension: filtered for current data
extention_plot <- filtered %>%
  ggplot(aes(x = collection_point, y = standardized_leaf_extension, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6
  ) +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass standardized leaf extention",
    x = "Collection Point",
    y = "Leaf extension") +
  theme_minimal()
plot(extention_plot)

extention_plot_sheath <- filtered %>%
  ggplot(aes(x = collection_point, y = standardized_to_sheath, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6
  ) +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass standardized leaf extention",
    x = "Collection Point",
    y = "RGR") +
  theme_minimal()
plot(extention_plot_sheath)

#looks like between looking at RGR and extension the plants grew the most in week 1 and slowed growth alot in week 2
#donor t1 and high t2 may be showing difference in treatment, although ns are very differnt and not much diffence so idk. 

#Index cut is 0= not cut, 1= cut, 2= shoot gont, 
cut_plot <- ggplot(filtered, aes(x = cut_index, fill = treatment)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ site) +
  labs(
    title = "Cut Index Counts by Treatment and Site",
    x = "Cut Index",
    y = "Count"
  ) +
  theme_minimal()

plot(cut_plot)

#condition is 0-5
condition_plot <- ggplot(filtered, aes(x = condition_index, fill = treatment)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ site) +
  labs(
    title = "Cut Index Counts by Treatment and Site",
    x = "condition_index",
    y = "Count"
  ) +
  theme_minimal()
plot(condition_plot)

# Calculate n per group for label positioning
rgr_clean <- filtered %>% 
  filter(collection_point %in% c("t1", "t2"), treatment %in% c("g", "ng"))
#grouped all together
n_summary_rgr <- rgr_clean %>%
  group_by(site, treatment) %>%
  summarise(
    n = n(),
    y_pos = max(RGR, na.rm = TRUE) + 0.05,  # Position label just above the box
    .groups = "drop")

#seperatied by collection point
n_summary_rgr2<- rgr_clean %>%
  group_by(site, collection_point, treatment) %>%
  summarise(
    n = n(),
    y_pos = max(RGR, na.rm = TRUE) +0.03,  # Position label just above the box
    .groups = "drop")

#standardized to sheath
#grouped all together
n_summary_rgr3 <- rgr_clean %>%
  group_by(site, treatment) %>%
  summarise(
    n = n(),
    y_pos = max(rgr_sheath, na.rm = TRUE) + 0.05,  # Position label just above the box
    .groups = "drop")

#seperatied by collection point
n_summary_rgr4<- rgr_clean %>%
  group_by(site, collection_point, treatment) %>%
  summarise(
    n = n(),
    y_pos = max(rgr_sheath, na.rm = TRUE) +0.03,  # Position label just above the box
    .groups = "drop")

#extension
ext_clean <- growth2 %>%
  filter(!is.na(standardized_leaf_extension), !is.na(site), !is.na(collection_point), !is.na(treatment))

n_summary_ext<- ext_clean %>%
  group_by(site, collection_point, treatment) %>%
  summarise(
    n = n(),
    y_pos = max(standardized_leaf_extension, na.rm = TRUE) +0.03,  # Position label just above the box
    .groups = "drop")
#standardized to sheath
n_summary_ext2<- ext_clean %>%
  group_by(site, collection_point, treatment) %>%
  summarise(
    n = n(),
    y_pos = max(standardized_to_sheath, na.rm = TRUE) +0.03,  # Position label just above the box
    .groups = "drop")

# boxplot and n values

# Plots with n's 
rgr3 <- ggplot(rgr_clean, aes(x = site, y = RGR, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6
  ) +
  geom_text(
    data = n_summary_rgr,
    aes(x = site, y = y_pos, label = paste0("n=", n), group = treatment),
    position = position_dodge(width = 0.75),
    vjust = 0,
    size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    title = "RGR",
    x = "Site",
    y = "RGR"
  ) +
  theme_minimal()
plot(rgr3)

rgr3.b <- ggplot(rgr_clean, aes(x = collection_point, y = RGR, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6) +
  facet_wrap(~site)+
  geom_text(
    data = n_summary_rgr2,
    aes(x = collection_point, y = y_pos, label = paste0("n=", n), group = treatment),
    position = position_dodge(width = 0.75),
    vjust = 0,
    size = 3,
    inherit.aes = FALSE)+
  labs(
    title = "RGR",
    x = "Site",
    y = "RGR") +
  theme_minimal()
plot(rgr3.b)

rgr4 <- ggplot(rgr_clean, aes(x = site, y = rgr_sheath, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6
  ) +
  geom_text(
    data = n_summary_rgr3,
    aes(x = site, y = y_pos, label = paste0("n=", n), group = treatment),
    position = position_dodge(width = 0.75),
    vjust = 0,
    size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    title = "rgr standardized to sheath",
    x = "Site",
    y = "RGR"
  ) +
  theme_minimal()
plot(rgr4)

rgr4.b <- ggplot(rgr_clean, aes(x = collection_point, y = rgr_sheath, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6) +
  facet_wrap(~site)+
  geom_text(
    data = n_summary_rgr4,
    aes(x = collection_point, y = y_pos, label = paste0("n=", n), group = treatment),
    position = position_dodge(width = 0.75),
    vjust = 0,
    size = 3,
    inherit.aes = FALSE)+
  labs(
    title = "rgr standardized to sheath",
    x = "Site",
    y = "RGR") +
  theme_minimal()
plot(rgr4.b)

#just extentsion
ext2 <- ggplot(ext_clean, 
               aes(x = collection_point, y = standardized_leaf_extension, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6
  ) +
  facet_wrap(~site) +
  geom_text(
    data = n_summary_ext,
    aes(x = collection_point, y = y_pos, label = paste0("n=", n), group = treatment),
    position = position_dodge(width = 0.75),
    vjust = 0,
    size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    title = "standardized_leaf_extension by Site and Collection Point",
    x = "Collection Point",
    y = "standardized_leaf_extension"
  ) +
  theme_minimal()
plot(ext2)

#standardized to sheath
ext3 <- ggplot(ext_clean, 
               aes(x = collection_point, y = standardized_to_sheath, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6
  ) +
  facet_wrap(~site) +
  geom_text(
    data = n_summary_ext2,
    aes(x = collection_point, y = y_pos, label = paste0("n=", n), group = treatment),
    position = position_dodge(width = 0.75),
    vjust = 0,
    size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    title = "standardized leaf extension to sheath by Site and Collection Point",
    x = "Collection Point",
    y = "standardized_leaf_extension"
  ) +
  theme_minimal()
plot(ext3)


# Regressions -------------------------------------------------------------
ext_clean_anova <- filtered %>% 
  filter(!is.na(standardized_to_sheath))
anova_model <- aov(standardized_to_sheath ~ treatment * collection_point * site, data = filtered)
summary(anova_model)

par(mfrow = c(2, 2))
plot(anova_model)  # residual plots

anova_model2 <- aov(RGR ~ treatment * collection_point * site, data = rgr_clean)
summary(anova_model)

par(mfrow = c(2, 2))
plot(anova_model2)  # residual plots

anova_model3 <- aov(rgr_sheath ~ treatment * collection_point * site, data = rgr_clean)
summary(anova_model3)

par(mfrow = c(2, 2))
plot(anova_model3)  # residual plots
