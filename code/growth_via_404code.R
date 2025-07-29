###### from BIOL402 Lab 4

# Getting seagrass data in shape for plotting -----------------------------------

### LOAD PACKAGES
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)
install.packages("bit64")

#read in data
growth<- read_csv("raw_data/Tansplant_data.csv")


#peek at the data 
str(growth)
head(growth)
View(growth)

#calculation
growth2 <- growth %>%
  mutate(LA = (length_calculated * shoot_width * blade_number_tx)) %>%
  group_by(site) %>%
  mutate(
    new_leaves_sum = rowSums(across(b1_new:b10_new), na.rm = TRUE),
    full_leaves_sum = rowSums(across(b1_full:b10_full), na.rm = TRUE),
    standardized_leaf_extension = new_leaves_sum / full_leaves_sum,
    RGR = standardized_leaf_extension / duration) %>%
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

#Now let's compare growth rates among the two treatments with a boxplot

RGR_boxplot <- growth2 %>%
  ggplot(aes(x=treatment,y=RGR,group=treatment,fill=factor(treatment))) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~site)+
  xlab("Seagrass relative growth rate")

plot(RGR_boxplot)

#  Filter and clean the data for t1, t2 and g/ng only
filtered <- growth2 %>%
  mutate(
    collection_point = tolower(trimws(collection_point)),
    treatment = tolower(trimws(treatment))) %>%
  filter(collection_point %in% c("t1", "t2", "t0"), treatment %in% c("g", "ng", "none"))

# Boxplot for RGR: filtered to T1, T2 and g/ng only seperated by site and time point
filtered_plot <- filtered %>%
  ggplot(aes(x = collection_point, y = RGR, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass relative growth rate",
    x = "Collection Point",
    y = "RGR") +
  theme_minimal()
plot(filtered_plot)

#boxplot of RGR only seperated by site and treatment as RGR has duration incorporated already
RGR_plot2 <- filtered %>%
  ggplot(aes(x = site, y = RGR, fill = treatment)) +
  geom_boxplot()+
  labs(
    title = "Seagrass relative growth rate",
    x = "Collection Point",
    y = "RGR") +
  theme_minimal()
plot(RGR_plot2)

#the black line indicates the mean RGR value for each group. It appears the two groups do not appear to differ significantly...
#you see some differences when seperated by time point. Thinking of what this means .. looking at extention may help here

# Boxplot for extention: filtered for curent data
extention_plot <- filtered %>%
  ggplot(aes(x = collection_point, y = standardized_leaf_extension, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass standardized leaf extention",
    x = "Collection Point",
    y = "RGR") +
  theme_minimal()
plot(extention_plot)

#looks like between looking at RGR and extension the plants grew the most in week 1 and slowed growth alot in week 2
#donor t1 and high t2 may be showing difference in treatment, although ns are very differnt and not much diffence so idk. 

# Calculate n per group for label positioning
rgr_clean <- growth2 %>% filter(!is.na(RGR))%>%
  filter(collection_point %in% c("t1", "t2"), treatment %in% c("g", "ng"))

n_summary_rgr <- rgr_clean %>%
  group_by(site, treatment) %>%
  summarise(
    n = n(),
    y_pos = max(RGR, na.rm = TRUE) + 0.05,  # Position label just above the box
    .groups = "drop")

n_summary_rgr2<- rgr_clean %>%
  group_by(site, collection_point, treatment) %>%
  summarise(
    n = n(),
    y_pos = max(RGR, na.rm = TRUE) +0.03,  # Position label just above the box
    .groups = "drop")

ext_clean <- growth2 %>%
  filter(!is.na(standardized_leaf_extension), !is.na(site), !is.na(collection_point), !is.na(treatment))

n_summary_ext<- ext_clean %>%
  group_by(site, collection_point, treatment) %>%
  summarise(
    n = n(),
    y_pos = max(standardized_leaf_extension, na.rm = TRUE) +0.03,  # Position label just above the box
    .groups = "drop")

# boxplot and n values

library(ggplot2)
library(dplyr)

# Remove NAs
growth2_clean <- growth2 %>% filter(!is.na(RGR))

# Plots with n's 
rgr3 <- ggplot(rgr_clean, aes(x = site, y = RGR, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
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

rgr4 <- ggplot(rgr_clean, aes(x = collection_point, y = RGR, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  facet_wrap(~site)+
  geom_text(
    data = n_summary_rgr2,
    aes(x = collection_point, y = y_pos, label = paste0("n=", n), group = treatment),
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
plot(rgr4)


ext2 <- ggplot(ext_clean, 
               aes(x = collection_point, y = standardized_leaf_extension, fill = treatment)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
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

#donor t1 and high t2 may be showing difference in treatment, although n's are very different and not much difference so idk. 

# Regressions -------------------------------------------------------------

