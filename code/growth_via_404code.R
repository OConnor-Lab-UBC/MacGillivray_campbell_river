###### from BIOL402 Lab 4

# Getting seagrass data in shape for plotting -----------------------------------

### LOAD PACKAGES
library(tidyverse)
#install.packages("dplyr")
library(dplyr)
#install.packages("readr")
library(readr)
#install.packages("bit64")
install.packages("MuMIn")
library(MuMIn)
#read in data
growth<- read_csv("raw_data/Tansplant_data.csv")

growth <- growth %>%
  filter(!is.na(id))

#peek at the data 
str(growth)
head(growth)
View(growth)
names(growth)

#calculations, note standardization already done in excel standardized to sheath = sum(newleaves)/ sheath, standardized to old = sum new/ sum old
growth2 <- growth %>%
  mutate(
    sheath_length = as.numeric(gsub("[^0-9.]", "", sheath_length)),
    standardised_to_sheath = as.numeric(standardized_extension_to_sheath),
    standardised_to_old = as.numeric(`standardized_to_old`),
    LA = length_calculated * shoot_width * blade_number_tx) %>%
  group_by(site) %>%
  mutate(RGR = standardised_to_sheath / duration) %>%
  mutate(RGR_old = standardised_to_old / duration)%>%
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
  filter(collection_point %in% c("t1", "t2"), treatment %in% c("g", "ng"), (!is.na(site)))


# Boxplot for RGR  
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


ext_plot <- filtered %>%
  ggplot(aes(x = collection_point, y = standardised_to_sheath, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6
  ) +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass extension standardized tosheath",
    x = "Collection Point",
    y = "standardized extension") +
  theme_minimal()
plot(ext_plot)

# Boxplot for rgr - standardized to old leaf
rgr_plot2 <- filtered %>%
  ggplot(aes(x = collection_point, y = RGR_old, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6
  ) +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass relative growth rate standardized to old leafs",
    x = "Collection Point",
    y = "RGR") +
  theme_minimal()
plot(rgr_plot2)

#plant id 333 (row 53) is a large outlier that I dont have confidence in the measurment so I am removing here
rgr_clean <- filtered[-53, ]

rgr_plot2.2 <- rgr_clean %>%
  ggplot(aes(x = collection_point, y = RGR_old, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6
  ) +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass relative growth rate standardized to old leafs",
    x = "Collection Point",
    y = "RGR") +
  theme_minimal()
plot(rgr_plot2.2)

###GOING TO STICK TO STANDARDIZED TO SHEATH BECAUSE IT MAKES MORE SENSE! ##########

#boxplot of rgr only seperated by site and treatment as RGR has duration incorporated already
rgr_plot3 <- filtered %>%
  ggplot(aes(x = site, y = RGR, fill = treatment)) +
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

# Calculate n per group for label positioning

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

# boxplot and n values

# Plots with n's 
rgr_plot4 <- ggplot(rgr_clean, aes(x = site, y = RGR, fill = treatment)) +
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
plot(rgr_plot4)


rgr_plot5 <- rgr_clean %>%
  ggplot(aes(x = collection_point, y = RGR, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6) +
  geom_text(
    data = n_summary_rgr2,
    aes(x = collection_point, y = y_pos, label = paste0("n=", n), group = treatment),
    position = position_dodge(width = 0.75),
    vjust = 0,
    size = 3,
    inherit.aes = FALSE)+
  facet_wrap(~ site) +
  labs(
    title = "Seagrass relative growth rate",
    x = "Collection Point",
    y = "Relative Growth Rate (Extension standardized to sheath/ duration (days)") +
  theme_minimal()
plot(rgr_plot5)


#Index cut is 0= not cut, 1= cut, 2= shoot gont, 
cut_plot <- ggplot(rgr_clean, aes(x = cut_index, fill = treatment)) +
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
condition_plot <- ggplot(rgr_clean, aes(x = condition_index, fill = treatment)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ site) +
  labs(
    title = "Cut Index Counts by Treatment and Site",
    x = "condition_index",
    y = "Count"
  ) +
  theme_minimal()
plot(condition_plot)


#how many RGR of 0 or NA
rgr_summary <- rgr_clean %>%
  mutate(RGR_zero_or_na = is.na(RGR) | RGR == 0) %>%
  group_by(site, treatment) %>%
  summarise(count = sum(RGR_zero_or_na), .groups = "drop")

ggplot(rgr_summary, aes(x = treatment, y = count, fill = treatment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ site) +
  labs(
    title = "Count of RGR == 0 or NA per Treatment at Each Site",
    x = "Treatment",
    y = "Count of RGR == 0 or NA"
  ) +
  theme_minimal()

# STATS -------------------------------------------------------------
#just time point 2
rgr_clean2 <- filtered %>% 
  filter(!is.na(standardised_to_sheath)) %>%
  filter(treatment %in% c("ng", "g")) %>%
  filter(collection_point == "t2")

mod1 <- lm(standardised_to_sheath ~ treatment* site, data = rgr_clean2)
summary(mod1)

mod2 <- lm(standardised_to_sheath ~ site, data = rgr_clean2)
summary(mod2)

mod0 <- lm(standardised_to_sheath ~ 1, data = rgr_clean2)
summary(mod0)

mod3 <- lm(standardised_to_sheath ~ treatment + site, data = rgr_clean2)

model.sel(mod1, mod2, mod0, mod3)


rgr_clean3 <- filtered %>% 
  filter(!is.na(standardised_to_sheath)) %>%
  filter(treatment %in% c("ng", "g")) %>%
  filter(collection_point %in% c("t1", "t2"))

mod1 <- lm(standardised_to_sheath ~ treatment* site + collection_point, data = rgr_clean3)
summary(mod1)

mod2 <- lm(standardised_to_sheath ~ site+ collection_point, data = rgr_clean3)
summary(mod2)

mod2.5 <- lm(standardised_to_sheath ~ treatment + collection_point, data = rgr_clean3)
summary(mod2.5)

mod4 <- lm(standardised_to_sheath ~ collection_point, data = rgr_clean3)
summary(mod2.5)

mod0 <- lm(standardised_to_sheath ~ 1, data = rgr_clean3)
summary(mod0)

mod3 <- lm(standardised_to_sheath ~ treatment + site, data = rgr_clean3)

mod3.2 <- lm(standardised_to_sheath ~ treatment * collection_point, data = rgr_clean3)

model.sel(mod1, mod2, mod2.5, mod4, mod0, mod3, mod3.2)




