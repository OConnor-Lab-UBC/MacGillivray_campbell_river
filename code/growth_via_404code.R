###### from BIOL402 Lab 4

# Getting seagrass data in shape for plotting -----------------------------------

### LOAD PACKAGES
library(tidyverse)
#install.packages("dplyr")
library(dplyr)
#install.packages("readr")
library(readr)
#install.packages("bit64")
#install.packages("MuMIn")
library(MuMIn)
#read in data
growth<- read_csv("raw_data/plant_data.csv")

growth <- growth %>%
  filter(!is.na(id))

#peek at the data 
str(growth)
head(growth)
View(growth)
names(growth)

growth2 <- growth %>%
  mutate(
    sheath_length = as.numeric(gsub("[^0-9.]", "", sheath_length)),
    duration = as.numeric(duration_days),  # Also need to convert duration
    # Remove Excel errors and convert to numeric
    stndrd_to_sheath = as.numeric(ifelse(standardized_extension_to_sheath == "#DIV/0!", NA, standardized_extension_to_sheath)),
    stndrd_to_old = as.numeric(ifelse(standardized_to_old == "#DIV/0!", NA, standardized_to_old)),
    LA = length_measured * shoot_width * blade_number_tx) %>%
  group_by(site) %>%
  mutate(
    RGR = stndrd_to_sheath / duration,  # Use the abbreviated name you created
    RGR_old = stndrd_to_old / duration) %>% # Use the abbreviated name you created
  ungroup()

head(growth2) #check to see that the new growth columns have been added

#clean to data that is there
growth3<- growth2 %>%
  filter(!is.na(date_collected), 
         site %in% c("high", "low", "donor", "natural"),
         collection_point %in% c("t1", "t2", "t3", "t0"))#,
         #length_measured > 0)
view(growth3)

# Making plots -------------------------------------------------------

## Most of the plots in this code chunk make use of the package ggplot2(), which integrates nicely with the other tidyverse package. You can load it individually with:
library(ggplot2)

# Let's say we simply want to get a sense of the distribution of maximum shoot lengths among the clipped vs unclipped treatments. A histogram with colour codes for the two treatments would do:

growth3 <- growth3%>%
  mutate(treatment = g_ng)

length_hist <- growth3 %>%
  ggplot(aes(x = as.numeric(length_calculated), 
             group = treatment, 
             fill = factor(treatment), 
             color = factor(treatment))) + 
  geom_histogram(binwidth = 15) +
  theme_classic() +
  xlab("Seagrass length by treatment (mm)")
length_hist

#Note we have 0 nflated data! but otherwise normal
# Fill and color arguments mean you want to group your data based on values in that column (in this case, treatment) . 


#  Filter and clean the data for only T1 and T2 as well as refrence as those will be the ones with growth
t1t2<- growth3 %>%
  mutate(collection_point = tolower(trimws(collection_point)),
    treatment = tolower(trimws(treatment))) %>%
  filter(collection_point %in% c("t1", "t2"), treatment %in% c("g", "ng"), (!is.na(site)))


# Boxplot for RGR  
rgr_plot <- t1t2 %>%
  ggplot(aes(x = collection_point, y = RGR, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6) +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass relative growth rate",
    x = "Collection Point",
    y = "RGR") +
  theme_minimal()
plot(rgr_plot)

# Check the distribution of site and treatment combinations
growth3 %>%
  count(site, g_ng, collection_point) %>%
  arrange(site, collection_point, g_ng)

# Or get a clearer overview
table(growth3$site, growth3$g_ng)


ext_plot <- t1t2 %>%
  ggplot(aes(x = collection_point, y = stndrd_to_sheath, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6
  ) +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass extension standardized to sheath",
    x = "Collection Point",
    y = "standardized extension") +
  theme_minimal()
plot(ext_plot)

# Boxplot for rgr - standardized to old leaf
rgr_plot2 <- t1t2 %>%
  ggplot(aes(x = collection_point, y = RGR_old, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6) +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass relative growth rate standardized to old leafs",
    x = "Collection Point",
    y = "RGR") +
  theme_minimal()
plot(rgr_plot2)

#plant id 333 (row 53) is a large outlier that I dont have confidence in the measurment so I am removing here
rgr_clean <- t1t2[-53, ]

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
rgr_plot3 <- t1t2 %>%
  ggplot(aes(x = site, y = RGR, fill = treatment)) +
  geom_boxplot()+
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6) +
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
    alpha = 0.6) +
  geom_text(
    data = n_summary_rgr,
    aes(x = site, y = y_pos, label = paste0("n=", n), group = treatment),
    position = position_dodge(width = 0.75),
    vjust = 0,
    size = 3,
    inherit.aes = FALSE) +
  labs(
    title = "RGR",
    x = "Site",
    y = "RGR") +
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
#cut_plot <- ggplot(rgr_clean, aes(x = cut_index, fill = treatment)) +
 # geom_bar(position = "dodge") +
 # facet_wrap(~ site) +
 # labs(
 #   title = "Cut Index Counts by Treatment and Site",
  #  x = "Cut Index",
  #  y = "Count") +
#theme_minimal()

#plot(cut_plot)

#condition is 0-5
#condition_plot <- ggplot(rgr_clean, aes(x = condition_index, fill = treatment)) +
 # geom_bar(position = "dodge") +
  #facet_wrap(~ site) +
#  labs(
#    title = "Cut Index Counts by Treatment and Site",
#    x = "condition_index",
#    y = "Count") +
#  theme_minimal()
#plot(condition_plot)


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
    y = "Count of RGR == 0 or NA") +
  theme_minimal()

# STATS -------------------------------------------------------------
#just time point 2
rgr_clean2 <- t1t2 %>% 
  filter(!is.na(standardised_to_sheath)) %>%
  filter(treatment %in% c("ng", "g")) %>%
  filter(collection_point == "t2")

mod1 <- lm(stndrd_to_sheath ~ treatment* site, data = rgr_clean)
summary(mod1)

mod2 <- lm(stndrd_to_sheath ~ site, data = rgr_clean)
summary(mod2)

mod0 <- lm(stndrd_to_sheath ~ 1, data = rgr_clean)
summary(mod0)

mod3 <- lm(stndrd_to_sheath ~ treatment + site, data = rgr_clean)

model.sel(mod1, mod2, mod0, mod3)


rgr_clean3 <- t1t2%>% 
  filter(!is.na(stndrd_to_sheath)) %>%
  filter(treatment %in% c("ng", "g")) %>%
  filter(collection_point %in% c("t1", "t2"))

mod1 <- lm(stndrd_to_sheath ~ treatment* site + collection_point, data = rgr_clean3)
summary(mod1)

mod2 <- lm(stndrd_to_sheath ~ site+ collection_point, data = rgr_clean3)
summary(mod2)

mod2.5 <- lm(stndrd_to_sheath ~ treatment + collection_point, data = rgr_clean3)
summary(mod2.5)

mod4 <- lm(stndrd_to_sheath ~ collection_point, data = rgr_clean3)
summary(mod4)

mod0 <- lm(stndrd_to_sheath ~ 1, data = rgr_clean3)
summary(mod0)

mod3 <- lm(stndrd_to_sheath ~ treatment + site, data = rgr_clean3)

mod3.2 <- lm(stndrd_to_sheath ~ treatment * collection_point, data = rgr_clean3)

model.sel(mod1, mod2, mod2.5, mod4, mod0, mod3, mod3.2)




