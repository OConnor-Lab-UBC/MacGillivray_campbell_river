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
library(ggplot2)

growth<- read_csv("raw_data/plant_data.csv")

#peek at the data 
str(growth)
head(growth)
View(growth)
names(growth)

#growthid <- growth %>%
 # filter(!is.na(id))

growth2 <- growth %>%
  mutate(
    sheath_length = as.numeric(sheath_length),
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

#clean to data 
growth3<- growth2 %>%
  mutate(treatment = g_ng) %>%
  filter(!is.na(date_collected), 
         site %in% c("high", "low", "donor", "natural"),
         collection_point %in% c("t1", "t2", "t3", "t0"))
view(growth3)

# Making plots -------------------------------------------------------


# Let's say we simply want to get a sense of the distribution of maximum shoot lengths among the clipped vs unclipped treatments. A histogram with colour codes for the two treatments would do:

length_hist <- growth3 %>%
  ggplot(aes(x = as.numeric(length_calculated), 
             group = treatment, 
             fill = factor(treatment), 
             color = factor(treatment))) + 
  geom_histogram(binwidth = 15) +
  theme_classic() +
  xlab("Seagrass length by treatment (mm)")
length_hist

# Check the RGR data by treatment
growth3 %>%
  group_by(treatment) %>%
  summarise(
    total = n(),
    na_values = sum(is.na(RGR)),
    inf_values = sum(is.infinite(RGR), na.rm = TRUE),
    valid_values = sum(is.finite(RGR)),
    mean_RGR = mean(RGR, na.rm = TRUE),
    median_RGR = median(RGR, na.rm = TRUE))

# Create histogram for RGR distribution (only g and ng treatments)
RGR_hist <- growth3 %>%
  filter(is.finite(RGR), 
         !is.na(treatment), 
         treatment %in% c("g", "ng")) %>%  # Only galvanized and non-galvanized
  ggplot(aes(x = RGR, 
             fill = factor(treatment), 
             color = factor(treatment))) + 
  geom_histogram(binwidth = 0.05, alpha = 0.6, position = "identity") +
  theme_classic()
RGR_hist


#  Filter and clean the data for only T1 and T2 and T3 as those will be the ones with RGR
growtht<- growth3 %>%
  filter(collection_point %in% c("t1", "t2", "t3"), treatment %in% c("g", "ng"), (!is.na(site)), !is.na(RGR))
unique(growtht$collection_point)

# Boxplot for RGR  
# n_labels from the filtered growtht data
n_labels <- growtht %>%
  group_by(site, collection_point, treatment) %>%
  summarise(
    n = n(),
    max_rgr = suppressWarnings(max(RGR, na.rm = TRUE)),
    .groups = "drop") %>%
  mutate(
    y_pos = if_else(is.infinite(max_rgr), 0.05, max_rgr + 0.05))

# Recreate plot
rgr_plot <- growtht %>%
  ggplot(aes(x = collection_point, y = RGR, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6) +
  geom_text(
    data = n_labels,
    aes(
      x = collection_point,
      y = y_pos,
      label = paste0("n=", n),
      group = treatment),
    position = position_dodge(width = 0.75),
    size = 3,
    vjust = -1) +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass relative growth rate",
    x = "Collection Point",
    y = "RGR") +
  theme_minimal()

plot(rgr_plot)

str(growtht)
summary(growtht$RGR)


# Visualize distribution
ggplot(growtht, aes(x = RGR)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ site) +
  theme_minimal()

# QQ plot by site
ggplot(growtht, aes(sample = RGR)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ site) +
  theme_minimal()

library(car)
library(emmeans)
# 3. Three-way ANOVA
# Testing effects of treatment, site, and collection_point on RGR
model_anova <- aov(RGR ~ treatment * site * collection_point, 
                   data = growtht_clean)

# Check assumptions
par(mfrow = c(2, 2))
plot(model_anova)

# Levene's test for homogeneity of variance
leveneTest(RGR ~ treatment * site * collection_point, 
           data = growtht_clean)

# Results
summary(model_anova)

# Kruskal-Wallis tests
kruskal.test(RGR ~ treatment, data = growtht_clean)
kruskal.test(RGR ~ site, data = growtht_clean)
kruskal.test(RGR ~ collection_point, data = growtht_clean)

# Separate analyses by factor
# Effect of treatment
wilcox.test(RGR ~ treatment, data = growtht_clean)

# Effect by site
growtht_clean %>%
  group_by(site) %>%
  summarise(
    p_value = wilcox.test(RGR ~ treatment)$p.value,
    .groups = "drop")

# Effect by site and collection point
growtht_clean %>%
  group_by(site, collection_point) %>%
  summarise(
    p_value = wilcox.test(RGR ~ treatment)$p.value,
    n_g = sum(treatment == "g"),
    n_ng = sum(treatment == "ng"),
    .groups = "drop")

#extention
ext_plot <- growtht %>%
  ggplot(aes(x = collection_point, y = stndrd_to_sheath, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6) +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass extension standardized to sheath",
    x = "Collection Point",
    y = "standardized extension") +
  theme_minimal()
plot(ext_plot)

# Boxplot for rgr - standardized to old leaf
rgr_plot2 <- growtht %>%
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

#remove outlieres (the two point >1)
rgr_clean <- growtht %>%
  filter(RGR_old <=1)

# compute n per group
n_labels <- rgr_clean %>%
  filter(RGR_old <= 1) %>%        # optional if you already filtered
  group_by(site, collection_point, treatment) %>%
  summarise(n = n(), .groups = "drop")

rgr_plot2.2 <- rgr_clean %>%
  ggplot(aes(x = collection_point, y = RGR_old, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6) +
  # add n labels above boxes
  geom_text(
    data = n_labels,
    aes(
      x = collection_point,
      y = 0.15,        # adjust depending on your y-scale
      label = paste0("n=", n),
      group = treatment),
    position = position_dodge(width = 0.75),
    size = 3,
    vjust = -0.3) +
  facet_wrap(~ site) +
  labs(
    title = "Seagrass relative growth rate standardized to old leaves",
    x = "Collection Point",
    y = "RGR") +
  theme_minimal()

plot(rgr_plot2.2)

###GOING TO STICK TO STANDARDIZED TO SHEATH BECAUSE IT MAKES MORE SENSE! ##########

#boxplot of rgr only seperated by site and treatment as RGR has duration incorporated already
rgr_plot3 <- growtht %>%
  ggplot(aes(x = site, y = RGR, fill = treatment)) +
  geom_boxplot()+
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 1.5,
    alpha = 0.6) +
  labs(
    title = "Seagrass relative growth rate",
    x = "Site",
    y = "RGR") +
  theme_minimal()
plot(rgr_plot3)


# Calculate n per group for label positioning

#grouped all together
n_summary_rgr <- growtht %>%
  group_by(site, treatment) %>%
  summarise(
    n = n(),
    y_pos = max(RGR, na.rm = TRUE) + 0.05,  # Position label just above the box
    .groups = "drop")


#how many RGR of 0 or NA
rgr_summary <- growtht %>%
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
#rgr_clean2 <- growtht %>% 
  #filter(!is.na(stndrd_to_sheath)) %>%
 # filter(treatment %in% c("ng", "g")) %>%
#  filter(collection_point == "t2")

mod1 <- lm(stndrd_to_sheath ~ treatment* site, data = growtht)
summary(mod1)

mod2 <- lm(stndrd_to_sheath ~ site, data = growtht)
summary(mod2)

mod0 <- lm(stndrd_to_sheath ~ 1, data = growtht)
summary(mod0)

mod3 <- lm(stndrd_to_sheath ~ treatment + site, data = growtht)

model.sel(mod1, mod2, mod0, mod3)


rgr_clean3 <- growtht%>% 
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




