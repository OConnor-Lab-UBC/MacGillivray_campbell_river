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

#download data
growth<- read_csv("raw_data/Tansplant_data.csv")

#peek at the data 
str(growth)
head(growth)
View(growth)
names(growth)

#check summary 
site_time_summary <- growth %>%
  filter(!is.na(date_collected)) %>%
  count(site, collection_point) %>%
  arrange(site, collection_point)
site_time_summary

# Full summary with site, collection_point, and treatment
full_summary <- growth %>%
  filter(!is.na(date_collected)) %>%
  count(site, collection_point, g_ng) %>%
  pivot_wider(
    names_from = g_ng,
    values_from = n,
    values_fill = 0) %>%
  arrange(site, collection_point)
print(full_summary)

#clean data and add LA And RGR
growth2 <- growth %>%
  filter(!is.na(date_collected)) %>%
  filter(!is.na(g_ng)) %>%
  mutate(
    sheath_length = as.numeric(sheath_length),
    duration = as.numeric(duration_days),  # Also need to convert duration
    # Remove Excel errors and convert to numeric
    stndrd_to_sheath = as.numeric(ifelse(standardized_extension_to_sheath == "#DIV/0!", NA, standardized_extension_to_sheath)),
    stndrd_to_old = as.numeric(ifelse(standardized_to_old == "#DIV/0!", NA, standardized_to_old)),
    LA = length_measured * shoot_width * blade_number_tx) %>%
  mutate(
    RGR = stndrd_to_sheath / duration,  # Use the abbreviated name you created
    RGR_old = stndrd_to_old / duration) %>% # Use the abbreviated name you created
  mutate(
    RGR = ifelse(is.na(RGR), 0, RGR),
    RGR_old = ifelse(is.na(RGR_old), 0, RGR_old))
head(growth2) #check to see that the new growth columns have been added


#check NA in RGR 
growth2 %>%
  group_by(site, g_ng) %>%
  summarize(na_count = sum(is.na(RGR)))

#NA in RGR just means 0 growth as plant as we already have filtered for is collected
#this means we need to change NA RGR to 0 
full_summary2 <- growth2 %>%
  count(site, collection_point, g_ng) %>%
  pivot_wider(
    names_from = g_ng,
    values_from = n,
    values_fill = 0) %>%
  arrange(site, collection_point)
print(full_summary2)

growth3<- growth2 %>%
  mutate(treatment = g_ng) %>%
  filter(!is.na(date_collected), !is.na(RGR),
         site %in% c("high", "low", "donor"),
         treatment %in% c("g", "ng"),
         collection_point %in% c("t1", "t2", "t3", "t0"))
head(growth3)

#check nas
full_summary3 <- growth3 %>%
  filter(!is.na(date_collected)) %>%
  count(site, collection_point, g_ng) %>%
  pivot_wider(
    names_from = g_ng,
    values_from = n,
    values_fill = 0) %>%
  arrange(site, collection_point)
print(full_summary3)



# Making plots ------------------RGR# Making plots -------------------------------------------------------


# Let's say we simply want to get a sense of the distribution of maximum shoot lengths among the clipped vs unclipped treatments. A histogram with colour codes for the two treatments would do:

length_hist <- growth2 %>%
  ggplot(aes(x = as.numeric(length_calculated), 
             group = g_ng, 
             fill = factor(g_ng), 
             color = factor(g_ng))) + 
  geom_histogram(binwidth = 15) +
  theme_classic() +
  xlab("Seagrass length by treatment (mm)")
length_hist
#this is very 0 inflated but otherwise normal. For this study it should be ok to remove 0s and just look at ones with leghth. We will look at 0's and survival later

# Check the RGR data by treatment
growth2 %>%
  group_by(treatment, site, collection_point) %>%
  summarise(
    total = n(),
    na_values = sum(is.na(RGR)),
    inf_values = sum(is.infinite(RGR), na.rm = TRUE),
    valid_values = sum(is.finite(RGR)),
    mean_RGR = mean(RGR, na.rm = TRUE),
    median_RGR = median(RGR, na.rm = TRUE))

# Create histogram for RGR distribution 
RGR_hist <- growth3 %>%
  filter(is.finite(RGR)) %>% 
  ggplot(aes(x = RGR, 
             fill = factor(treatment), 
             color = factor(treatment))) + 
  geom_histogram(binwidth = 0.05, alpha = 0.6, position = "identity") +
  theme_classic()
RGR_hist
#right skewed

# Boxplot for RGR  
# n_labels from the filtered growth data
n_labels <- growth3 %>%
  group_by(site, collection_point, treatment) %>%
  summarise(
    n = n(),
    max_rgr = suppressWarnings(max(RGR, na.rm = TRUE)),
    .groups = "drop") %>%
  mutate(
    y_pos = if_else(is.infinite(max_rgr), 0.05, max_rgr + 0.05))

# Recreate plot
rgr_plot <- growth3 %>%
  ggplot(aes(x = collection_point, y = RGR)) +
  geom_boxplot(aes(fill = treatment), position = position_dodge(width = 0.75)) +
  geom_point(
    aes(color = treatment),
    position = position_dodge(width = 0.75),
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
    y = "Relative growth rate (extension / sheath*duration)") +
  theme_minimal()

plot(rgr_plot)

#--was having problem with plot this is diagnost code---
##look at tw high data as points dont look right
#ht2 <- growth3 %>%
 # filter(site %in% c("high")) %>%
 # filter(collection_point %in% c("t2"))%>%
  #filter(g_ng %in% c("g")) %>%
 # select(id, RGR, g_ng)
#ht2

# First, let's check if your 'treatment' column matches 'g_ng'
#growth3 %>%
 # filter(site == "high", collection_point == "t2") %>%
 # select(id, g_ng, treatment, RGR) %>%
  #head(10)

# Also check if 'treatment' has any NA values
# filter(is.na(treatment)) %>%
  #nrow()

# Check if you have any custom color scales set globally
# or in your environment that might be causing issues

# Try this simpler version to isolate the problem:
#growth3 %>%
 # filter(site == "high", collection_point == "t2") %>%
 # ggplot(aes(x = collection_point, y = RGR, color = treatment)) +
 # geom_jitter(width = 0.2, size = 2, alpha = 0.8) +
  #labs(title = "Test plot - high site t2 only")


# Check  "ng" values at high site T2
#growth3 %>%
 # filter(site == "high", collection_point == "t2") %>%
 # select(id, RGR, g_ng) %>%
 # arrange(desc(RGR))%>%
 # print(n = 34)

# Check if the same data is being plotted twice somehow
# Look at your plotting code - are you adding multiple geom_point() layers?

# Also check if there are actual duplicate rows in your data
#growth3 %>%
 #filter(site == "high", collection_point == "t2") %>%
 # group_by(id, RGR, g_ng) %>%
 # filter(n() > 1) %>%
 # arrange(id)

# Or check for duplicate IDs with different values
#growth3 %>%
 # filter(site == "high", collection_point == "t2") %>%
  #add_count(id) %>%
 # filter(n > 1) %>%
 # arrange(id)
##-----------------------------##

#only high and low adn T1 / T2 as donor site had unreliable collection and t3 pinpricks should have grown off so 0 growth is not accurate
hl <- growth3 %>%
  filter(site %in% c("high", "low")) %>%
  filter(collection_point %in% c("t1", "t2"))

n_labelshl <- hl %>%
  group_by(site, collection_point, treatment) %>%
  summarise(
    n = n(),
    max_rgr = suppressWarnings(max(RGR, na.rm = TRUE)),
    .groups = "drop") %>%
  mutate(
    y_pos = if_else(is.infinite(max_rgr), 0.05, max_rgr + 0.05))

rgr_plot <- hl %>%
  ggplot(aes(x = collection_point, y = RGR, fill = treatment)) +
  geom_boxplot() +
  geom_point(
    aes(color = treatment),
    position = position_dodge(width = 0.75),
    size = 1.5,
    alpha = 0.6) +
  geom_text(
    data = n_labelshl,
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
    y = "Relative growth rate (extension / sheath*duration)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12))
plot(rgr_plot)

#Now the STATS

# Visualize distribution
ggplot(growth3, aes(x = RGR)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ site) +
  theme_minimal()
#same as before just seperated by site, still all right skewed
ggplot(hl, aes(x = RGR)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ site) +
  theme_minimal()

# QQ plot by site
ggplot(growth3, aes(sample = RGR)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ site) +
  theme_minimal()
#not perfect but maybe ok?

ggplot(hl, aes(sample = RGR)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ site) +
  theme_minimal()

#not perfect but maybe ok?
library(car)
library(emmeans)

# ANOVA

# Testing effects of treatment, site, and collection_point on RGR
anova1 <- aov(RGR ~ treatment * site * collection_point, 
                   data = growth3)
summary(anova1)

# Check assumptions
par(mfrow = c(2, 2))
plot(anova1)
#looks OK

#for only T1/ T2 high and low
# Three-way ANOVA
modelhl <- aov(RGR ~ site * collection_point * treatment, data = hl)
summary(modelhl)

# Check assumptions
par(mfrow = c(2, 2))
plot(modelhl)


library(rstatix)
#install.packages("rstatix")

# Site effect (p = 0.016)
pairwise_reslut1 <- pairwise_t_test(growth3, RGR ~ site, p.adjust.method = "bonferroni")
pairwise_reslut1

# Collection point effect (p < 0.001)
pairwsis_result2<- pairwise_t_test(growth3, RGR ~ collection_point, p.adjust.method = "bonferroni")
pairwsis_result2


#remove donor
# Testing effects of treatment, site, and collection_point on RGR
growth4 <- growth3 %>%
  filter(site != "donor")

anova4 <- aov(RGR ~ treatment * site * collection_point, 
              data = growth4)
summary(anova4)

#further stats for high low, t1/t2 

mod1 <- lm(RGR ~ treatment* site + collection_point, data = hl)
summary(mod1)

mod2 <- lm(RGR ~ site+ collection_point, data = hl)
summary(mod2)

mod2.5 <- lm(RGR ~ treatment + collection_point, data = hl)
summary(mod2.5)

mod4 <- lm(RGR ~ collection_point, data = hl)
summary(mod4)

mod0 <- lm(RGR ~ 1, data = hl)
summary(mod0)

mod3 <- lm(RGR ~ treatment + site, data = hl)
summary(mod3)

mod3.2 <- lm(RGR ~ treatment * collection_point, data = hl)
summary(mod3.2)

model.sel(mod1, mod2, mod2.5, mod4, modelhl, mod3, mod3.2, mod0)

#mod3 and 2 best, no interactions











#____________________extention
#extention
ext_plot <- growth3 %>%
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
rgr_plot2 <- growth3 %>%
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
rgr_clean <- growth3 %>%
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


#how many RGR of 0 or NA
rgr_summary <- growth3 %>%
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




