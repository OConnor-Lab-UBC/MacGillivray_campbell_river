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

# clean data and add LA And RGR
growth2 <- growth %>%
  mutate(
    sheath_length = as.numeric(sheath_length),
    duration = as.numeric(duration_days),  # Also need to convert duration
    # Remove Excel errors and convert to numeric
    stndrd_to_sheath = as.numeric(ifelse(standardized_extension_to_sheath == "#DIV/0!", NA, standardized_extension_to_sheath)),
    stndrd_to_old = as.numeric(ifelse(standardized_to_old == "#DIV/0!", NA, standardized_to_old)),
    LA = length_measured * shoot_width * blade_number_tx) %>%
  mutate(
    RGR = stndrd_to_sheath / duration,  # Use the abbreviated name you created
    RGR_old = stndrd_to_old / duration) # Use the abbreviated name you created
head(growth2) #check to see that the new growth columns have been added

#create data frame with only collections with RGR for RGR analysis
growth3<- growth2 %>%
  mutate(treatment = g_ng) %>%
  filter(!is.na(date_collected), !is.na(RGR),
         site %in% c("high", "low", "donor"),
         treatment %in% c("g", "ng"),
         collection_point %in% c("t1", "t2", "t3", "t0"))
head(growth3)

# Making plots -------------------------------------------------------


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
growth3 %>%
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
# n_labels from the filtered growtht data
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

#Now the STATS

# Visualize distribution
ggplot(growth3, aes(x = RGR)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ site) +
  theme_minimal()
#same as before just seperated by site, still all right skewed

# QQ plot by site
ggplot(growth3, aes(sample = RGR)) +
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
plot(model_anova)
#looks OK

library(rstatix)
#install.packages("rstatix")

# Site effect (p = 0.0354)
pairwise_reslut1 <- pairwise_t_test(growth3, RGR ~ site, p.adjust.method = "bonferroni")
pairwise_reslut1

# Collection point effect (p < 0.001)
pairwsis_result2<- pairwise_t_test(growth3, RGR ~ collection_point, p.adjust.method = "bonferroni")
pairwsis_result2


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




