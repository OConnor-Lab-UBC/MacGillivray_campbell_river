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
library(car)
library(emmeans)
#install.packages("ggpubr")
library(ggpubr)

library(rstatix)
#install.packages("rstatix")

#download data
growth<- read_csv("raw_data/Transplant_data.csv")

#peek at the data 
str(growth)
head(growth)
View(growth)
names(growth)

# Summary with site, collection_point, and treatment
full_summary <- growth %>%
  filter(!is.na(date_collected)) %>%
  #filter (length_measured > 0) %>%
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
  filter (length_measured > 0) %>% #removing 0 length measured plants as those will come out in survival but will skew this data
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
#no NAs good

#check summary with removed 0 length (mortality in survival plot)
full_summary2 <- growth2 %>%
  count(site, collection_point, g_ng) %>%
  pivot_wider(
    names_from = g_ng,
    values_from = n,
    values_fill = 0) %>%
  arrange(site, collection_point)
print(full_summary2)

#for RGR this would only be at treatment locations and on T2, 2, and maybe 3 so lets filter out other sutff
growth3<- growth2 %>%
  mutate(treatment = g_ng) %>%
  filter(!is.na(date_collected), !is.na(RGR),
         site %in% c("high", "low", "donor"),
         treatment %in% c("g", "ng"),
         collection_point %in% c("t1", "t2", "t3"))
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
#still no Nas good.


# Making plots ------------------RGR# Making plots -------------------------------------------------------


# Let's say we simply want to get a sense of the distribution of maximum shoot lengths among the clipped vs unclipped treatments. A histogram with colour codes for the two treatments would do:

length_hist <- growth3 %>%
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
#right skewed, and still fairly 0 inflated

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
  theme_bw(base_size = 14) +
  labs(
    title = "Seagrass relative growth rate",
    x = "Collection Point",
    y = "Relative growth rate (extension / sheath*duration)") +
  theme_minimal()
plot(rgr_plot)


##There is no effect of treatment so we will look at just site
##RGRplot 2 (no treatment, only T1 and T2)

growth3_b<- growth2 %>%
  filter(!is.na(date_collected), !is.na(RGR),
         site %in% c("high", "low", "donor"),
         g_ng %in% c("g", "ng"),
         collection_point %in% c("t1", "t2"))


#quick look at data without dividing by treatment
growth3_b %>%
  group_by(site, collection_point) %>%
  summarise(
    total = n(),
    na_values = sum(is.na(RGR)),
    inf_values = sum(is.infinite(RGR), na.rm = TRUE),
    valid_values = sum(is.finite(RGR)),
    mean_RGR = mean(RGR, na.rm = TRUE),
    median_RGR = median(RGR, na.rm = TRUE))

# Create histogram for RGR distribution 
RGR_hist <- growth3_b %>%
  filter(is.finite(RGR)) %>% 
  ggplot(aes(x = RGR, 
             fill = factor(g_ng), 
             color = factor(g_ng))) + 
  geom_histogram(binwidth = 0.05, alpha = 0.6, position = "identity") +
  theme_classic()
RGR_hist
#right skewed

# Boxplot for RGR without seperating by treatment (only site)
##n_labels from the filtered growth data
n_labels_b <- growth3_b %>%
  group_by(site, collection_point) %>%
  summarise(
    n = n(),
    max_rgr = suppressWarnings(max(RGR, na.rm = TRUE)),
    .groups = "drop") %>%
  mutate(
    y_pos = if_else(is.infinite(max_rgr), 0.05, max_rgr + 0.05))

## Plot WITH donor site (colored boxes)
rgr_plot_b <- growth3_b %>%
  ggplot(aes(x = site, y = RGR, fill = site)) +  # Added fill = site
  geom_boxplot() +
  geom_point(
    size = 1.5,
    alpha = 0.6) +
  geom_text(
    data = n_labels_b,
    aes(
      x = site,
      y = y_pos,
      label = paste0("n=", n)),
    size = 3,
    vjust = -0.5) +
  facet_wrap(~ collection_point) +
  scale_fill_manual(values = c("high" = "#FF8080", "low" = "#00CED1")) + 
  theme_bw(base_size = 14) +
  labs(
    title = "Seagrass relative growth rate",
    x = "Collection Point",
    y = "Relative growth rate (extension / sheath*duration)") +
  theme_minimal() +
  theme(legend.position = "none") +# Remove legend since it's redundant
plot(rgr_plot_b)


## Plot WITHOUT donor site (colored boxes)
n_labels_b_no_donor <- growth3_b %>%
  filter(site != "donor") %>%  # Exclude donor
  group_by(site, collection_point) %>%
  summarise(
    n = n(),
    max_rgr = suppressWarnings(max(RGR, na.rm = TRUE)),
    .groups = "drop") %>%
  mutate(
    y_pos = if_else(is.infinite(max_rgr), 0.05, max_rgr + 0.05))

rgr_plot_b_no_donor <- growth3_b %>%
  filter(site != "donor") %>%  # Exclude donor
  ggplot(aes(x = site, y = RGR, fill = site)) +
  geom_boxplot() +
  geom_point(
    size = 1.5,
    alpha = 0.6) +
  geom_text(
    data = n_labels_b_no_donor,
    aes(
      x = site,
      y = y_pos,
      label = paste0("n=", n)),
    size = 3,
    vjust = -0.5) +
  facet_wrap(~ collection_point) +
  scale_fill_manual(values = c("high" = "#FF8080", "low" = "#00CED1")) +  
  labs(
    title = "Seagrass relative growth rate",
    x = "Collection Point",
    y = "Relative growth rate (extension / sheath*duration)") +
  theme_minimal() +
  theme(legend.position = "none")
plot(rgr_plot_b_no_donor)
## lets keep donor site in for now even though methods were different

#Now the STATS 

## Visualize distribution
ggplot(growth3_b, aes(x = RGR)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ site) +
  theme_minimal()
##with treatment
ggplot(growth3, aes(x = RGR)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ site) +
  theme_minimal()

# QQ plot by site
ggplot(growth3, aes(sample = RGR)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ site) +
  theme_minimal()
ggplot(growth3_b, aes(sample = RGR)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ site) +
  theme_minimal()
##not perfect but maybe ok?


## ANOVA

## Testing effects of treatment, site, and collection_point on RGR
anova1 <- aov(RGR ~ treatment * site * collection_point, 
                   data = growth3)
summary(anova1)
### site and collection point sig

##Testing effects of site, and collection_point on RGR if ignore treatment
anova1_b <- aov(RGR ~ site * collection_point, 
              data = growth3_b)
summary(anova1_b)
###site sig

# Check assumptions
par(mfrow = c(2, 2))
plot(anova1)
#looks OK?

par(mfrow = c(2, 2))
plot(anova1_b)
#looks OK?


## Tukey's HSD (ANOVA post-hocs)
TukeyHSD(anova1, "site")
##only low and donor diff

## Tukey's HSD (ANOVA post-hocs)
TukeyHSD(anova1_b, "site")
##low and high and low and donor 

#remove donor due to methods
# Testing effects of treatment, site, and collection_point on RGR
growth4 <- growth3 %>%
  filter(site != "donor")

growth4_b <- growth3_b %>%
  filter(site != "donor")

anova4 <- aov(RGR ~ treatment * site * collection_point, 
              data = growth4)
summary(anova4)

anova4_b <- aov(RGR ~ site * collection_point, 
              data = growth4_b)
summary(anova4_b)

#further stats using g3
mod1b <- lm(RGR ~ treatment* site + collection_point, data = growth3)
summary(mod1b)

mod2b <- lm(RGR ~ site+ collection_point, data = growth3)
summary(mod2b)

mod2.5b <- lm(RGR ~ treatment + collection_point, data = growth3)
summary(mod2.5b)

mod4b <- lm(RGR ~ collection_point, data = growth3)
summary(mod4b)

mod0b <- lm(RGR ~ 1, data = growth3)
summary(mod0b)

mod3b <- lm(RGR ~ treatment + site, data = growth3)
summary(mod3b)

mod3.2b <- lm(RGR ~ treatment * collection_point, data = growth3)
summary(mod3.2b)

model.sel(mod1b, mod2b, mod2.5b, mod4b, mod3b, mod3.2b, mod0b, anova1)
#best model dosent have effect of treatment but top two have effect of site

#further stats for ignoring treatment data
mod1c <- lm(RGR ~ site + collection_point, data = growth3_b)
summary(mod1c)

mod2c <- lm(RGR ~ site+ collection_point, data = growth3_b)
summary(mod2c)

mod2.5c <- lm(RGR ~ collection_point, data = growth3_b)
summary(mod2.5c)

mod4c <- lm(RGR ~ collection_point, data = growth3_b)
summary(mod4c)

mod0c <- lm(RGR ~ 1, data = growth3_b)
summary(mod0c)

mod3c <- lm(RGR ~ site, data = growth3_b)
summary(mod3c)

mod3.2c <- lm(RGR ~ collection_point, data = growth3_b)
summary(mod3.2c)

model.sel(mod1c, mod2c, mod2.5c, mod4c, mod0c, mod3c, mod3.2c, anova1_b)
# best model jsut site second best only 1AIC away and has collection point


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

