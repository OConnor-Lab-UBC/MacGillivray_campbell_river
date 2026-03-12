# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
#install.packages("patchwork")
library(patchwork)

# load data
data_h2s<- read.csv("raw_data/sulfide.csv")
data_fe <- read.csv("raw_data/Fe.csv")

# clean data
## h2s
data_h2s <- data_h2s %>%
  mutate(type = case_when(
    type %in% c("ng", "NG") ~ "ng",
    TRUE ~ type)) %>%
  mutate(unique_core_id = paste(site, time_point, core, sep = "_")) %>%
  mutate(site = factor(site, 
                       levels = c("donor", "low", "high"),
                       labels = c("Donor", "Low", "High"))) %>%
  rename(conc = h2s) %>%  # Rename fe column 
  mutate(variable = "H2S",
         rep = as.character(rep))  # Convert rep to character

# Clean Fe data (adjust column names as needed)
data_fe <- data_fe %>%
  mutate(type = case_when(
    type %in% c("ng", "NG") ~ "ng",
    TRUE ~ type)) %>%
  mutate(unique_core_id = paste(site, time_point, core, sep = "_")) %>%
  mutate(site = factor(site, 
                       levels = c("donor", "low", "high"),
                       labels = c("Donor", "Low", "High"))) %>%
  rename(conc = total_Fe_uM) %>%  # Rename fe column 
  mutate(variable = "Fe",
         rep = as.character(rep))  # Convert rep to character

#combine datasets
data_combined <- bind_rows(data_h2s, data_fe)

##_________________________________
#**H2S**
#average reps so each cor at each depth has one number
data_avg_h2s <- data_h2s %>%
  mutate(sample_base = gsub("_r[0-9]+$", "", sample_id)) %>%  # strip the _r1/_r2/_r3
  group_by(sample_base, site, time_point, type, core, depth_cm) %>%
  summarise(
    abs_mean = mean(abs, na.rm = TRUE),
    abs_se = sd(abs, na.rm = TRUE) / sqrt(n()),
    h2s_mean = mean(conc, na.rm = TRUE),
    h2s_se = sd(conc, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop")

## Create averaged data with standard error for each depth, type, site, and time point
data_avg_h2s_2 <- data_avg_h2s %>%
  group_by(site, type, depth_cm, time_point) %>%  
  summarise(
    h2s_avg = mean(h2s_mean, na.rm = TRUE),
    h2s_se = sd(h2s_mean, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

###check each core has one value at each depth
data_avg_h2s_2 %>% count(site, type, depth_cm, time_point)

#### If n > 1 per group, cores are being averaged. If n = 1, they aren't.

#look at data
##Count cores for each combination
core_counts_h2s <- data_avg_h2s %>%   # use data_avg_h2s, not data_avg_h2s_2
  group_by(site, type, depth_cm, time_point) %>%
  summarise(n_cores = n_distinct(core),
            .groups = 'drop') %>%
  pivot_wider(names_from = c(time_point, type), 
              values_from = n_cores)
core_counts_h2s
###**only have one low away T3 core, these were the ones that were dropped!* 
###*Hal says sediment should change in the months thats why we are using T3 
###*maybe I can combine the  T1 cores to get higher n


#Plots

##some cleinging for plotting
data_avg_h2s_plot <- data_avg_h2s %>% mutate(depth_f = factor(depth_cm))

##jitter and doddge so we can read multiple treatments
jd <- position_jitterdodge(jitter.width = 0, jitter.height = 0.15, dodge.width = 0.5)
dodge <- position_dodge(width = 0.5)  # for averages and error bars - no jitter on these


#___PLOT 1: ALL TIMEPOINTS TOGETHER WITH ALL TREATMENTS____________________________####

##── Collapse across time_point for plotting ───────────────────
data_avg_h2s_2_collapsed <- data_avg_h2s %>%
  group_by(site, type, depth_cm) %>%
  summarise(
    h2s_avg = mean(h2s_mean, na.rm = TRUE),
    h2s_se  = sd(h2s_mean,  na.rm = TRUE) / sqrt(n()),
    .groups = "drop") %>%
  mutate(depth_f = factor(depth_cm))

## ── Clean n labels — no time_point ───────────────────────────
n_labels <- data_avg_h2s %>%
  group_by(site, type, depth_cm) %>%
  summarise(n = n_distinct(core), .groups = "drop") %>%
  mutate(
    depth_f = factor(depth_cm),
    x_nudge = ifelse(site == "High", -50, 0))  # adjust -50 to taste= "drop")
n_labels <- n_labels %>%
  mutate(x_pos = ifelse(site == "High", 850, Inf))  # adjust 850 to taste
## ── Plot ──────────────────────────────────────────────────────
ggplot() +
  geom_point(data = data_avg_h2s_plot,
             aes(x = h2s_mean, y = depth_f, color = type, group = type),
             size = 1.5, alpha = 0.4, shape = 19,
             position = jd) +
  geom_errorbarh(data = data_avg_h2s_2_collapsed,     # <-- collapsed
                 aes(xmin = h2s_avg - h2s_se,
                     xmax = h2s_avg + h2s_se,
                     y = depth_f, color = type, group = type),
                 height = 0.3, linewidth = 0.8,
                 position = dodge) +
  geom_point(data = data_avg_h2s_2_collapsed,         # <-- collapsed
             aes(x = h2s_avg, y = depth_f, shape = type, color = type, group = type),
             size = 3,
             position = dodge) +
  # In both p_sulfide and p_iron, update the geom_text layer:
  geom_text(data = n_labels,
            aes(x = Inf, y = depth_f, color = type,
                label = paste0("n=", n), group = type),
            hjust = -0.1,          # <-- positive pushes it outside the panel
            size = 4.5,
            position = dodge) +
  scale_y_discrete(limits = rev(levels(factor(data_avg_h2s_plot$depth_f)))) +
  scale_color_manual(
    values = c("g" = "black", "ng" = "green3", "away" = "royalblue"),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  scale_shape_manual(
    values = c("g" = 16, "ng" = 17, "away" = 15),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  facet_grid(~ site) +
  labs(x = expression(H[2]*S~(mu*M)),
       y = "Depth (cm)", color = "Treatment", shape = "Treatment") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13))

#_________________________PLOT 2: ONLY T3______________________###

##── Prep ──────────────────────────────────────────────────────
###filter for only T3
dataT3_points <- data_avg_h2s %>%
  filter(time_point == "T3")

dataT3_avg <- data_avg_h2s_2 %>%
  filter(time_point == "T3")
###set up labels
n_T3 <-  dataT3_points %>%
  group_by(site, type, depth_cm) %>%
  summarise(n = n_distinct(core), .groups = "drop") %>%
  mutate(depth_f = factor(depth_cm))


###clean up for plotting
dataT3_points_plot <- dataT3_points %>% mutate(depth_f = factor(depth_cm))
dataT3_avg_plot    <- dataT3_avg    %>% mutate(depth_f = factor(depth_cm))
n_T3_plot          <- n_T3          %>% mutate(depth_f = factor(depth_cm))

###ensure we can see everything on plot
jd    <- position_jitterdodge(jitter.width = 0, jitter.height = 0.15, dodge.width = 0.5)
dodge <- position_dodge(width = 0.5)

##plot
ggplot() +
  geom_point(data = dataT3_points_plot,
             aes(x = h2s_mean, y = depth_f, color = type, group = type),
             size = 2, alpha = 0.5, shape = 19,
             position = jd) +
  geom_errorbarh(data = dataT3_avg_plot,
                 aes(xmin = h2s_avg - h2s_se,
                     xmax = h2s_avg + h2s_se,
                     y = depth_f, color = type, group = type),
                 height = 0.3, linewidth = 0.8,
                 position = dodge) +
  geom_point(data = dataT3_avg_plot,
             aes(x = h2s_avg, y = depth_f, shape = type, color = type, group = type),
             size = 3,
             position = dodge) +
  geom_text(data = n_T3_plot,
            aes(x = Inf, y = depth_f, color = type,
                label = paste0("n=", n), group = type),
            hjust = 1.1, size = 3.5,
            position = dodge) +
  scale_y_discrete(limits = rev(levels(factor(dataT3_points_plot$depth_f)))) +
  scale_color_manual(
    values = c("g" = "black", "ng" = "green3", "away" = "royalblue"),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  scale_shape_manual(
    values = c("g" = 16, "ng" = 17, "away" = 15),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  facet_wrap(~ site, ncol = 3) +
  labs(x = expression(H[2]*S~(mu*M)),
       y = "Depth (cm)", color = "Treatment", shape = "Treatment") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13))

# ── Plot 3: adding T1 away to low data as that shouldnt chnge and will increase n ───────────────────────────────────
# ── Custom filtered points data ───────────────────────────────
dataT3_points2 <- bind_rows(
  # T3 for everything
  data_avg_h2s %>% filter(time_point == "T3"),
  # all timepoints for Low away only (excluding T3 to avoid duplicates)
  data_avg_h2s %>% filter(site == "Low", type == "away", time_point != "T3"))

# ── Custom filtered avg data ──────────────────────────────────
dataT3_avg2 <- bind_rows(
  # T3 averages for everything except Low away
  data_avg_h2s_2 %>% filter(time_point == "T3") %>% 
    filter(!(site == "Low" & type == "away")),
  # Low away — re-averaged across ALL time points
  data_avg_h2s %>%
    filter(site == "Low", type == "away") %>%
    group_by(site, type, depth_cm) %>%
    summarise(
      h2s_avg = mean(h2s_mean, na.rm = TRUE),
      h2s_se  = sd(h2s_mean,  na.rm = TRUE) / sqrt(n()),
      .groups = "drop")) %>%
  mutate(depth_f = factor(depth_cm))

###set up labels
n_T3 <-  dataT3_points2 %>%
  group_by(site, type, depth_cm) %>%
  summarise(n = n_distinct(core), .groups = "drop") %>%
  mutate(depth_f = factor(depth_cm))


###clean up for plotting
dataT3_points_plot <- dataT3_points2 %>% mutate(depth_f = factor(depth_cm))
dataT3_avg_plot    <- dataT3_avg2    %>% mutate(depth_f = factor(depth_cm))
n_T3_plot          <- n_T3          %>% mutate(depth_f = factor(depth_cm))

###ensure we can see everything on plot
jd    <- position_jitterdodge(jitter.width = 0, jitter.height = 0.15, dodge.width = 0.5)
dodge <- position_dodge(width = 0.5)

##plot name this one because we want to use it later
psulfide<- ggplot() +
  geom_point(data = dataT3_points_plot,
             aes(x = h2s_mean, y = depth_f, color = type, group = type),
             size = 2, alpha = 0.5, shape = 19,
             position = jd) +
  geom_errorbarh(data = dataT3_avg_plot,
                 aes(xmin = h2s_avg - h2s_se,
                     xmax = h2s_avg + h2s_se,
                     y = depth_f, color = type, group = type),
                 height = 0.3, linewidth = 0.8,
                 position = dodge) +
  geom_point(data = dataT3_avg_plot,
             aes(x = h2s_avg, y = depth_f, shape = type, color = type, group = type),
             size = 3,
             position = dodge) +
  #geom_text(data = n_T3_plot,
            #aes(x = Inf, y = depth_f, color = type,
              #  label = paste0("n=", n), group = type),
           # hjust = 1.1, size = 3.5,
           # position = dodge) +
  scale_y_discrete(limits = rev(levels(factor(dataT3_points_plot$depth_f)))) +
  scale_color_manual(
    values = c("g" = "black", "ng" = "green3", "away" = "royalblue"),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  scale_shape_manual(
    values = c("g" = 16, "ng" = 17, "away" = 15),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  facet_wrap(~ site, ncol = 3) +
  labs(x = expression(H[2]*S~(mu*M)),
       y = "Depth (cm)", color = "Treatment", shape = "Treatment") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13))
psulfide


#___Plot4: only away

###filter fROM t3 for only away
dataT3_away_points <- dataT3_points %>%
  filter(type == "away")

dataT3_away_avg <- dataT3_avg %>%
  filter(type == "away")

###set up labels
n_T3_away <- data_avg_h2s %>%
  filter(time_point == "T3", type == "away") %>%
  group_by(site, depth_cm) %>%
  summarise(n = n_distinct(core), .groups = "drop")

##plot
ggplot() +
  geom_point(data = dataT3_away_points,
             aes(x = h2s_mean, y = depth_cm),
             color = "royalblue", size = 2, alpha = 0.5, shape = 19) +
  geom_errorbarh(data = dataT3_away_avg,
                 aes(xmin = h2s_avg - h2s_se,
                     xmax = h2s_avg + h2s_se,
                     y = depth_cm),
                 color = "royalblue", height = 0.3, linewidth = 0.8) +
  geom_point(data = dataT3_away_avg,
             aes(x = h2s_avg, y = depth_cm),
             color = "royalblue", shape = 15, size = 3) +
  geom_text(data = n_T3_away,
            aes(x = Inf, y = depth_cm, label = paste0("n=", n)),
            color = "royalblue", hjust = 1.1, size = 3.5) +
  facet_wrap(~ site, ncol = 3) +
  scale_y_reverse() +
  coord_cartesian(ylim = c(max(data_h2s$depth_cm, na.rm = TRUE), 0)) +
  labs(x = expression(H[2]*S~(mu*M)),
       y = "Depth (cm)") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 14))

#_______________________________________________________________________________
#_______IRON____________________________________________________________________

#**Fe**
# Step 1: average reps within each core
data_avg_fe <- data_fe %>%
  mutate(sample_base = gsub("_r[0-9]+$", "", sample_id)) %>%
  group_by(sample_base, site, time_point, type, core, depth_cm) %>%
  summarise(
    fe_mean = mean(conc, na.rm = TRUE),
    fe_se = sd(conc, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop")

# Step 2: average cores within each site/type/depth/timepoint
data_avg_fe_2 <- data_avg_fe %>%
  group_by(site, type, depth_cm, time_point) %>%
  summarise(
    fe_avg = mean(fe_mean, na.rm = TRUE),
    fe_se = sd(fe_mean, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

#_______PLOT1: ALL TIMEPOINTS AND TREATMENTS TOGETHER
# ── Factor depth for dodging ──────────────────────────────────
data_avg_fe_plot   <- data_avg_fe   %>% mutate(depth_f = factor(depth_cm))
data_avg_fe_2_plot <- data_avg_fe_2 %>% mutate(depth_f = factor(depth_cm))
n_labels_fe_plot   <- n_labels_fe   %>% mutate(depth_f = factor(depth_cm))

# ── Positions ─────────────────────────────────────────────────
jd     <- position_jitterdodge(jitter.width = 0, jitter.height = 0.15, dodge.width = 0.5)
dodge  <- position_dodge(width = 0.5)
# ── n labels — no time_point ──────────────────
# Make sure this is run first:
n_labels_fe <- data_avg_fe %>%
  group_by(site, type, depth_cm) %>%
  summarise(n = n_distinct(core), .groups = "drop") %>%
  mutate(depth_f = factor(depth_cm))

#n_labels_fe_plot <- n_labels_fe %>% mutate(depth_f = factor(depth_cm))

# ── Also collapse data_avg_fe_2 across time_point for plotting ─
data_avg_fe_2_collapsed <- data_avg_fe %>%
  group_by(site, type, depth_cm) %>%
  summarise(
    fe_avg = mean(fe_mean, na.rm = TRUE),
    fe_se  = sd(fe_mean,  na.rm = TRUE) / sqrt(n()),
    .groups = "drop") %>%
  mutate(depth_f = factor(depth_cm))

# ── Use collapsed version in plot ────────────────────────────
piron<- ggplot() +
  geom_errorbarh(data = data_avg_fe_plot,
                 aes(xmin = fe_mean - fe_se,
                     xmax = fe_mean + fe_se,
                     y = depth_f, color = type, group = type),
                 height = 0.15, linewidth = 0.4, alpha = 0.35,
                 position = dodge) +
  geom_point(data = data_avg_fe_plot,
             aes(x = fe_mean, y = depth_f, color = type, group = type),
             size = 1.5, alpha = 0.4, shape = 19,
             position = jd) +
  geom_errorbarh(data = data_avg_fe_2_collapsed,      # <-- collapsed
                 aes(xmin = fe_avg - fe_se,
                     xmax = fe_avg + fe_se,
                     y = depth_f, color = type, group = type),
                 height = 0.3, linewidth = 0.8,
                 position = dodge) +
  geom_point(data = data_avg_fe_2_collapsed,          # <-- collapsed
             aes(x = fe_avg, y = depth_f, shape = type, color = type, group = type),
             size = 3,
             position = dodge) +
  # In both p_sulfide and p_iron, update the geom_text layer:
  geom_text(data = n_labels_fe,
            aes(x = Inf, y = depth_f, color = type,
                label = paste0("n=", n), group = type),
            hjust = 1.1,          # <-- back to inside panel
            size = 4.5,
            position = dodge) +
  scale_y_discrete(limits = rev(levels(factor(data_avg_fe_plot$depth_f)))) +
  # replace the existing scale_x_continuous in p_iron with:
  scale_x_continuous(
    limits = c(0, 225),
    breaks = seq(0, 225, by = 50),
    expand = expansion(mult = c(0.05, 0.12))) +
  scale_color_manual(
    values = c("g" = "black", "ng" = "green3", "away" = "royalblue"),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  scale_shape_manual(
    values = c("g" = 16, "ng" = 17, "away" = 15),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  facet_grid(~ site) +
  coord_cartesian(clip = "off") +   # <-- add this
  labs(x = expression("Total Fe " * (mu*M)),
       y = "Depth (cm)", color = "Treatment", shape = "Treatment") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16))
piron

# ── Plot 3: adding T1 away to low data as that shouldnt chnge and will increase n ───────────────────────────────────
# ── Custom filtered points data ───────────────────────────────
dataT3_points2 <- bind_rows(
  # T3 for everything
  data_avg_fe %>% filter(time_point == "T3"),
  # all timepoints for Low away only (excluding T3 to avoid duplicates)
  data_avg_fe %>% filter(site == "Low", type == "away", time_point != "T3"))

# ── Custom filtered avg data ──────────────────────────────────
dataT3_avg2 <- bind_rows(
  # T3 averages for everything except Low away
  data_avg_fe_2 %>% filter(time_point == "T3") %>% 
    filter(!(site == "Low" & type == "away")),
  # Low away — re-averaged across ALL time points
  data_avg_fe %>%
    filter(site == "Low", type == "away") %>%
    group_by(site, type, depth_cm) %>%
    summarise(
      fe_avg = mean(fe_mean, na.rm = TRUE),
      fe_se  = sd(fe_mean,  na.rm = TRUE) / sqrt(n()),
      .groups = "drop")) %>%
  mutate(depth_f = factor(depth_cm))

###set up labels
n_T3 <-  dataT3_points2 %>%
  group_by(site, type, depth_cm) %>%
  summarise(n = n_distinct(core), .groups = "drop") %>%
  mutate(depth_f = factor(depth_cm))


###clean up for plotting
dataT3_points_plot <- dataT3_points2 %>% mutate(depth_f = factor(depth_cm))
dataT3_avg_plot    <- dataT3_avg2    %>% mutate(depth_f = factor(depth_cm))
n_T3_plot          <- n_T3          %>% mutate(depth_f = factor(depth_cm))

###ensure we can see everything on plot
jd    <- position_jitterdodge(jitter.width = 0, jitter.height = 0.15, dodge.width = 0.5)
dodge <- position_dodge(width = 0.5)

##plot name this one because we want to use it later
piron<- ggplot() +
  geom_point(data = dataT3_points_plot,
             aes(x = fe_mean, y = depth_f, color = type, group = type),
             size = 2, alpha = 0.5, shape = 19,
             position = jd) +
  geom_errorbarh(data = dataT3_avg_plot,
                 aes(xmin = fe_avg - fe_se,
                     xmax = fe_avg + fe_se,
                     y = depth_f, color = type, group = type),
                 height = 0.3, linewidth = 0.8,
                 position = dodge) +
  geom_point(data = dataT3_avg_plot,
             aes(x = fe_avg, y = depth_f, shape = type, color = type, group = type),
             size = 3,
             position = dodge) +
  geom_text(data = n_T3_plot,
            aes(x = Inf, y = depth_f, color = type,
                label = paste0("n=", n), group = type),
            hjust = 1.1, size = 3.5,
            position = dodge) +
  scale_y_discrete(limits = rev(levels(factor(dataT3_points_plot$depth_f)))) +
  scale_color_manual(
    values = c("g" = "black", "ng" = "green3", "away" = "royalblue"),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  scale_shape_manual(
    values = c("g" = 16, "ng" = 17, "away" = 15),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  facet_wrap(~ site, ncol = 3) +
  labs(x = expression(Fe~(mu*M)),
       y = "Depth (cm)", color = "Treatment", shape = "Treatment") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13))
piron

#________PLOT 3: T3 only________________________________________________________
dataT3_fe_points <- data_avg_fe %>% filter(time_point == "T3")
dataT3_fe_avg <- data_avg_fe_2 %>% filter(time_point == "T3")

##── n labels for T3 only ──────────────────────────────────────
n_labels_T3_fe <- dataT3_fe_points %>%
  group_by(site, type, depth_cm) %>%
  summarise(n = n_distinct(core), .groups = "drop") %>%
  mutate(depth_f = factor(depth_cm))

## ── Factor depth for dodging ──────────────────────────────────
dataT3_fe_points <- dataT3_fe_points %>% mutate(depth_f = factor(depth_cm))
dataT3_fe_avg    <- dataT3_fe_avg    %>% mutate(depth_f = factor(depth_cm))

## ── Positions ─────────────────────────────────────────────────
jd    <- position_jitterdodge(jitter.width = 0, jitter.height = 0.15, dodge.width = 0.5)
dodge <- position_dodge(width = 0.5)

##___plot
ggplot() +
  geom_point(data = dataT3_fe_points,
             aes(x = fe_mean, y = depth_f, color = type, group = type),
             position = jd,
             size = 1.5, alpha = 0.4, shape = 19) +
  geom_errorbarh(data = dataT3_fe_avg,
                 aes(xmin = fe_avg - fe_se,
                     xmax = fe_avg + fe_se,
                     y = depth_f, color = type, group = type),
                 position = dodge,
                 height = 0.3, linewidth = 0.8) +
  geom_point(data = dataT3_fe_avg,
             aes(x = fe_avg, y = depth_f, shape = type, color = type, group = type),
             position = dodge,
             size = 3) +
  geom_text(data = n_labels_T3_fe,
            aes(x = Inf, y = depth_f, label = paste0("n=", n), color = type, group = type),
            position = dodge,
            hjust = 1.1, size = 3) +
  facet_wrap(~ site, ncol = 3) +
  scale_y_discrete(labels = function(x) x) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.12))) +  # room for n labels
  coord_cartesian(clip = "off") +
  scale_color_manual(
    values = c("g" = "black", "ng" = "green3", "away" = "royalblue"),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  scale_shape_manual(
    values = c("g" = 16, "ng" = 17, "away" = 15),
    labels = c("g" = "Galvanized", "ng" = "Non-galvanized", "away" = "Away")) +
  labs(x = expression("Total Fe " * (mu*M)),
       y = "Depth (cm)", color = "Treatment", shape = "Treatment") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold", size = 14))


##--- Away only at T3 ---##
dataT3_away_fe_points <- data_avg_fe %>% filter(time_point == "T3", type == "away")
dataT3_away_fe_avg <- data_avg_fe_2 %>% filter(time_point == "T3", type == "away")

# ── n labels for Away at T3 ───────────────────────────────────
# ── n labels for Away at T3 ───────────────────────────────────
n_labels_T3_away_fe <- data_avg_fe %>%
  filter(time_point == "T3", type == "away") %>%
  group_by(site, depth_cm) %>%
  summarise(n = n_distinct(core), .groups = "drop")

ggplot() +
  geom_point(data = dataT3_away_fe_points,
             aes(x = fe_mean, y = depth_cm),
             color = "royalblue", size = 2, alpha = 0.5, shape = 19) +
  geom_errorbarh(data = dataT3_away_fe_avg,
                 aes(xmin = fe_avg - fe_se,
                     xmax = fe_avg + fe_se,
                     y = depth_cm),
                 color = "royalblue", height = 0.3, linewidth = 0.8) +
  geom_point(data = dataT3_away_fe_avg,
             aes(x = fe_avg, y = depth_cm),
             color = "royalblue", shape = 15, size = 3) +
  geom_text(data = n_labels_T3_away_fe,
            aes(x = Inf, y = depth_cm, label = paste0("n=", n)),
            color = "royalblue", hjust = 1.1, size = 3.5) +
  facet_wrap(~ site, ncol = 3) +
  scale_y_reverse() +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  coord_cartesian(ylim = c(max(data_fe$depth_cm, na.rm = TRUE), 0)) +
  labs(x = expression("Total Fe " * (mu*M)), y = "Depth (cm)") +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold", size = 20))

# ── STACK ─────────────────────────────────────────────────────
#psulfide / piron +
  #plot_annotation(tag_levels = "A")

# ── Shared theme ──────────────────────────────────────────────
shared_theme <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    panel.spacing.x = unit(1.5, "cm"),
    plot.margin = margin(t = 5, r = 40, b = 5, l = 5, unit = "pt"))  # <-- wider right margin

n_T3_plot <- n_T3 %>%
  mutate(
    depth_f = factor(depth_cm),
    x_pos = case_when(
      site == "High" ~ 850,       # left-shifted for High
      TRUE ~ Inf))                # right edge for Donor and Low


# In psulfide definition — REMOVE the geom_text layer entirely
# Then in p_sulfide:
p_sulfide <- psulfide + 
  shared_theme +
  geom_text(data = n_T3_plot,
            aes(x = x_pos, y = depth_f, color = type,
                label = paste0("n=", n), group = type),
            hjust = 1.1, size = 3.5,
            position = dodge)
p_iron <- piron + shared_theme
# ── STACK ─────────────────────────────────────────────────────
p_sulfide / p_iron +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16, face = "bold"))


