##############

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


################cut code from growth code #####################
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

#high low and donor just no T3
#only high and low adn T1 / T2 as donor site had unreliable collection and t3 pinpricks should have grown off so 0 growth is not accurate
T1_2 <- growth3 %>%
  filter(site %in% c("high", "low", "donor")) %>%
  filter(collection_point %in% c("t1", "t2"))

n_labelsT12 <- T1_2 %>%
  group_by(site, collection_point, treatment) %>%
  summarise(
    n = n(),
    max_rgr = suppressWarnings(max(RGR, na.rm = TRUE)),
    .groups = "drop") %>%
  mutate(
    y_pos = if_else(is.infinite(max_rgr), 0.05, max_rgr + 0.05))

rgr_plot <- T1_2 %>%
  ggplot(aes(x = collection_point, y = RGR, fill = treatment)) +
  geom_boxplot() +
  geom_point(
    aes(color = treatment),
    position = position_dodge(width = 0.75),
    size = 1.5,
    alpha = 0.6) +
  geom_text(
    data = n_labelsT12,
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
