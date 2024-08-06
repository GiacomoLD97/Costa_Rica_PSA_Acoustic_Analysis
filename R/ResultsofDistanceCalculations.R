rm(list = ls())

library(ggpubr)
library(tidyverse)

### 2. LOAD AND REFORMAT DATA ##############################
Dist2Pasture <- read.csv("data/wasserstein10min_freq_toPasture.csv")

Nat2Past <- Dist2Pasture %>%
  select(nearest_10, distance_nat, freq_category) %>%
  mutate(Type = "Natural_Regeneration") %>%
  rename(Timebin = nearest_10, Distance = distance_nat, FrequencyCat = freq_category)

Ref2Past <- Dist2Pasture %>%
  select(nearest_10, distance_ref, freq_category) %>%
  mutate(Type = "Reference_Forest") %>%
  rename(Timebin = nearest_10, Distance = distance_ref, FrequencyCat = freq_category)

Plant2Past <- Dist2Pasture %>%
  select(nearest_10, distance_plant, freq_category) %>%
  mutate(Type = "Plantation") %>%
  rename(Timebin = nearest_10, Distance = distance_plant, FrequencyCat = freq_category)

Dist2PastRows <- bind_rows(Nat2Past, Ref2Past, Plant2Past)

Dist2Reference <- read.csv("data/wasserstein10min_freq_toRefForest.csv")

Nat2Ref <- Dist2Reference %>%
  select(nearest_10, distance_nat, freq_category) %>%
  mutate(Type = "Natural_Regeneration") %>%
  rename(Timebin = nearest_10, Distance = distance_nat, FrequencyCat = freq_category)

Past2Ref <- Dist2Reference %>%
  select(nearest_10, distance_past, freq_category) %>%
  mutate(Type = "Pasture") %>%
  rename(Timebin = nearest_10, Distance = distance_past, FrequencyCat = freq_category)

Plant2Ref <- Dist2Reference %>%
  select(nearest_10, distance_plant, freq_category) %>%
  mutate(Type = "Plantation") %>%
  rename(Timebin = nearest_10, Distance = distance_plant, FrequencyCat = freq_category)

Dist2RefRows <- bind_rows(Nat2Ref, Past2Ref, Plant2Ref)

### 3. INITIAL VISUALIZE  ##############################

# Average across all frequency band per timebin and then feed into a boxplot to get daily average distances

Dist2RefRows %>% group_by(Timebin, Type) %>% mutate(meanDistance = mean(Distance)) %>% 
  subset(select = -c(Distance, FrequencyCat)) %>% unique() %>% ungroup() %>% 
  ggboxplot(., x = "Type", y = "meanDistance", add = "jitter")

Dist2PastRows %>% group_by(Timebin, Type) %>% mutate(meanDistance = mean(Distance)) %>% 
  subset(select = -c(Distance, FrequencyCat)) %>% unique() %>% ungroup() %>%
  ggboxplot(., x = "Type", y = "meanDistance", add = "jitter")

#Load included times from Modelling Script
includedtimes <- read.csv("data/identifiedminutes.csv") %>% 
  filter(include == "TRUE") %>% 
  pull(nearest_10) %>% unique()

# Use this to trim distance values to the region of acoustic space we're interested in
TrimDist2Past <- Dist2PastRows %>%
  filter(FrequencyCat %in% c("3-4 kHz", "4-5 kHz", "5-6 kHz", "6-7 kHz", "7-8 kHz", "8-9 kHz")) %>%
  filter(Timebin %in% includedtimes) %>%
  group_by(Timebin, Type) %>%
  summarise(meanDistance = mean(Distance), .groups = 'drop') %>%
  distinct()

TrimDist2Ref <- Dist2RefRows %>%
  filter(FrequencyCat %in% c("3-4 kHz", "4-5 kHz", "5-6 kHz", "6-7 kHz", "7-8 kHz", "8-9 kHz")) %>%
  filter(Timebin %in% includedtimes) %>%
  group_by(Timebin, Type) %>%
  summarise(meanDistance = mean(Distance), .groups = 'drop') %>%
  distinct()

# Plot
# Distances to Pasture
ggplot(TrimDist2Past, aes(Timebin, meanDistance, color = Type)) + geom_point()
ggboxplot(TrimDist2Past, x = "Type", y = "meanDistance", add = "jitter")

# Distances to Reference
ggplot(TrimDist2Ref, aes(Timebin, meanDistance, color = Type)) + geom_point()
ggboxplot(TrimDist2Ref, x = "Type", y = "meanDistance", add = "jitter")

### 4. QUANTIFICATIONS AND STATISTICAL TESTS  ##############################

#Having grouped the data into meanDistances per Timebin allows us to use Friedman test for repeated measures
friedman.test(meanDistance~Type|Timebin, data=TrimDist2Past)
results.fried.Past <- TrimDist2Past %>% friedman_test(meanDistance~Type|Timebin)

friedman.test(meanDistance~Type|Timebin, data=TrimDist2Ref)
results.fried.Ref <- TrimDist2Ref %>% friedman_test(meanDistance~Type|Timebin)

#Post Hoc
pairwise.wilcox.test(TrimDist2Past$meanDistance, TrimDist2Past$Type, p.adj = "bonf")
pwc.Past <- TrimDist2Past %>% wilcox_test(as.formula(paste("meanDistance", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Past
#Nat Regen and plantation both closer to pasture than reference forest, but non-sign different from each other

pairwise.wilcox.test(TrimDist2Ref$meanDistance, TrimDist2Ref$Type, p.adj = "bonf")
pwc.Ref <- TrimDist2Ref %>% wilcox_test(as.formula(paste("meanDistance", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Ref
#gradient of distance from pasture closest is plantation then nat regen then reference forest, differences between groups sign


#Is Pasture/Ref distance higher with low frequencies included?
PasttoRefall <- Dist2PastRows %>%
  filter(Type == "Natural_Regeneration") %>%
  mutate(Split = "All")

PasttoRefupper <- PasttoRefall %>%
  filter(!FrequencyCat %in% c("0-1 kHz", "1-2 kHz", "2-3 kHz")) %>%
  mutate(Split = "Upper")

PasttoRefall %>% get_summary_stats(Distance, type = "mean_sd")
PasttoRefupper %>% get_summary_stats(Distance, type = "mean_sd")

PasttoRef <- rbind(PasttoRefall, PasttoRefupper)

t.test(Distance~Split, data = PasttoRef)
#Yes, showing that including the lower frequencies overestimates the differences between the two soundscapes

#We can see that distances peak in the lowest and middle freuencies
ggboxplot(PasttoRef, x = "FrequencyCat", y = "Distance", add = "jitter")

### 5. FIGURE 3B  ##############################
#group times into periods of the day to visualize where distances are largest 
# Max distance is used to scale the distances for visualization
maxdist <- max(TrimDist2Ref$meanDistance)

TrimDist2Ref <- TrimDist2Ref %>%
  mutate(
    Period = case_when(
      Timebin >= 290 & Timebin <= 350 ~ "Dawn",
      Timebin >= 1050 & Timebin <= 1110 ~ "Dusk",
      Timebin > 350 & Timebin < 1050 ~ "Day",
      TRUE ~ "Night"
    ),
    ScaledDistance = meanDistance / maxdist,
    Type = ifelse(grepl("Nat", Type, ignore.case = TRUE), "Natural Regeneration", Type),
    Type = factor(Type, levels = c("Natural Regeneration", "Plantation", "Pasture"))
  )

#redo statistical tests and plot
results.fried.Ref <- TrimDist2Ref %>% friedman_test(ScaledDistance~Type|Timebin)
pwc.Ref <- TrimDist2Ref %>% wilcox_test(as.formula(paste("ScaledDistance", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")

pwc.Ref <- pwc.Ref %>% add_xy_position(x = "Type")

ggboxplot(TrimDist2Ref, x = "Type", y = "ScaledDistance", add = "boxplot") +
  geom_jitter(aes(color = Period), 
              alpha = 0.5,
              position = position_jitter(0.2)) +
  stat_pvalue_manual(pwc.Ref, hide.ns = TRUE) +
  labs(
    x = "Intervention Type",
    y = "Scaled Distance",
    subtitle = get_test_label(results.fried.Ref, detailed = TRUE),
    caption = get_pwc_label(pwc.Ref)
  )

ggsave('figures/Figure3B_boxplot.pdf')
