rm(list = ls())

library(ggpubr)
library(tidyverse)

#<<<<<<< HEAD
### 2. LOAD AND PROCESS DATA ##############################

#Pastures 
Dist2Pasture <- read.csv("/Users/giacomodelgado/Downloads/wasserstein10min_freq_toPasture.csv")
#Reference Forest
Dist2Reference <- read.csv("/Users/giacomodelgado/Downloads/wasserstein10min_freq_toRefForest.csv")

#Rename
colnames(Dist2Pasture) <- c("Timebin", "Natural_Regeneration", "Reference_Forest", "Plantation", "Closest", "FrequencyCat")
colnames(Dist2Reference) <- c("Timebin", "Natural_Regeneration", "Pasture", "Plantation", "Closest", "FrequencyCat")

#Trim
selectedfreqs <- c("1-2 kHz", "2-3 kHz", "3-4 kHz", "4-5 kHz", "5-6 kHz", "6-7 kHz", "7-8 kHz", "8-9 kHz")
includedtimes <- read.csv("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/identifiedminutes.csv")
includedtimes <- includedtimes %>% subset(include == "TRUE") %>% subset(select = nearest) 
includedtimes <- includedtimes[,1]

Dist2Pasture <- Dist2Pasture %>%
  subset(FrequencyCat %in% selectedfreqs) %>% 
  subset(Timebin %in% includedtimes) 
Dist2Reference <- Dist2Reference %>%
  subset(FrequencyCat %in% selectedfreqs) %>% 
  subset(Timebin %in% includedtimes) 

# Dataframe for closest type by Time-Frequency Bin
Closesttypes <- Dist2Pasture %>% subset(select = c(Timebin, FrequencyCat, Closest))
colnames(Closesttypes)[3] <- "ClosesttoPasture"
Closesttypes <- Dist2Reference %>% subset(select = c(Timebin, FrequencyCat, Closest)) %>%
  merge(., Closesttypes, by = c("Timebin", "FrequencyCat"))
colnames(Closesttypes)[3] <- "ClosesttoReference"

#Number of TFI in which each intervention was closest 
#To reference
Closesttypes %>% subset(select = ClosesttoReference) %>% count()
#To Pasture
Closesttypes %>% subset(select = ClosesttoPasture) %>% count()
#=======
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
#>>>>>>> 8cdec5ccafbec51a7df95771d361d96709a6cafe

TrimDist2Ref <- Dist2RefRows %>%
  filter(FrequencyCat %in% c("3-4 kHz", "4-5 kHz", "5-6 kHz", "6-7 kHz", "7-8 kHz", "8-9 kHz")) %>%
  filter(Timebin %in% includedtimes) %>%
  group_by(Timebin, Type) %>%
  summarise(meanDistance = mean(Distance), .groups = 'drop') %>%
  distinct()

#<<<<<<< HEAD
#Dataframe for Distances to Pasture
Dist2PastRows <- Dist2Pasture %>% subset(select = -Closest) %>% pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")
#Dataframe for Distances to Reference
Dist2RefRows <- Dist2Reference %>% subset(select = -Closest) %>% pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")

#Scale Distance values into Similarity values
#Use Inverse exponential scaling: y=e^-kx
#As average values are large, use a k of 0.00001 to achieve normal distributions
Dist2PastRows <- Dist2PastRows %>% mutate(Similarity = exp(-Distance * 0.00001))
Dist2RefRows <- Dist2RefRows %>% mutate(Similarity = exp(-Distance * 0.00001))


#Dataframe for each of the PES types
#Natural Regeneration
DistNatReg <- Dist2PastRows %>% subset(Type == "Natural_Regeneration") %>%
  subset(select = -Type)
colnames(DistNatReg)[3:4] <- c("DistancetoPast", "Sim2Pasture")
temp <- Dist2RefRows %>% subset(Type == "Natural_Regeneration") %>%
  subset(select = -Type)
colnames(temp)[3:4] <- c("DistancetoRef", "Sim2Ref")
DistNatReg <- merge(DistNatReg, temp, by = c("Timebin", "FrequencyCat"))
#Add the Distance Differential
DistNatReg <- DistNatReg %>% mutate(DistDiff = ifelse(DistancetoRef < DistancetoPast, 
       ((DistancetoPast-DistancetoRef)/DistancetoRef)*100, 
       ((DistancetoPast-DistancetoRef)/DistancetoPast)*100))

#Natural Regeneration
DistPlant <- Dist2PastRows %>% subset(Type == "Plantation") %>%
  subset(select = -Type)
colnames(DistPlant)[3:4] <- c("DistancetoPast", "Sim2Pasture")
temp <- Dist2RefRows %>% subset(Type == "Plantation") %>%
  subset(select = -Type)
colnames(temp)[3:4] <- c("DistancetoRef", "Sim2Ref")
DistPlant <- merge(DistPlant, temp, by = c("Timebin", "FrequencyCat"))
#Add the Distance Differential
DistPlant <- DistPlant %>% mutate(DistDiff = ifelse(DistancetoRef < DistancetoPast, 
                                                      ((DistancetoPast-DistancetoRef)/DistancetoRef)*100, 
                                                      ((DistancetoPast-DistancetoRef)/DistancetoPast)*100))
#=======
# Plot
# Distances to Pasture
ggplot(TrimDist2Past, aes(Timebin, meanDistance, color = Type)) + geom_point()
ggboxplot(TrimDist2Past, x = "Type", y = "meanDistance", add = "jitter")

# Distances to Reference
ggplot(TrimDist2Ref, aes(Timebin, meanDistance, color = Type)) + geom_point()
ggboxplot(TrimDist2Ref, x = "Type", y = "meanDistance", add = "jitter")

### 4. QUANTIFICATIONS AND STATISTICAL TESTS  ##############################
#>>>>>>> 8cdec5ccafbec51a7df95771d361d96709a6cafe

#Our dataframes of interest are as follows:
# For distances to baselines: Dist2RefRows, Dist2PastRows
# For categorical closest type to baseline: Closesttypes
# For distances from each PES: DistNatReg, DistPlant

### 3. INITIAL VISUALIZATION  ##############################

# Overall similarities
Dist2RefRows %>%
  ggboxplot(., x = "Type", y = "Similarity", add = "jitter")
Dist2RefRows %>% group_by(Type) %>% summarise(avgSimilarity = mean(Similarity))


Dist2PastRows %>%
  ggboxplot(., x = "Type", y = "Similarity", add = "jitter")
Dist2PastRows %>% group_by(Type) %>% summarise(avgSimilarity = mean(Similarity))


### 4. STATISTICAL TESTS  ##############################

#Testing for significant in distances to baselines among interventions
#Having grouped the data into meanSimilaritys per Timebin allows us to use Friedman test for repeated measures

#To Pasture
results.fried.Past <- Dist2PastRows %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.Past
#Post Hoc
pwc.Past <- Dist2PastRows %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Past

#To Reference
results.fried.Ref <- Dist2RefRows %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.Ref
#Post Hoc
pwc.Ref <- Dist2RefRows %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Ref


#<<<<<<< HEAD

#Testing for significance in distance to baseline within interventions
#Natural Regeneration
results.fried.nat <- DistNatReg %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>% unique() %>% 
  friedman_test(meanDistance~Baseline|Timebin)
results.fried.nat 
#Post Hoc
pwc.Nat <- DistNatReg %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>%
  wilcox_test(as.formula(paste("meanDistance", "Baseline", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Nat

#Plantation
results.fried.plant <- DistPlant %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>% unique() %>% 
  friedman_test(meanDistance~Baseline|Timebin)
results.fried.plant
#Post Hoc
pwc.plant <- DistPlant %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>%
  wilcox_test(as.formula(paste("meanDistance", "Baseline", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.plant

### 5. FIGURE 3B  ##############################

custom_order <- c("Pasture", "Plantation", "Natural_Regeneration")
Dist2RefRows$Type <- factor(Dist2RefRows$Type, levels = custom_order)
labels <- c("Pasture", "Plantation", "Natural Regeneration")

pwc.Ref <- pwc.Ref %>% mutate(y.position = c(0.9, 1.0, 0.95))

means_df <- Dist2RefRows %>% 
  group_by(Type) %>% 
  summarise(meanSimilarity = mean(Similarity, na.rm = TRUE))

Dist2RefRows %>% 
  group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% 
  subset(select = c(Type, Timebin, meanSimilarity)) %>%
  mutate(Period = case_when(
    `Timebin` >= 290 & `Timebin` <= 350 ~ "Dawn",
    `Timebin` >= 1050 & `Timebin` <= 1110 ~ "Dusk",
    `Timebin` > 350 & `Timebin` < 1050 ~ "Day",
    TRUE ~ "Night"
  )) %>%
  subset(select = c(Type, meanSimilarity, Period)) %>% 
  unique() %>% 
  ggboxplot(x = "Type", y = "meanSimilarity", add = "boxplot") +
  scale_x_discrete(label = labels) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2)) +
  geom_text(data = means_df, aes(x = Type, y = 0.76, label = paste("Mean", round(meanSimilarity, 2), sep=": ")), 
            color = "black") +  # Add means as text along the same line
#=======
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
#>>>>>>> 8cdec5ccafbec51a7df95771d361d96709a6cafe
  stat_pvalue_manual(pwc.Ref, hide.ns = TRUE) +
  labs(
    x = "Intervention Type",
    y = "Scaled Similarity",
    subtitle = get_test_label(results.fried.Ref, detailed = TRUE),
    caption = get_pwc_label(pwc.Ref)
#<<<<<<< HEAD
  ) +
  ggtitle("Similarity to Reference Forests")

ggsave('/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/figures/Figure3B_boxplot.pdf')

custom_order <- c("Reference_Forest", "Plantation", "Natural_Regeneration")
Dist2PastRows$Type <- factor(Dist2PastRows$Type, levels = custom_order)
labels <- c("Reference Forest", "Plantation", "Natural Regeneration")

pwc.Past <- pwc.Past %>% mutate(y.position = c(0.9, 1.0, 0.95))

means_df <- Dist2PastRows %>% 
  group_by(Type) %>% 
  summarise(meanSimilarity = mean(Similarity, na.rm = TRUE))

Dist2PastRows %>% 
  group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% 
  subset(select = c(Type, Timebin, meanSimilarity)) %>%
  mutate(Period = case_when(
    `Timebin` >= 290 & `Timebin` <= 350 ~ "Dawn",
    `Timebin` >= 1050 & `Timebin` <= 1110 ~ "Dusk",
    `Timebin` > 350 & `Timebin` < 1050 ~ "Day",
    TRUE ~ "Night"
  )) %>%
  subset(select = c(Type, meanSimilarity, Period)) %>% 
  unique() %>% 
  ggboxplot(x = "Type", y = "meanSimilarity", add = "boxplot") +
  scale_x_discrete(label = labels) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2)) +
  geom_text(data = means_df, aes(x = Type, y = 0.76, label = paste("Mean", round(meanSimilarity, 2), sep=": ")), 
            color = "black") +  # Add means as text along the same line
  stat_pvalue_manual(pwc.Past, hide.ns = TRUE) +
  labs(
    x = "Intervention Type",
    y = "Scaled Similarity",
    subtitle = get_test_label(results.fried.Past, detailed = TRUE),
    caption = get_pwc_label(pwc.Past)
  ) +
  ggtitle("Similarity to Pastures")

#=======

ggsave('figures/Figure3B_boxplot.pdf')
#>>>>>>> 8cdec5ccafbec51a7df95771d361d96709a6cafe
