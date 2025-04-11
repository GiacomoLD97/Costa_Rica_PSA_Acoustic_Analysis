#Trying the comparison with all the minutes

rm(list = ls())

### 1. Load Required Packages ##############################

library(ggpubr)
library(tidyverse)
library(rstatix)


### 2. LOAD AND PROCESS DATA ##############################

### 2.1 Distances to Baseline and Scaling ##############################

#Pastures 
Dist2Pasture <- read.csv("/Users/giacomodelgado/Downloads/wasserstein10min_freq_toPasture.csv")
#Reference Forest
Dist2Reference <- read.csv("/Users/giacomodelgado/Downloads/wasserstein10min_freq_toRefForest.csv")

#Rename
colnames(Dist2Pasture) <- c("Timebin", "Natural_Regeneration", "Reference_Forest", "Plantation", "Closest", "FrequencyCat")
colnames(Dist2Reference) <- c("Timebin", "Natural_Regeneration", "Pasture", "Plantation", "Closest", "FrequencyCat")

#Trim
selectedfreqs <- c("1-2 kHz", "2-3 kHz", "3-4 kHz", "4-5 kHz", "5-6 kHz", "6-7 kHz", "7-8 kHz", "8-9 kHz")

#Not removing any times
#includedtimes <- read.csv("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/identifiedminutes.csv") %>% 
#  subset(select = nearest_10) 
#includedtimes <- includedtimes[,1]

Dist2Pasture <- Dist2Pasture %>%
  subset(FrequencyCat %in% selectedfreqs)#%>% 
  #subset(Timebin %in% includedtimes) 
Dist2Reference <- Dist2Reference %>%
  subset(FrequencyCat %in% selectedfreqs) #%>% 
  #subset(Timebin %in% includedtimes) 

#Dataframe for Distances to Pasture
Dist2PastRows <- Dist2Pasture %>% subset(select = -Closest) %>% pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")
#Dataframe for Distances to Reference
Dist2RefRows <- Dist2Reference %>% subset(select = -Closest) %>% pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")

#Scale Distance values into Similarity values
#Use Inverse exponential scaling: y=e^-kx
#As average values are large, use a k of 0.00001 to achieve normal distributions
Dist2PastRows <- Dist2PastRows %>% mutate(Similarity = exp(-Distance * 0.00001))
Dist2RefRows <- Dist2RefRows %>% mutate(Similarity = exp(-Distance * 0.00001))

### 2.2 Closest type for each TFB ##############################

Closesttypes <- Dist2Pasture %>% subset(select = c(Timebin, FrequencyCat, Closest))
colnames(Closesttypes)[3] <- "ClosesttoPasture"
Closesttypes <- Dist2Reference %>% subset(select = c(Timebin, FrequencyCat, Closest)) %>%
  merge(., Closesttypes, by = c("Timebin", "FrequencyCat"))
colnames(Closesttypes)[3] <- "ClosesttoReference"


### 2.3 Dataframe for each of the PES types ##############################

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

#Plantation
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

### 3. QUANTIFICATIONS AND STATISTICAL TESTS  ##############################

#Our dataframes of interest are as follows:
# For distances to baselines: Dist2RefRows, Dist2PastRows
# For categorical closest type to baseline: Closesttypes
# For distances from each PES: DistNatReg, DistPlant

### 3.1 Differences in similarity to baselines from all other classes ##############################

#Having grouped the data into meanSimilaritys per Timebin allows us to use Friedman test for repeated measures

### 3.1.a Pastures ##############################
results.fried.Past <- Dist2PastRows %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.Past
#Post Hoc
pwc.Past <- Dist2PastRows %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Past
#No difference in similarity between NR and Plant to Past

### 3.1.b Reference Forests ##############################
results.fried.Ref <- Dist2RefRows %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.Ref
#Post Hoc
pwc.Ref <- Dist2RefRows %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Ref
#All significant


### 3.2 Differences in Similarity to both baselines within PES classes ##############################


### 3.2.a Natural Regeneration ##############################
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
#NR statistically closer to Reference than to Pasture

### 3.2.b Plantation ##############################
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

### 4. Differential Distances   ##############################

mean(DistNatReg$DistDiff)
mean(DistNatReg$DistancetoRef)
mean(DistNatReg$DistancetoPast)

mean(DistPlant$DistDiff)
mean(DistPlant$DistancetoRef)
mean(DistPlant$DistancetoPast)

### 5. Figures   ##############################

### 5.1 Figure 3A ##############################

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
  stat_pvalue_manual(pwc.Ref, hide.ns = TRUE) +
  labs(
    x = "Intervention Type",
    y = "Scaled Similarity",
    subtitle = get_test_label(results.fried.Ref, detailed = TRUE),
    caption = get_pwc_label(pwc.Ref)
  ) 
#ggsave('figures/Figure3A_boxplot.pdf')

### 5.2 Possible extra supplementary figure 4 ##############################

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

