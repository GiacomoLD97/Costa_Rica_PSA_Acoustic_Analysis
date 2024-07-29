rm(list = ls())
library(ggpubr)
library(rstatix)
library(stats)
library(utils)
library(tidyverse)

### 2. LOAD AND REFORMAT DATA ##############################

Dist2Pasture <- read.csv("data/wasserstein10min_freq_toPasture.csv")

Nat2Past <- Dist2Pasture %>% subset(select = c(nearest_10, distance_nat, freq_category))
Nat2Past$Type <- "Natural_Regeneration"
colnames(Nat2Past) <- c("Timebin", "Distance", "FrequencyCat", "Type")
Ref2Past <- Dist2Pasture %>% subset(select = c(nearest_10, distance_ref, freq_category))
Ref2Past$Type <- "Reference_Forest"
colnames(Ref2Past) <- c("Timebin", "Distance", "FrequencyCat", "Type")
Plant2Past <- Dist2Pasture %>% subset(select = c(nearest_10, distance_plant, freq_category))
Plant2Past$Type <- "Plantation"
colnames(Plant2Past) <- c("Timebin", "Distance", "FrequencyCat", "Type")

Dist2PastRows <- rbind(Nat2Past, Ref2Past, Plant2Past)


Dist2Reference <- read.csv("/Users/giacomodelgado/Downloads/wasserstein10min_freq_toRefForest.csv")

Nat2Ref <- Dist2Reference %>% subset(select = c(nearest_10, distance_nat, freq_category))
Nat2Ref$Type <- "Natural_Regeneration"
colnames(Nat2Ref) <- c("Timebin", "Distance", "FrequencyCat", "Type")
Past2Ref <- Dist2Reference %>% subset(select = c(nearest_10, distance_past, freq_category))
Past2Ref$Type <- "Pasture"
colnames(Past2Ref) <- c("Timebin", "Distance", "FrequencyCat", "Type")
Plant2Ref <- Dist2Reference %>% subset(select = c(nearest_10, distance_plant, freq_category))
Plant2Ref$Type <- "Plantation"
colnames(Plant2Ref) <- c("Timebin", "Distance", "FrequencyCat", "Type")

Dist2RefRows <- rbind(Nat2Ref, Past2Ref, Plant2Ref)



### 3. INITIAL VISUALIZE  ##############################

# Simply average across all frequency band per timebin and then feed into a boxplot to get daily average distances

Dist2RefRows %>% group_by(Timebin, Type) %>% mutate(meanDistance = mean(Distance)) %>% 
  subset(select = -c(Distance, FrequencyCat)) %>% unique() %>% ungroup() %>% 
  ggboxplot(., x = "Type", y = "meanDistance", add = "jitter")

Dist2PastRows %>% group_by(Timebin, Type) %>% mutate(meanDistance = mean(Distance)) %>% 
  subset(select = -c(Distance, FrequencyCat)) %>% unique() %>% ungroup() %>%
  ggboxplot(., x = "Type", y = "meanDistance", add = "jitter")

#Load included times from Modelling Script

includedtimes <- read.csv("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/identifiedminutes.csv")
includedtimes <- includedtimes %>% subset(include == "TRUE") %>% subset(select = nearest) %>% unique() %>% as.vector()

# Use this to trim distance values to the region of acoustic space we're interested in


TrimDist2Past <- Dist2PastRows %>% subset(FrequencyCat %in% c("3-4 kHz", "4-5 kHz", "5-6 kHz", "6-7 kHz", "7-8 kHz", "8-9 kHz")) %>% 
  subset(Timebin %in% includedtimes$nearest) %>% group_by(Timebin, Type) %>% mutate(meanDistance = mean(Distance)) %>% 
  subset(select = -c(Distance, FrequencyCat)) %>% unique() %>% ungroup()

TrimDist2Ref <- Dist2RefRows %>% subset(FrequencyCat %in% c("3-4 kHz", "4-5 kHz", "5-6 kHz", "6-7 kHz", "7-8 kHz", "8-9 kHz")) %>% 
  subset(Timebin %in% includedtimes$nearest) %>% group_by(Timebin, Type) %>% mutate(meanDistance = mean(Distance)) %>% 
  subset(select = -c(Distance, FrequencyCat)) %>% unique() %>% ungroup()

#Quick Plot
#Distances to Pasture
ggplot(TrimDist2Past, aes(Timebin, meanDistance, color = Type)) + geom_point()
ggboxplot(TrimDist2Past, x = "Type", y = "meanDistance", add = "jitter")

#Distances to Reference
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
PasttoRefall <- Dist2PastRows %>% subset(Type == "Natural_Regeneration") %>% mutate(Split = "All") 
PasttoRefupper <- PasttoRefall %>% subset(FrequencyCat %notin% c("0-1 kHz", "1-2 kHz", "2-3 kHz")) %>% mutate(Split = "Upper")

PasttoRefall %>% get_summary_stats(Distance, type = "mean_sd")
PasttoRefupper %>% get_summary_stats(Distance, type = "mean_sd")

PasttoRef <- rbind(PasttoRefall, PasttoRefupper)

t.test(Distance~Split, data = PasttoRef)
#Yes, showing that including the lower frequencies overestimates the differences between the two soundscapes

#We can see that distances peak in the lowest and middle freuencies
ggboxplot(PasttoRef, x = "FrequencyCat", y = "Distance", add = "jitter")



### 5. FIGURE 3B  ##############################


#group times into periods of the day to visualize where distances are largest 
TrimDist2Ref <- TrimDist2Ref %>%
  mutate(Period = case_when(
    `Timebin` >= 290 & `Timebin` <= 350 ~ "Dawn",
    `Timebin` >= 1050 & `Timebin` <= 1110 ~ "Dusk",
    `Timebin` > 350 & `Timebin` < 1050 ~ "Day",
    TRUE ~ "Night"
  ))


#convert distances to scaled values 
maxdist<-max(TrimDist2Ref$meanDistance)
TrimDist2Ref <- TrimDist2Ref %>% mutate(ScaledDistance = meanDistance/maxdist)

#redo statistical tests and plot
results.fried.Ref <- TrimDist2Ref %>% friedman_test(ScaledDistance~Type|Timebin)
pwc.Ref <- TrimDist2Ref %>% wilcox_test(as.formula(paste("ScaledDistance", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")

pwc.Ref <- pwc.Ref %>% add_xy_position(x = "Type")


TrimDist2Ref$Type <- ifelse(grepl("Nat", TrimDist2Ref$Type, ignore.case = TRUE), "Natural Regeneration", TrimDist2Ref$Type)

custom_order <- c("Natural Regeneration", "Plantation", "Pasture")
TrimDist2Ref$Type <- factor(TrimDist2Ref$Type, levels = custom_order)

ggboxplot(TrimDist2Ref, x = "Type", y = "ScaledDistance", add = "boxplot") +
  geom_jitter(aes(color = Period), position = position_jitter(0.2)) +
  stat_pvalue_manual(pwc.Ref, hide.ns = TRUE) +
  labs(
    x = "Intervention Type",
    y = "Scaled Distance",
    subtitle = get_test_label(results.fried.Ref, detailed = TRUE),
    caption = get_pwc_label(pwc.Ref)
  )


ggsave('/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/figures/Figure3B_boxplot.pdf')
