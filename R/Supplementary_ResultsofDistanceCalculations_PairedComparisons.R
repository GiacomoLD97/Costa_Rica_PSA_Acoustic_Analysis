#Rerunning statistical tests on paired sites
rm(list = ls())
### 1. Install and Load packages #####
library(pacman)
p_load(dplyr)
p_load(tidyr)
p_load(ggpubr)
p_load(rstatix)
p_load(ggplot2)


### 2. Distance Analysis Grouped by Slope #####

#### 2.1 Load and Format Data #####

#To reference
dist2refbyslope <- read.csv("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/20250505_wasserstein_toRefForest_byslope.csv")
colnames(dist2refbyslope) <- c("Timebin", "Natural_Regeneration", "Pasture", "Plantation", "Closest", "FrequencyCat", "SlopeCat")
#To pasture
dist2pastbyslope <- read.csv("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/20250506_wasserstein_toPasture_byslope.csv")
colnames(dist2pastbyslope) <- c("Timebin", "Natural_Regeneration", "Reference_Forest", "Plantation", "Closest", "FrequencyCat", "SlopeCat")


#Trim
selectedfreqs <- c("1-2 kHz", "2-3 kHz", "3-4 kHz", "4-5 kHz", "5-6 kHz", "6-7 kHz", "7-8 kHz", "8-9 kHz")
includedtimes <- read.csv("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/identifiedminutes.csv") %>% 
  subset(select = nearest_10) 
includedtimes <- includedtimes[,1]

#To Reference
dist2refbyslope <- dist2refbyslope %>%
  subset(FrequencyCat %in% selectedfreqs) %>% 
  subset(Timebin %in% includedtimes) 

#To Pasture
dist2pastbyslope <- dist2pastbyslope %>%
  subset(FrequencyCat %in% selectedfreqs) %>% 
  subset(Timebin %in% includedtimes) 

#Format data into slope groups
#High slope Ref
dist2refslopehigh <- dist2refbyslope %>% subset(SlopeCat == "High") %>% 
  subset(select = -c(Closest, SlopeCat)) %>% 
  pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")
#Scale the similarity values
dist2refslopehigh <- dist2refslopehigh %>% mutate(Similarity = exp(-Distance * 0.00001))

#High slope Pasture
dist2pastslopehigh <- dist2pastbyslope %>% subset(SlopeCat == "High") %>% 
  subset(select = -c(Closest, SlopeCat)) %>% 
  pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")
#Scale the similarity values
dist2pastslopehigh <- dist2pastslopehigh %>% mutate(Similarity = exp(-Distance * 0.00001))

#Low slope Ref
dist2refslopelow <- dist2refbyslope %>% subset(SlopeCat == "Low") %>% 
  subset(select = -c(Closest, SlopeCat)) %>% 
  pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")
#Scale the similarity values
dist2refslopelow <- dist2refslopelow %>% mutate(Similarity = exp(-Distance * 0.00001))

#Low slope Pasture
dist2pastslopelow <- dist2pastbyslope %>% subset(SlopeCat == "Low") %>% 
  subset(select = -c(Closest, SlopeCat)) %>% 
  pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")
#Scale the similarity values
dist2pastslopelow <- dist2pastslopelow %>% mutate(Similarity = exp(-Distance * 0.00001))


#### 2.2 Similarity to Reference Forests Slope Groups #####

##### 2.2.a Similarity to Reference Forests: High Slope #####

#Statistical Test and Post hoc
results.fried.Ref.highslope <- dist2refslopehigh %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.Ref.highslope
#Post Hoc
pwc.Ref.highslope <- dist2refslopehigh %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Ref.highslope
#All significant

custom_order <- c("Pasture", "Plantation", "Natural_Regeneration")
dist2refslopehigh$Type <- factor(dist2refslopehigh$Type, levels = custom_order)
labels <- c("Pasture", "Plantation", "Natural Regeneration")

pwc.Ref.highslope <- pwc.Ref.highslope %>% mutate(y.position = c(0.9, 1.0, 0.95))

means_ref.highslope <- dist2refslopehigh %>% 
  group_by(Type) %>% 
  summarise(meanSimilarity = mean(Similarity, na.rm = TRUE))

dist2refslopehigh %>% 
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
  ggplot(aes(x = Type, y = meanSimilarity)) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2), alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.5, linetype = "solid", color = "black") +
  geom_text(data = means_ref.highslope, aes(x = Type, y = 0.76, 
                                          label = paste("Mean", round(meanSimilarity, 2), sep=": ")), 
            color = "black") +
  scale_x_discrete(label = labels) +
  stat_pvalue_manual(pwc.Ref.highslope, hide.ns = TRUE) +
  labs(
    title = "Acoustic Similarity to Reference for sites that have high slopes",
    x = "Intervention Type",
    y = "Scaled Similarity",
    subtitle = get_test_label(results.fried.Ref.highslope, detailed = TRUE),
    caption = get_pwc_label(pwc.Ref.highslope)
  ) +
  theme_classic2()



##### 2.2.b Similarity to Reference Forests: Low Slope #####

#Statistical Test and Post hoc
results.fried.Ref.lowslope <- dist2refslopelow %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.Ref.lowslope
#Post Hoc
pwc.Ref.lowslope <- dist2refslopelow %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Ref.lowslope
#All significant

custom_order <- c("Pasture", "Plantation", "Natural_Regeneration")
dist2refslopelow$Type <- factor(dist2refslopelow$Type, levels = custom_order)
labels <- c("Pasture", "Plantation", "Natural Regeneration")

pwc.Ref.lowslope <- pwc.Ref.lowslope %>% mutate(y.position = c(0.9, 1.0, 0.95))

means_ref.lowslope <- dist2refslopelow %>% 
  group_by(Type) %>% 
  summarise(meanSimilarity = mean(Similarity, na.rm = TRUE))

dist2refslopelow %>% 
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
  ggplot(aes(x = Type, y = meanSimilarity)) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2), alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.5, linetype = "solid", color = "black") +
  geom_text(data = means_ref.lowslope, aes(x = Type, y = 0.76, 
                                           label = paste("Mean", round(meanSimilarity, 2), sep=": ")), 
            color = "black") +
  scale_x_discrete(label = labels) +
  stat_pvalue_manual(pwc.Ref.lowslope, hide.ns = TRUE) +
  labs(
    title = "Acoustic Similarity to Reference for sites that have Medium slopes",
    x = "Intervention Type",
    y = "Scaled Similarity",
    subtitle = get_test_label(results.fried.Ref.lowslope, detailed = TRUE),
    caption = get_pwc_label(pwc.Ref.lowslope)
  ) +
  theme_classic2()

#### 2.3 Similarity to Pasture Slope Groups #####


##### 2.3.a Similarity to Pastures: High Slope #####

#Statistical Test and Post hoc
results.fried.past.highslope <- dist2pastslopehigh %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.past.highslope
#Post Hoc
pwc.past.highslope <- dist2pastslopehigh %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.past.highslope


custom_order <- c("Reference_Forest", "Natural_Regeneration", "Plantation")
dist2pastslopehigh$Type <- factor(dist2pastslopehigh$Type, levels = custom_order)
labels <- c("Reference Forest", "Natural Regeneration", "Plantation")

pwc.past.highslope <- pwc.past.highslope %>% mutate(y.position = c(0.9, 1.0, 0.95))

means_past.highslope <- dist2pastslopehigh %>% 
  group_by(Type) %>% 
  summarise(meanSimilarity = mean(Similarity, na.rm = TRUE))

dist2pastslopehigh %>% 
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
  ggplot(aes(x = Type, y = meanSimilarity)) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2), alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.5, linetype = "solid", color = "black") +
  geom_text(data = means_past.highslope, aes(x = Type, y = 0.76, 
                                            label = paste("Mean", round(meanSimilarity, 2), sep=": ")), 
            color = "black") +
  scale_x_discrete(label = labels) +
  stat_pvalue_manual(pwc.past.highslope, hide.ns = TRUE) +
  labs(
    title = "Acoustic Similarity to Pasture for sites that have high slopes",
    x = "Intervention Type",
    y = "Scaled Similarity",
    subtitle = get_test_label(results.fried.past.highslope, detailed = TRUE),
    caption = get_pwc_label(pwc.past.highslope)
  ) +
  theme_classic2()



##### 2.3.b Similarity to Pasture: Low Slope #####

#Statistical Test and Post hoc
results.fried.past.lowslope <- dist2pastslopelow %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.past.lowslope
#Post Hoc
pwc.past.lowslope <- dist2pastslopelow %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.past.lowslope
#All significant

custom_order <- c("Reference_Forest", "Natural_Regeneration", "Plantation")
dist2pastslopelow$Type <- factor(dist2pastslopelow$Type, levels = custom_order)
labels <- c("Reference Forest", "Natural Regeneration", "Plantation")

pwc.past.lowslope <- pwc.past.lowslope %>% mutate(y.position = c(0.9, 1.0, 0.95))

means_past.lowslope <- dist2pastslopelow %>% 
  group_by(Type) %>% 
  summarise(meanSimilarity = mean(Similarity, na.rm = TRUE))

dist2pastslopelow %>% 
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
  ggplot(aes(x = Type, y = meanSimilarity)) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2), alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.5, linetype = "solid", color = "black") +
  geom_text(data = means_past.lowslope, aes(x = Type, y = 0.76, 
                                           label = paste("Mean", round(meanSimilarity, 2), sep=": ")), 
            color = "black") +
  scale_x_discrete(label = labels) +
  stat_pvalue_manual(pwc.past.lowslope, hide.ns = TRUE) +
  labs(
    title = "Acoustic Similarity to pasture for sites that have low slopes",
    x = "Intervention Type",
    y = "Scaled Similarity",
    subtitle = get_test_label(results.fried.past.lowslope, detailed = TRUE),
    caption = get_pwc_label(pwc.past.lowslope)
  ) +
  theme_classic2()

### 3. Distance Analysis Grouped by Distance to Road #####

#### 3.1 Load and format data #####

dist2refbyroad <- read.csv("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/20250505_wasserstein_toRefForest_bydistroad.csv")
colnames(dist2refbyroad) <- c("Timebin", "Natural_Regeneration", "Pasture", "Plantation", "Closest", "FrequencyCat", "RoadDistCat")

dist2pastbyroad <- read.csv("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/20250506_wasserstein_toPasture_bydistroad.csv")
colnames(dist2pastbyroad) <- c("Timebin", "Natural_Regeneration", "Reference_Forest", "Plantation", "Closest", "FrequencyCat", "RoadDistCat")


#Trim
dist2refbyroad <- dist2refbyroad %>%
  subset(FrequencyCat %in% selectedfreqs) %>% 
  subset(Timebin %in% includedtimes)

dist2pastbyroad <- dist2pastbyroad %>%
  subset(FrequencyCat %in% selectedfreqs) %>% 
  subset(Timebin %in% includedtimes) 

#Format data into RoadDist groups

#High RoadDist Reference
dist2refroadhigh <- dist2refbyroad %>% subset(RoadDistCat == "High") %>% 
  subset(select = -c(Closest, RoadDistCat)) %>% 
  pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")
#Scale the similarity values
dist2refroadhigh <- dist2refroadhigh %>% mutate(Similarity = exp(-Distance * 0.00001))

#High RoadDist Pasture
dist2pastroadhigh <- dist2pastbyroad %>% subset(RoadDistCat == "High") %>%
  subset(select = -c(Closest, RoadDistCat)) %>% 
  pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")
#Scale the similarity values
dist2pastroadhigh <- dist2pastroadhigh %>% mutate(Similarity = exp(-Distance * 0.00001))

#Medium RoadDist Reference
dist2refroadmedium <- dist2refbyroad %>% subset(RoadDistCat == "Medium") %>% 
  subset(select = -c(Closest, RoadDistCat)) %>% 
  pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")
#Scale the similarity values
dist2refroadmedium <- dist2refroadmedium %>% mutate(Similarity = exp(-Distance * 0.00001))

#Medium RoadDist Pasture
dist2pastroadmedium <- dist2pastbyroad %>% subset(RoadDistCat == "Medium") %>%
  subset(select = -c(Closest, RoadDistCat)) %>% 
  pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")
#Scale the similarity values
dist2pastroadmedium <- dist2pastroadmedium %>% mutate(Similarity = exp(-Distance * 0.00001))

#Low RoadDist Reference
dist2refroadlow <- dist2refbyroad %>% subset(RoadDistCat == "Low") %>% 
  subset(select = -c(Closest, RoadDistCat)) %>% 
  pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")
#Scale the similarity values
dist2refroadlow <- dist2refroadlow %>% mutate(Similarity = exp(-Distance * 0.00001))

#Low RoadDist Pasture
dist2pastroadlow <- dist2pastbyroad %>% subset(RoadDistCat == "Low") %>%
  subset(select = -c(Closest, RoadDistCat)) %>% 
  pivot_longer(Natural_Regeneration:Plantation, names_to = "Type", values_to = "Distance")
#Scale the similarity values
dist2pastroadlow <- dist2pastroadlow %>% mutate(Similarity = exp(-Distance * 0.00001))






#### 3.2 Similarity to Reference Forests Distance to Road Groups #####

##### 3.2.a Similarity to Reference Forests: High Distance to Roads #####

#Testing for Similarity to References
results.fried.Ref.highroad <- dist2refroadhigh %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.Ref.highroad
#Post Hoc
pwc.Ref.highroad <- dist2refroadhigh %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Ref.highroad
#All significant

custom_order <- c("Pasture", "Plantation", "Natural_Regeneration")
dist2refroadhigh$Type <- factor(dist2refroadhigh$Type, levels = custom_order)
labels <- c("Pasture", "Plantation", "Natural Regeneration")

pwc.Ref.highroad <- pwc.Ref.highroad %>% mutate(y.position = c(0.9, 1.0, 0.95))

means_ref.highroad <- dist2refroadhigh %>% 
  group_by(Type) %>% 
  summarise(meanSimilarity = mean(Similarity, na.rm = TRUE))

dist2refroadhigh %>% 
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
  ggplot(aes(x = Type, y = meanSimilarity)) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2), alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.5, linetype = "solid", color = "black") +
  geom_text(data = means_ref.highroad, aes(x = Type, y = 0.76, 
                                 label = paste("Mean", round(meanSimilarity, 2), sep=": ")), 
            color = "black") +
  scale_x_discrete(label = labels) +
  stat_pvalue_manual(pwc.Ref.highroad, hide.ns = TRUE) +
  labs(
    title = "Acoustic Similarity to Reference for sites that are HIGHLY far from roads",
    x = "Intervention Type",
    y = "Scaled Similarity",
    subtitle = get_test_label(results.fried.Ref.highroad, detailed = TRUE),
    caption = get_pwc_label(pwc.Ref.highroad)
  ) +
  theme_classic2()


##### 3.2.b Similarity to Reference Forests: Medium Distance to Roads #####



#Testing for Similarity to References
results.fried.Ref.mediumroad <- dist2refroadmedium %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.Ref.mediumroad
#Post Hoc
pwc.Ref.mediumroad <- dist2refroadmedium %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Ref.mediumroad
#All significant

custom_order <- c("Pasture", "Plantation", "Natural_Regeneration")
dist2refroadmedium$Type <- factor(dist2refroadmedium$Type, levels = custom_order)
labels <- c("Pasture", "Plantation", "Natural Regeneration")

pwc.Ref.mediumroad <- pwc.Ref.mediumroad %>% mutate(y.position = c(0.9, 1.0, 0.95))

means_ref.mediumroad <- dist2refroadmedium %>% 
  group_by(Type) %>% 
  summarise(meanSimilarity = mean(Similarity, na.rm = TRUE))

dist2refroadmedium %>% 
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
  ggplot(aes(x = Type, y = meanSimilarity)) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2), alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.5, linetype = "solid", color = "black") +
  geom_text(data = means_ref.mediumroad, aes(x = Type, y = 0.76, 
                                          label = paste("Mean", round(meanSimilarity, 2), sep=": ")), 
            color = "black") +
  scale_x_discrete(label = labels) +
  stat_pvalue_manual(pwc.Ref.mediumroad, hide.ns = TRUE) +
  labs(
    title = "Acoustic Similarity to Reference for sites that are MEDIUM far from roads",
    x = "Intervention Type",
    y = "Scaled Similarity",
    subtitle = get_test_label(results.fried.Ref.mediumroad, detailed = TRUE),
    caption = get_pwc_label(pwc.Ref.mediumroad)
  ) +
  theme_classic2()



##### 3.2.c Similarity to Reference Forests: Low Distance to Roads #####


#Testing for Similarity to References
results.fried.Ref.lowroad <- dist2refroadlow %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.Ref.lowroad
#Post Hoc
pwc.Ref.lowroad <- dist2refroadlow %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Ref.lowroad
#All significant

custom_order <- c("Pasture", "Plantation", "Natural_Regeneration")
dist2refroadlow$Type <- factor(dist2refroadlow$Type, levels = custom_order)
labels <- c("Pasture", "Plantation", "Natural Regeneration")

pwc.Ref.lowroad <- pwc.Ref.lowroad %>% mutate(y.position = c(0.9, 1.0, 0.95))

means_ref.lowroad <- dist2refroadlow %>% 
  group_by(Type) %>% 
  summarise(meanSimilarity = mean(Similarity, na.rm = TRUE))

dist2refroadlow %>% 
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
  ggplot(aes(x = Type, y = meanSimilarity)) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2), alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.5, linetype = "solid", color = "black") +
  geom_text(data = means_ref.lowroad, aes(x = Type, y = 0.76, 
                                            label = paste("Mean", round(meanSimilarity, 2), sep=": ")), 
            color = "black") +
  scale_x_discrete(label = labels) +
  stat_pvalue_manual(pwc.Ref.lowroad, hide.ns = TRUE) +
  labs(
    title = "Acoustic Similarity to Reference for sites that are close to roads",
    x = "Intervention Type",
    y = "Scaled Similarity",
    subtitle = get_test_label(results.fried.Ref.lowroad, detailed = TRUE),
    caption = get_pwc_label(pwc.Ref.lowroad)
  ) +
  theme_classic2()


#### 3.3 Similarity to Pastures Distance to Road Groups #####

##### 3.3.a Similarity to Pastures: High Distance to Roads #####

#Testing for Similarity to Pastures
results.fried.past.highroad <- dist2pastroadhigh %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.past.highroad
#Post Hoc
pwc.past.highroad <- dist2pastroadhigh %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.past.highroad
#All significant

custom_order <- c("Reference_Forest", "Natural_Regeneration", "Plantation")
dist2pastroadhigh$Type <- factor(dist2pastroadhigh$Type, levels = custom_order)
labels <- c("Reference Forest", "Natural Regeneration", "Plantation")

pwc.past.highroad <- pwc.past.highroad %>% mutate(y.position = c(0.9, 1.0, 0.95))

means_past.highroad <- dist2pastroadhigh %>% 
  group_by(Type) %>% 
  summarise(meanSimilarity = mean(Similarity, na.rm = TRUE))

dist2pastroadhigh %>% 
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
  ggplot(aes(x = Type, y = meanSimilarity)) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2), alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.5, linetype = "solid", color = "black") +
  geom_text(data = means_past.highroad, aes(x = Type, y = 0.76, 
                                          label = paste("Mean", round(meanSimilarity, 2), sep=": ")), 
            color = "black") +
  scale_x_discrete(label = labels) +
  stat_pvalue_manual(pwc.past.highroad, hide.ns = TRUE) +
  labs(
    title = "Acoustic Similarity to pasture for sites that are HIGHLY far from roads",
    x = "Intervention Type",
    y = "Scaled Similarity",
    subtitle = get_test_label(results.fried.past.highroad, detailed = TRUE),
    caption = get_pwc_label(pwc.past.highroad)
  ) +
  theme_classic2()


##### 3.3.b Similarity to Pastures: Medium Distance to Roads #####

#Testing for Similarity to Pastures
results.fried.past.medroad <- dist2pastroadmedium %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.past.medroad
#Post Hoc
pwc.past.medroad <- dist2pastroadmedium %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.past.medroad
#All significant

custom_order <- c("Reference_Forest", "Natural_Regeneration", "Plantation")
dist2pastroadmedium$Type <- factor(dist2pastroadmedium$Type, levels = custom_order)
labels <- c("Reference Forest", "Natural Regeneration", "Plantation")

pwc.past.medroad <- pwc.past.medroad %>% mutate(y.position = c(0.9, 1.0, 0.95))

means_past.medroad <- dist2pastroadmedium %>% 
  group_by(Type) %>% 
  summarise(meanSimilarity = mean(Similarity, na.rm = TRUE))

dist2pastroadmedium %>% 
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
  ggplot(aes(x = Type, y = meanSimilarity)) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2), alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.5, linetype = "solid", color = "black") +
  geom_text(data = means_past.medroad, aes(x = Type, y = 0.76, 
                                          label = paste("Mean", round(meanSimilarity, 2), sep=": ")), 
            color = "black") +
  scale_x_discrete(label = labels) +
  stat_pvalue_manual(pwc.past.medroad, hide.ns = TRUE) +
  labs(
    title = "Acoustic Similarity to pasture for sites that are MEDIUM far from roads",
    x = "Intervention Type",
    y = "Scaled Similarity",
    subtitle = get_test_label(results.fried.past.medroad, detailed = TRUE),
    caption = get_pwc_label(pwc.past.medroad)
  ) +
  theme_classic2()

##### 3.3.c Similarity to Pastures: Low Distance to Roads #####

#Testing for Similarity to Pastures
results.fried.past.lowroad <- dist2pastroadlow %>% group_by(Type, Timebin) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup()  %>% subset(select = c(Type, Timebin, meanSimilarity)) %>%
  unique() %>% friedman_test(meanSimilarity~Type|Timebin)
results.fried.past.lowroad
#Post Hoc
pwc.past.lowroad <- dist2pastroadlow %>% group_by(Type, Timebin) %>% mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% wilcox_test(as.formula(paste("meanSimilarity", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.past.lowroad
#All significant

custom_order <- c("Reference_Forest", "Natural_Regeneration", "Plantation")
dist2pastroadlow$Type <- factor(dist2pastroadlow$Type, levels = custom_order)
labels <- c("Reference Forest", "Natural Regeneration", "Plantation")

pwc.past.lowroad <- pwc.past.lowroad %>% mutate(y.position = c(0.9, 1.0, 0.95))

means_past.lowroad <- dist2pastroadlow %>% 
  group_by(Type) %>% 
  summarise(meanSimilarity = mean(Similarity, na.rm = TRUE))

dist2pastroadlow %>% 
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
  ggplot(aes(x = Type, y = meanSimilarity)) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2), alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.5, linetype = "solid", color = "black") +
  geom_text(data = means_past.lowroad, aes(x = Type, y = 0.76, 
                                         label = paste("Mean", round(meanSimilarity, 2), sep=": ")), 
            color = "black") +
  scale_x_discrete(label = labels) +
  stat_pvalue_manual(pwc.past.lowroad, hide.ns = TRUE) +
  labs(
    title = "Acoustic Similarity to pasture for sites that are not far from roads (low)",
    x = "Intervention Type",
    y = "Scaled Similarity",
    subtitle = get_test_label(results.fried.past.lowroad, detailed = TRUE),
    caption = get_pwc_label(pwc.past.lowroad)
  ) +
  theme_classic2()



### 4. Differences in Similarity to both baselines within PES classes #####

#### 4.1 Differences in Similarity to both baselines within PES classes by Slope #####

##### 4.1.a Differences to Both Baselines: High Slope #####

###### 4.1.a.1 Differences from Nat Regen to both Baselines #####

#Format data
DistNatRegslopehigh <- dist2pastslopehigh %>% subset(Type == "Natural_Regeneration") %>%
  subset(select = -Type)
colnames(DistNatRegslopehigh)[3:4] <- c("DistancetoPast", "Sim2Pasture")
temp <- dist2refslopehigh %>% subset(Type == "Natural_Regeneration") %>%
  subset(select = -Type)
colnames(temp)[3:4] <- c("DistancetoRef", "Sim2Ref")
DistNatRegslopehigh <- merge(DistNatRegslopehigh, temp, by = c("Timebin", "FrequencyCat"))
#Add the Distance Differential
DistNatRegslopehigh <- DistNatRegslopehigh %>% mutate(DistDiff = ifelse(DistancetoRef < DistancetoPast, 
                                                      ((DistancetoPast-DistancetoRef)/DistancetoRef)*100, 
                                                      ((DistancetoPast-DistancetoRef)/DistancetoPast)*100))

#Statistical Test
results.fried.nat.slopehigh <- DistNatRegslopehigh %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>% unique() %>% 
  friedman_test(meanDistance~Baseline|Timebin)
results.fried.nat.slopehigh

pwc.Nat.slopehigh <- DistNatRegslopehigh %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>%
  wilcox_test(as.formula(paste("meanDistance", "Baseline", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Nat.slopehigh

#Summary
mean(DistNatRegslopehigh$DistDiff)
mean(DistNatRegslopehigh$DistancetoRef)
mean(DistNatRegslopehigh$DistancetoPast)


###### 4.1.a.2 Differences from Plantation to both Baselines #####

#Format data
Distplantslopehigh <- dist2pastslopehigh %>% subset(Type == "Plantation") %>%
  subset(select = -Type)
colnames(Distplantslopehigh)[3:4] <- c("DistancetoPast", "Sim2Pasture")
temp <- dist2refslopehigh %>% subset(Type == "Plantation") %>%
  subset(select = -Type)
colnames(temp)[3:4] <- c("DistancetoRef", "Sim2Ref")
Distplantslopehigh <- merge(Distplantslopehigh, temp, by = c("Timebin", "FrequencyCat"))
#Add the Distance Differential
Distplantslopehigh <- Distplantslopehigh %>% mutate(DistDiff = ifelse(DistancetoRef < DistancetoPast, 
                                                                        ((DistancetoPast-DistancetoRef)/DistancetoRef)*100, 
                                                                        ((DistancetoPast-DistancetoRef)/DistancetoPast)*100))

#Statistical Test
results.fried.plant.slopehigh <- Distplantslopehigh %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>% unique() %>% 
  friedman_test(meanDistance~Baseline|Timebin)
results.fried.plant.slopehigh

pwc.plant.slopehigh <- Distplantslopehigh %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>%
  wilcox_test(as.formula(paste("meanDistance", "Baseline", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.plant.slopehigh

#Summary
mean(Distplantslopehigh$DistDiff)
mean(Distplantslopehigh$DistancetoRef)
mean(Distplantslopehigh$DistancetoPast)


##### 4.1.b Differences to Both Baselines: Low Slope #####


###### 4.1.b.1 Differences from Nat Regen to both Baselines #####

#Format data
DistNatRegslopelow <- dist2pastslopelow %>% subset(Type == "Natural_Regeneration") %>%
  subset(select = -Type)
colnames(DistNatRegslopelow)[3:4] <- c("DistancetoPast", "Sim2Pasture")
temp <- dist2refslopelow %>% subset(Type == "Natural_Regeneration") %>%
  subset(select = -Type)
colnames(temp)[3:4] <- c("DistancetoRef", "Sim2Ref")
DistNatRegslopelow <- merge(DistNatRegslopelow, temp, by = c("Timebin", "FrequencyCat"))
#Add the Distance Differential
DistNatRegslopelow <- DistNatRegslopelow %>% mutate(DistDiff = ifelse(DistancetoRef < DistancetoPast, 
                                                                            ((DistancetoPast-DistancetoRef)/DistancetoRef)*100, 
                                                                            ((DistancetoPast-DistancetoRef)/DistancetoPast)*100))

#Statistical Test
results.fried.nat.slopelow <- DistNatRegslopelow %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>% unique() %>% 
  friedman_test(meanDistance~Baseline|Timebin)
results.fried.nat.slopelow

pwc.Nat.slopelow <- DistNatRegslopelow %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>%
  wilcox_test(as.formula(paste("meanDistance", "Baseline", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Nat.slopelow

#Summary
mean(DistNatRegslopelow$DistDiff)
mean(DistNatRegslopelow$DistancetoRef)
mean(DistNatRegslopelow$DistancetoPast)


###### 4.1.b.2 Differences from Plantation to both Baselines #####

#Format data
Distplantslopelow <- dist2pastslopelow %>% subset(Type == "Plantation") %>%
  subset(select = -Type)
colnames(Distplantslopelow)[3:4] <- c("DistancetoPast", "Sim2Pasture")
temp <- dist2refslopelow %>% subset(Type == "Plantation") %>%
  subset(select = -Type)
colnames(temp)[3:4] <- c("DistancetoRef", "Sim2Ref")
Distplantslopelow <- merge(Distplantslopelow, temp, by = c("Timebin", "FrequencyCat"))
#Add the Distance Differential
Distplantslopelow <- Distplantslopelow %>% mutate(DistDiff = ifelse(DistancetoRef < DistancetoPast, 
                                                                          ((DistancetoPast-DistancetoRef)/DistancetoRef)*100, 
                                                                          ((DistancetoPast-DistancetoRef)/DistancetoPast)*100))

#Statistical Test
results.fried.plant.slopelow <- Distplantslopelow %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>% unique() %>% 
  friedman_test(meanDistance~Baseline|Timebin)
results.fried.plant.slopelow

pwc.plant.slopelow <- Distplantslopelow %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>%
  wilcox_test(as.formula(paste("meanDistance", "Baseline", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.plant.slopelow

#Summary
mean(Distplantslopelow$DistDiff)
mean(Distplantslopelow$DistancetoRef)
mean(Distplantslopelow$DistancetoPast)


#### 4.2 Differences in Similarity to both baselines within PES classes by distance to road #####

##### 4.2.a Differences to Both Baselines: High RoadDist #####

###### 4.2.a.1 Differences from Nat Regen to both Baselines #####

#Format data
DistNatRegroadhigh <- dist2pastroadhigh %>% subset(Type == "Natural_Regeneration") %>%
  subset(select = -Type)
colnames(DistNatRegroadhigh)[3:4] <- c("DistancetoPast", "Sim2Pasture")
temp <- dist2refroadhigh %>% subset(Type == "Natural_Regeneration") %>%
  subset(select = -Type)
colnames(temp)[3:4] <- c("DistancetoRef", "Sim2Ref")
DistNatRegroadhigh <- merge(DistNatRegroadhigh, temp, by = c("Timebin", "FrequencyCat"))
#Add the Distance Differential
DistNatRegroadhigh <- DistNatRegroadhigh %>% mutate(DistDiff = ifelse(DistancetoRef < DistancetoPast, 
                                                                        ((DistancetoPast-DistancetoRef)/DistancetoRef)*100, 
                                                                        ((DistancetoPast-DistancetoRef)/DistancetoPast)*100))

#Statistical Test
results.fried.nat.roadhigh <- DistNatRegroadhigh %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>% unique() %>% 
  friedman_test(meanDistance~Baseline|Timebin)
results.fried.nat.roadhigh

pwc.Nat.roadhigh <- DistNatRegroadhigh %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>%
  wilcox_test(as.formula(paste("meanDistance", "Baseline", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Nat.roadhigh

#Summary
mean(DistNatRegroadhigh$DistDiff)
mean(DistNatRegroadhigh$DistancetoRef)
mean(DistNatRegroadhigh$DistancetoPast)


###### 4.2.a.2 Differences from Plantation to both Baselines #####

#Format data
Distplantroadhigh <- dist2pastroadhigh %>% subset(Type == "Plantation") %>%
  subset(select = -Type)
colnames(Distplantroadhigh)[3:4] <- c("DistancetoPast", "Sim2Pasture")
temp <- dist2refroadhigh %>% subset(Type == "Plantation") %>%
  subset(select = -Type)
colnames(temp)[3:4] <- c("DistancetoRef", "Sim2Ref")
Distplantroadhigh <- merge(Distplantroadhigh, temp, by = c("Timebin", "FrequencyCat"))
#Add the Distance Differential
Distplantroadhigh <- Distplantroadhigh %>% mutate(DistDiff = ifelse(DistancetoRef < DistancetoPast, 
                                                                      ((DistancetoPast-DistancetoRef)/DistancetoRef)*100, 
                                                                      ((DistancetoPast-DistancetoRef)/DistancetoPast)*100))

#Statistical Test
results.fried.plant.roadhigh <- Distplantroadhigh %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>% unique() %>% 
  friedman_test(meanDistance~Baseline|Timebin)
results.fried.plant.roadhigh

pwc.plant.roadhigh <- Distplantroadhigh %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>%
  wilcox_test(as.formula(paste("meanDistance", "Baseline", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.plant.roadhigh

#Summary
mean(Distplantroadhigh$DistDiff)
mean(Distplantroadhigh$DistancetoRef)
mean(Distplantroadhigh$DistancetoPast)


##### 4.2.b Differences to Both Baselines: Medium RoadDist #####


###### 4.2.b.1 Differences from Nat Regen to both Baselines #####

#Format data
DistNatRegroadmedium <- dist2pastroadmedium %>% subset(Type == "Natural_Regeneration") %>%
  subset(select = -Type)
colnames(DistNatRegroadmedium)[3:4] <- c("DistancetoPast", "Sim2Pasture")
temp <- dist2refroadmedium %>% subset(Type == "Natural_Regeneration") %>%
  subset(select = -Type)
colnames(temp)[3:4] <- c("DistancetoRef", "Sim2Ref")
DistNatRegroadmedium <- merge(DistNatRegroadmedium, temp, by = c("Timebin", "FrequencyCat"))
#Add the Distance Differential
DistNatRegroadmedium <- DistNatRegroadmedium %>% mutate(DistDiff = ifelse(DistancetoRef < DistancetoPast, 
                                                                            ((DistancetoPast-DistancetoRef)/DistancetoRef)*100, 
                                                                            ((DistancetoPast-DistancetoRef)/DistancetoPast)*100))

#Statistical Test
results.fried.nat.roadmedium <- DistNatRegroadmedium %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>% unique() %>% 
  friedman_test(meanDistance~Baseline|Timebin)
results.fried.nat.roadmedium

pwc.Nat.roadmedium <- DistNatRegroadmedium %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>%
  wilcox_test(as.formula(paste("meanDistance", "Baseline", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Nat.roadmedium

#Summary
mean(DistNatRegroadmedium$DistDiff)
mean(DistNatRegroadmedium$DistancetoRef)
mean(DistNatRegroadmedium$DistancetoPast)


###### 4.2.b.2 Differences from Plantation to both Baselines #####

#Format data
Distplantroadmedium <- dist2pastroadmedium %>% subset(Type == "Plantation") %>%
  subset(select = -Type)
colnames(Distplantroadmedium)[3:4] <- c("DistancetoPast", "Sim2Pasture")
temp <- dist2refroadmedium %>% subset(Type == "Plantation") %>%
  subset(select = -Type)
colnames(temp)[3:4] <- c("DistancetoRef", "Sim2Ref")
Distplantroadmedium <- merge(Distplantroadmedium, temp, by = c("Timebin", "FrequencyCat"))
#Add the Distance Differential
Distplantroadmedium <- Distplantroadmedium %>% mutate(DistDiff = ifelse(DistancetoRef < DistancetoPast, 
                                                                          ((DistancetoPast-DistancetoRef)/DistancetoRef)*100, 
                                                                          ((DistancetoPast-DistancetoRef)/DistancetoPast)*100))

#Statistical Test
results.fried.plant.roadmedium <- Distplantroadmedium %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>% unique() %>% 
  friedman_test(meanDistance~Baseline|Timebin)
results.fried.plant.roadmedium

pwc.plant.roadmedium <- Distplantroadmedium %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>%
  wilcox_test(as.formula(paste("meanDistance", "Baseline", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.plant.roadmedium

#Summary
mean(Distplantroadmedium$DistDiff)
mean(Distplantroadmedium$DistancetoRef)
mean(Distplantroadmedium$DistancetoPast)


##### 4.2.c Differences to Both Baselines: Low RoadDist #####


###### 4.2.c.1 Differences from Nat Regen to both Baselines #####

#Format data
DistNatRegroadlow <- dist2pastroadlow %>% subset(Type == "Natural_Regeneration") %>%
  subset(select = -Type)
colnames(DistNatRegroadlow)[3:4] <- c("DistancetoPast", "Sim2Pasture")
temp <- dist2refroadlow %>% subset(Type == "Natural_Regeneration") %>%
  subset(select = -Type)
colnames(temp)[3:4] <- c("DistancetoRef", "Sim2Ref")
DistNatRegroadlow <- merge(DistNatRegroadlow, temp, by = c("Timebin", "FrequencyCat"))
#Add the Distance Differential
DistNatRegroadlow <- DistNatRegroadlow %>% mutate(DistDiff = ifelse(DistancetoRef < DistancetoPast, 
                                                                      ((DistancetoPast-DistancetoRef)/DistancetoRef)*100, 
                                                                      ((DistancetoPast-DistancetoRef)/DistancetoPast)*100))

#Statistical Test
results.fried.nat.roadlow <- DistNatRegroadlow %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>% unique() %>% 
  friedman_test(meanDistance~Baseline|Timebin)
results.fried.nat.roadlow

pwc.Nat.roadlow <- DistNatRegroadlow %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>%
  wilcox_test(as.formula(paste("meanDistance", "Baseline", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.Nat.roadlow

#Summary
mean(DistNatRegroadlow$DistDiff)
mean(DistNatRegroadlow$DistancetoRef)
mean(DistNatRegroadlow$DistancetoPast)


###### 4.2.c.2 Differences from Plantation to both Baselines #####

#Format data
Distplantroadlow <- dist2pastroadlow %>% subset(Type == "Plantation") %>%
  subset(select = -Type)
colnames(Distplantroadlow)[3:4] <- c("DistancetoPast", "Sim2Pasture")
temp <- dist2refroadlow %>% subset(Type == "Plantation") %>%
  subset(select = -Type)
colnames(temp)[3:4] <- c("DistancetoRef", "Sim2Ref")
Distplantroadlow <- merge(Distplantroadlow, temp, by = c("Timebin", "FrequencyCat"))
#Add the Distance Differential
Distplantroadlow <- Distplantroadlow %>% mutate(DistDiff = ifelse(DistancetoRef < DistancetoPast, 
                                                                    ((DistancetoPast-DistancetoRef)/DistancetoRef)*100, 
                                                                    ((DistancetoPast-DistancetoRef)/DistancetoPast)*100))

#Statistical Test
results.fried.plant.roadlow <- Distplantroadlow %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>% unique() %>% 
  friedman_test(meanDistance~Baseline|Timebin)
results.fried.plant.roadlow

pwc.plant.roadlow <- Distplantroadlow %>% 
  subset(select = c(Timebin, FrequencyCat, DistancetoPast, DistancetoRef)) %>%
  pivot_longer(cols = starts_with("Dist"), names_to = "Baseline", values_to= "Distance") %>%
  group_by(Timebin, Baseline) %>% mutate(meanDistance = mean(Distance)) %>% ungroup() %>%
  subset(select = c(Timebin, Baseline, meanDistance)) %>%
  wilcox_test(as.formula(paste("meanDistance", "Baseline", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwc.plant.roadlow

#Summary
mean(Distplantroadlow$DistDiff)
mean(Distplantroadlow$DistancetoRef)
mean(Distplantroadlow$DistancetoPast)


### 5. Supplementary Figures #####

#### 5.1 Supplementary Figure 5: Similarity to Reference Forest for Distance to Road Categories #####

dist2refroadhigh$RoadDist <- "High"
dist2refroadmedium$RoadDist <- "Medium"
dist2refroadlow$RoadDist <- "Low"

dist2refrows <- rbind(dist2refroadhigh,dist2refroadmedium, dist2refroadlow)

labels <- c("Pasture", "Plantation", "Natural Regeneration")

dist2refrows %>% 
  group_by(Type, Timebin, RoadDist) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% 
  subset(select = c(Type, Timebin, meanSimilarity, RoadDist)) %>%
  mutate(Period = case_when(
    `Timebin` >= 290 & `Timebin` <= 350 ~ "Dawn",
    `Timebin` >= 1050 & `Timebin` <= 1110 ~ "Dusk",
    `Timebin` > 350 & `Timebin` < 1050 ~ "Day",
    TRUE ~ "Night"
  )) %>%
  subset(select = c(Type, meanSimilarity, Period, RoadDist)) %>% 
  unique() %>%
  mutate(RoadDist = factor(RoadDist, levels = c("Low", "Medium", "High"))) %>%
  ggplot(aes(x = interaction(Type, RoadDist), y = meanSimilarity)) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2), alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.5, linetype = "solid", color = "black") +
  stat_summary(fun = mean, geom = "text", 
               aes(label = round(..y.., 2)), 
               vjust = -1.6, color = "black", size = 4) +
  theme_classic2() +
  labs(y = "Mean Acoustic Similarity to Reference Forests", x = "Grouping") +
  scale_x_discrete(
    labels = function(x) gsub("\\.", " × ", x),
    expand = expansion(add = 1) 
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.spacing.x = unit(1, "lines")
  ) 

#ggsave('figures/SupplementaryFigure5A_stripplot_DistancetoRoad.pdf')


#### 5.2 Supplementary Figure 5B: Similarity to Reference Forest for Slope Categories #####

dist2refslopehigh$slopeDist <- "High"
dist2refslopelow$slopeDist <- "Low"

dist2refrows <- rbind(dist2refslopehigh,dist2refslopelow)

labels <- c("Pasture", "Plantation", "Natural Regeneration")

dist2refrows %>% 
  group_by(Type, Timebin, slopeDist) %>% 
  mutate(meanSimilarity = mean(Similarity)) %>%
  ungroup() %>% 
  subset(select = c(Type, Timebin, meanSimilarity, slopeDist)) %>%
  mutate(Period = case_when(
    `Timebin` >= 290 & `Timebin` <= 350 ~ "Dawn",
    `Timebin` >= 1050 & `Timebin` <= 1110 ~ "Dusk",
    `Timebin` > 350 & `Timebin` < 1050 ~ "Day",
    TRUE ~ "Night"
  )) %>%
  subset(select = c(Type, meanSimilarity, Period, slopeDist)) %>% 
  unique() %>%
  mutate(slopeDist = factor(slopeDist, levels = c("Low", "Medium", "High"))) %>%
  ggplot(aes(x = interaction(Type, slopeDist), y = meanSimilarity)) +
  geom_jitter(aes(color = Period), position = position_jitter(0.2), alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.5, linetype = "solid", color = "black") +
  stat_summary(fun = mean, geom = "text", 
               aes(label = round(..y.., 2)), 
               vjust = -1.6, color = "black", size = 4) +
  theme_classic2() +
  labs(y = "Mean Acoustic Similarity to Pastures", x = "Grouping") +
  scale_x_discrete(
    labels = function(x) gsub("\\.", " × ", x),
    expand = expansion(add = 1) 
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.spacing.x = unit(1, "lines")
  ) 

#ggsave('figures/SupplementaryFigure5B_stripplot_Distancetoslope.pdf')

