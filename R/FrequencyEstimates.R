rm(list = ls())

library(RColorBrewer)
library(tidyverse)

#<<<<<<< HEAD
### 1. LOAD PACKAGES ##############################
#Pacman allows you to install and load (or just load) packages at once
#install.packages(pacman)
library(pacman)
#Load other packages
p_load(RColorBrewer)
p_load(terra)
p_load(dplyr)
p_load(ggplot2)
p_load(purrr)
p_load(RColorBrewer)
p_load(stats)
p_load(utils)
#=======
### 2. LOAD AND CLEAN LABEL DATA ##############################
FrequencyEstimates <- read.csv('data/FrequencyEstimates.csv')
#>>>>>>> 8cdec5ccafbec51a7df95771d361d96709a6cafe

FrequencyEstimates <- FrequencyEstimates %>%
  mutate(
    Annotation = case_when(
      str_detect(Annotation, regex("ins", ignore_case = TRUE)) ~ "Insects",
      str_detect(Annotation, regex("inns", ignore_case = TRUE)) ~ "Insects",
      str_detect(Annotation, regex("bir", ignore_case = TRUE)) ~ "Birds",
      str_detect(Annotation, regex("dom", ignore_case = TRUE)) ~ "Domestic",
      str_detect(Annotation, regex("mamm", ignore_case = TRUE)) ~ "Mammals",
      str_detect(Annotation, regex("amph", ignore_case = TRUE)) ~ "Amphibians",
      str_detect(Annotation, regex("bat", ignore_case = TRUE)) ~ "Unknown",
      str_detect(Annotation, regex("unk", ignore_case = TRUE)) ~ "Unknown",
      str_detect(Annotation, regex("v", ignore_case = TRUE)) ~ "Unknown",
      Annotation == "" ~ "Unknown",
      TRUE ~ Annotation
    )
  )

# Proportion of domestic mammal and human noises that make up the mammal calls
totalmammal <- FrequencyEstimates %>% subset(Annotation %in% c("Domestic", "Mammals", "Humans")) %>% nrow()
wildmammal <- FrequencyEstimates %>% subset(Annotation == "Mammals") %>% nrow()
#<<<<<<< HEAD

propwild <- wildmammal/totalmammal * 100
#=======
wildmammal/totalmammal * 100
#>>>>>>> 8cdec5ccafbec51a7df95771d361d96709a6cafe

#31.82% of annotations are wild mammals, most are domestic and human

#90% frequency ranges for each 

quantile_ranges <- FrequencyEstimates %>% 
  subset(Annotation %in% c("Mammals", "Humans", "Domestic")) %>%
  group_by(Annotation) %>%
  summarise(
    Q05 = quantile(MidFreq, 0.05),
    Q95 = quantile(MidFreq, 0.95)
  )

# Display the result
print(quantile_ranges)

#Quick plot with three low-frequency classes
LowFreqGroupRanges <- FrequencyEstimates %>%
  subset(Annotation %in% c("Mammals", "Humans", "Domestic"))

LowFreqGroupRanges <- LowFreqGroupRanges %>% mutate(MidFreq = ((LowFreq + HighFreq)/2))

LowFreqGroupRanges <- LowFreqGroupRanges %>%
  mutate(density = case_when(
    Annotation == "Humans" ~ 0.006,
    Annotation == "Domestic" ~ 0.005,
    Annotation == "Mammals" ~ 0.004,
    ))

LowFreqGroupRanges  %>%
  ggplot(aes(x = MidFreq, fill = Annotation, color = Annotation)) +
  geom_density(alpha = 0.5) +  
  labs(
    title = "Density of Call Frequencies by Taxonomic Group",
    x = "Middle Frequency of Call",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_boxplot(
    data = LowFreqGroupRanges, 
    aes(x = MidFreq, y = density, color = Annotation),
    fill = NA,           # Make the boxes hollow
    outlier.shape = NA,  # Remove outlier points
    width = 0.0002       # Make the boxes smaller
  ) 


#Filter for only four main classes, combine domestic into mammal
#<<<<<<< HEAD

ClassFrequencyEstimates <- FrequencyEstimates %>% 
  subset(Annotation %in% c("Birds", "Insects", "Mammals", "Amphibians", "Domestic"))
ClassFrequencyEstimates$Annotation <- ifelse(grepl("dom", ClassFrequencyEstimates$Annotation, ignore.case = TRUE), "Mammals", ClassFrequencyEstimates$Annotation)

#=======
#>>>>>>> 8cdec5ccafbec51a7df95771d361d96709a6cafe
#Add middle frequency column
FrequencyEstimates <- FrequencyEstimates %>%
  filter(Annotation %in% c("Birds", "Insects", "Mammals", "Amphibians", "Domestic")) %>%
  mutate(Annotation = ifelse(str_detect(Annotation, regex("dom", ignore_case = TRUE)),
                             "Mammals",
                             Annotation)) %>% 
  mutate(MidFreq = ((LowFreq + HighFreq)/2)) %>% 
  mutate(Annotation = factor(Annotation, levels = c("Mammals", "Amphibians", "Birds", "Insects")))

### 4. FIGURE 2B VISUALIZE THE FREQUENCY RANGES ##############################

# Use Spectral color palette
custom_colors <- rev(brewer.pal(4, "Spectral"))

# Add dummy column for mapping
FrequencyEstimates <- FrequencyEstimates %>%
  mutate(density = case_when(
    Annotation == "Mammals" ~ 0.0032,
    Annotation == "Amphibians" ~ 0.0024,
    Annotation == "Birds" ~ 0.0016,
    Annotation == "Insects" ~ 0.0008,
    TRUE ~ NA_real_
  ))

# Plot
FrequencyEstimates %>% 
  ggplot(aes(x = MidFreq, fill = Annotation, color = Annotation)) +
  geom_density(alpha = 0.5) +  
  labs(x = "Middle Frequency of Call", y = "Density") +
  scale_x_continuous(breaks = seq(0, 15000, by = 1000),
                     limits = c(0, 15000)) +
  scale_fill_manual(values = custom_colors) + 
  scale_color_manual(values = custom_colors) +  
  theme_bw() +
  theme(legend.position = "top") +
  geom_boxplot(
    data = FrequencyEstimates,
    aes(x = MidFreq, y = density, color = Annotation),
    fill = NA,      
    outlier.shape = NA, 
    width = 0.0002  
  ) 
  # geom_vline(xintercept = c(0, 9000), colour="lightgreen", linetype = "longdash")

ggsave('figures/Figure2B_DensityandBox_v2.pdf')

### 5. Check if taxonomic groups have significantly different frequency ranges #################

# Low frequency
# Perform ANOVA
anovaLow <- aov(LowFreq ~ Annotation, data = ClassFrequencyEstimates)
anovaLow

# Conduct post-hoc test (Tukey's HSD)
tukeyLow <- TukeyHSD(anovaLow)

# View ANOVA summary
summary(anovaLow)

# View post-hoc test results
print(tukeyLow)
#Highly statistically significant between all groups


# Middle frequency
# Perform ANOVA
anova_Middle <- aov(MidFreq ~ Annotation, data = ClassFrequencyEstimates)
anova_Middle

# Conduct post-hoc test (Tukey's HSD)
tukey_Middle <- TukeyHSD(anova_Middle)

# View ANOVA summary
summary(anova_Middle)

# View post-hoc test results
print(tukey_Middle)
#Highly statistically significant between all groups



# High frequency
# Perform ANOVA
anova_High <- aov(HighFreq ~ Annotation, data = ClassFrequencyEstimates)
anova_High

# Conduct post-hoc test (Tukey's HSD)
tukey_High <- TukeyHSD(anova_High)

# View ANOVA summary
summary(anova_High)

# View post-hoc test results
print(tukey_High)
#Highly statistically significant between all groups



# Delta frequency
# Perform ANOVA
anova_Delta <- aov(DeltaFreq ~ Annotation, data = ClassFrequencyEstimates)
anova_Delta

# Conduct post-hoc test (Tukey's HSD)
tukey_Delta <- TukeyHSD(anova_Delta)

# View ANOVA summary
summary(anova_Delta)

# View post-hoc test results
print(tukey_Delta)
#Highly statistically significant between all groups
#<<<<<<< HEAD

#90% quantiles by group

quantile_ranges <- ClassFrequencyEstimates %>%
  group_by(Annotation) %>%
  summarise(
    Q05 = quantile(MidFreq, 0.05),
    Q95 = quantile(MidFreq, 0.95)
  )

print(quantile_ranges)

#Biophony 90% range

quantile_ranges <- ClassFrequencyEstimates %>%
  summarise(
    Q05 = quantile(MidFreq, 0.05),
    Q95 = quantile(MidFreq, 0.95)
  )

print(quantile_ranges)
#=======
#>>>>>>> 8cdec5ccafbec51a7df95771d361d96709a6cafe
