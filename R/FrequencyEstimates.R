rm(list = ls())
library(RColorBrewer)
library(terra)
library(gplyr)
library(purrr)
library(RColorBrewer)
library(stats)
library(utils)
library(tidyverse)

### 2. LOAD AND PROCESS ALL THE SELECTION DATA ##############################

#Pastures
DayPasture <- list()
setwd("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/Pasture/DAY")
files <- dir()

for (i in 1:length(files)) {
  DayPasture[[i]] <- read.delim(paste(getwd(), files[i], sep = "/"))
}
DayPastureFrequencies <- list_rbind(DayPasture)
DayPastureFrequencies <- DayPastureFrequencies %>% subset( select = -c(Selection, View, Channel))
colnames(DayPastureFrequencies) <- c("StartTime", "EndTime", "LowFreq", "HighFreq", "DeltaTime", "DeltaFreq", "AvgPowerDensity", "Annotation")

NightPasture <- list()
setwd("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/Pasture/NIGHT")
files <- dir()

for (i in 1:length(files)) {
  NightPasture[[i]] <- read.delim(paste(getwd(), files[i], sep = "/"))
}
NightPastureFrequencies <- list_rbind(NightPasture)
NightPastureFrequencies <- NightPastureFrequencies %>% subset( select = -c(Selection, View, Channel))
colnames(NightPastureFrequencies) <- c("StartTime", "EndTime", "LowFreq", "HighFreq", "DeltaTime", "DeltaFreq", "AvgPowerDensity", "Annotation")

#Reference Forest
DayReference_Forest <- list()
setwd("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/reference forest/DAY")
files <- dir()

for (i in 1:length(files)) {
  DayReference_Forest[[i]] <- read.delim(paste(getwd(), files[i], sep = "/"))
}
DayReference_ForestFrequencies <- list_rbind(DayReference_Forest)
DayReference_ForestFrequencies <- DayReference_ForestFrequencies %>% subset( select = -c(Selection, View, Channel))
colnames(DayReference_ForestFrequencies) <- c("StartTime", "EndTime", "LowFreq", "HighFreq", "DeltaTime", "DeltaFreq", "AvgPowerDensity", "Annotation")

NightReference_Forest <- list()
setwd("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/reference forest/NIGHT")
files <- dir()

for (i in 1:length(files)) {
  NightReference_Forest[[i]] <- read.delim(paste(getwd(), files[i], sep = "/"))
}
NightReference_ForestFrequencies <- list_rbind(NightReference_Forest)
NightReference_ForestFrequencies <- NightReference_ForestFrequencies %>% subset( select = -c(Selection, View, Channel))
colnames(NightReference_ForestFrequencies) <- c("StartTime", "EndTime", "LowFreq", "HighFreq", "DeltaTime", "DeltaFreq", "AvgPowerDensity", "Annotation")

#Natural Regeneration
DayNatural_Regeneration <- list()
setwd("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/PSA/conservation/DAY")
files <- dir()

for (i in 1:length(files)) {
  DayNatural_Regeneration[[i]] <- read.delim(paste(getwd(), files[i], sep = "/"))
}
DayNatural_RegenerationFrequencies <- list_rbind(DayNatural_Regeneration)
DayNatural_RegenerationFrequencies <- DayNatural_RegenerationFrequencies %>% subset( select = -c(Selection, View, Channel))
colnames(DayNatural_RegenerationFrequencies) <- c("StartTime", "EndTime", "LowFreq", "HighFreq", "DeltaTime", "DeltaFreq", "AvgPowerDensity", "Annotation")

NightNatural_Regeneration <- list()
setwd("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/PSA/conservation/NIGHT")
files <- dir()

for (i in 1:length(files)) {
  NightNatural_Regeneration[[i]] <- read.delim(paste(getwd(), files[i], sep = "/"))
}
NightNatural_RegenerationFrequencies <- list_rbind(NightNatural_Regeneration)
NightNatural_RegenerationFrequencies <- NightNatural_RegenerationFrequencies %>% subset( select = -c(Selection, View, Channel))
colnames(NightNatural_RegenerationFrequencies) <- c("StartTime", "EndTime", "LowFreq", "HighFreq", "DeltaTime", "DeltaFreq", "AvgPowerDensity", "Annotation")

#Plantations
DayPlantation <- list()
setwd("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/PSA/reforestation/DAY")
files <- dir()

for (i in 1:length(files)) {
  DayPlantation[[i]] <- read.delim(paste(getwd(), files[i], sep = "/"))
}
DayPlantationFrequencies <- list_rbind(DayPlantation)
DayPlantationFrequencies <- DayPlantationFrequencies %>% subset( select = -c(Selection, View, Channel))
colnames(DayPlantationFrequencies) <- c("StartTime", "EndTime", "LowFreq", "HighFreq", "DeltaTime", "DeltaFreq", "AvgPowerDensity", "Annotation")

NightPlantation <- list()
setwd("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/PSA/reforestation/NIGHT")
files <- dir()

for (i in 1:length(files)) {
  NightPlantation[[i]] <- read.delim(paste(getwd(), files[i], sep = "/"))
}
NightPlantationFrequencies <- list_rbind(NightPlantation)
NightPlantationFrequencies <- NightPlantationFrequencies %>% subset( select = -c(Selection, View, Channel))
colnames(NightPlantationFrequencies) <- c("StartTime", "EndTime", "LowFreq", "HighFreq", "DeltaTime", "DeltaFreq", "AvgPowerDensity", "Annotation")

#Now combine them all into a single dataframe

FrequencyEstimates <- rbind(DayPastureFrequencies, NightPastureFrequencies, DayReference_ForestFrequencies, NightReference_ForestFrequencies, DayNatural_RegenerationFrequencies, NightNatural_RegenerationFrequencies, DayPlantationFrequencies, NightPlantationFrequencies)


### 3. CLEAN AND PROPERLY LABEL DATA ##############################

unique(FrequencyEstimates$Annotation)

FrequencyEstimates$Annotation <- ifelse(grepl("ins", FrequencyEstimates$Annotation, ignore.case = TRUE), "Insects", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("inns", FrequencyEstimates$Annotation, ignore.case = TRUE), "Insects", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("bir", FrequencyEstimates$Annotation, ignore.case = TRUE), "Birds", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("dom", FrequencyEstimates$Annotation, ignore.case = TRUE), "Domestic", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("mamm", FrequencyEstimates$Annotation, ignore.case = TRUE), "Mammals", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("ins", FrequencyEstimates$Annotation, ignore.case = TRUE), "Insects", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("amph", FrequencyEstimates$Annotation, ignore.case = TRUE), "Amphibians", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("bat", FrequencyEstimates$Annotation, ignore.case = TRUE), "Unknown", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("unk", FrequencyEstimates$Annotation, ignore.case = TRUE), "Unknown", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("v", FrequencyEstimates$Annotation, ignore.case = TRUE), "Unknown", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation[FrequencyEstimates$Annotation== ""] <- "Unknown"

# Proportion of domestic mammal and human noises that make up the mammal calls

totalmammal <- FrequencyEstimates %>% subset(Annotation %in% c("Domestic", "Mammals", "Humans")) %>% nrow()

wildmammal <- FrequencyEstimates %>% subset(Annotation == "Mammals") %>% nrow()

wildmammal/totalmammal * 100

#31.82% of annotations are wild mammals, most are domestic and human

#Filter for only four main classes, combine domestic into mammal

ClassFrequencyEstimates <- FrequencyEstimates %>% subset(Annotation %in% c("Birds", "Insects", "Mammals", "Amphibians", "Domestic"))
ClassFrequencyEstimates$Annotation <- ifelse(grepl("dom", ClassFrequencyEstimates$Annotation, ignore.case = TRUE), "Mammals", ClassFrequencyEstimates$Annotation)

#Add middle frequency column

ClassFrequencyEstimates <- ClassFrequencyEstimates %>% mutate(MidFreq = ((LowFreq + HighFreq)/2))

### 4. FIGURE 2B VISUALIZE THE FREQUENCY RANGES ##############################

#Use Spectral color palette
custom_colors <- rev(brewer.pal(4, "Spectral"))

#Set levels of factor for labelling
ClassFrequencyEstimates$Annotation <- factor(ClassFrequencyEstimates$Annotation, levels=c("Mammals", "Amphibians", "Birds", "Insects"))

#Add meaningless column for mapping
ClassFrequencyEstimates <- ClassFrequencyEstimates %>%
  mutate(density = case_when(
    Annotation == "Mammals" ~ 0.0032,
    Annotation == "Amphibians" ~ 0.0024,
    Annotation == "Birds" ~ 0.0016,
    Annotation == "Insects" ~ 0.0008,
    TRUE ~ NA_real_
  ))

#Plot
ClassFrequencyEstimates %>% 
  ggplot(aes(x = MidFreq, fill = Annotation, color = Annotation)) +
  geom_density(alpha = 0.5) +  
  labs(
    title = "Density of Call Frequencies by Taxonomic Group",
    x = "Middle Frequency of Call",
    y = "Density"
  ) +
  scale_x_continuous(
    breaks = seq(0, 15000, by = 1000),  # Set x-axis breaks every 1000 units
    limits = c(0, 15000)                # Limit x-axis to 0-15,000
  ) +
  scale_fill_manual(values = custom_colors) +  # Set custom fill colors from the "spectral" palette
  scale_color_manual(values = custom_colors) +  # Set custom line colors from the "spectral" palette
  theme_minimal() +
  theme(legend.position = "top") +
  geom_boxplot(
    data = ClassFrequencyEstimates, 
    aes(x = MidFreq, y = density, color = Annotation),
    fill = NA,           # Make the boxes hollow
    outlier.shape = NA,  # Remove outlier points
    width = 0.0002       # Make the boxes smaller
  ) +
  geom_vline(xintercept = c(0, 9000), colour="lightgreen", linetype = "longdash")


ggsave('/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/figures/Figure2B_DensityandBox.pdf')

#Warnings because of few outlier points above 15,000 Hz


### 5. DO TAXANOMIC GROUPS HAVE SIGNIFICANTLY DIFFERENT FREQUENCY RANGES? ##############################


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


