

### 1. LOAD PACKAGES ##############################

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
FrequencyEstimates$Annotation <- ifelse(grepl("mamm", FrequencyEstimates$Annotation, ignore.case = TRUE), "Mammals", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("ins", FrequencyEstimates$Annotation, ignore.case = TRUE), "Insects", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("amph", FrequencyEstimates$Annotation, ignore.case = TRUE), "Amphibians", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("bat", FrequencyEstimates$Annotation, ignore.case = TRUE), "Unknown", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("unk", FrequencyEstimates$Annotation, ignore.case = TRUE), "Unknown", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation <- ifelse(grepl("v", FrequencyEstimates$Annotation, ignore.case = TRUE), "Unknown", FrequencyEstimates$Annotation)
FrequencyEstimates$Annotation[FrequencyEstimates$Annotation== ""] <- "Unknown"
