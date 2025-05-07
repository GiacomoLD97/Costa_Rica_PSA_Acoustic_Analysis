# Clean up environment
rm(list = ls())

### 1. Load Libraries ###################

# Load libraries
library(data.table)
library(rlist)
library(ggpubr)
library(rstatix)
library(tmap)
library(sf)
library(corrplot)
library(usdm)
library(GGally)
library(lme4)
library(tidyverse)
library(xgboost)
library(SHAPforxgboost)

### 2. Load Climatic/Predictive Variables for each Site #############################

### 2a. Load Data #############################

# Extract climatic variables for all sites included from Google Earth Engine
# The code used in Google Earth Engine can be found at the following link: 
# https://code.earthengine.google.com/50d4b0b049b1cbdc90f4d0372663efe5
# Load the climatic data
env_data <- fread("data/climaticvariables.csv") %>% 
  select(-c('system:index', '.geo'))

# Add canopy height as measured on the ground
canopy_height <- fread("data/FieldDataSheet.csv") %>% 
  select(Site, Tstart = "Canopy T start", Tmid = "Canopy T mid", Tend = "Canopy T end") %>%
  mutate(across(c(Tstart, Tmid, Tend), ~replace_na(., 0)),
         AvgCanopyHeight = rowMeans(across(c(Tstart, Tmid, Tend)))) %>% 
  select(Site, AvgCanopyHeight)

# Add this to the climatic data 
env_data <- env_data %>% left_join(canopy_height, by = "Site") %>% 
  rename(Aridity_Index = "CGIAR_Aridity_Index",
         Annual_Mean_Temperature = "CHELSA_BIO_Annual_Mean_Temperature",
         Annual_Precipitation = "CHELSA_BIO_Annual_Precipitation",
         NPP = "CHELSA_exBIO_NPP",
         EVI = "EarthEnvTexture_Contrast_EVI",
         Elevation = "EarthEnvTopoMed_Elevation",
         Soil_pH = "SG_Soil_pH_H2O_000cm",
         Human_Footprint = "WCS_Human_Footprint_2009")

### 2b. Test for Collinearity #############################

# Test collinearity of the predictive variables  
cor_matrix <- cor(env_data %>% select(-Site), use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color")

# Custom function to include a linear model line on the ggpairs scatter plots
lowerFn <- function(data, mapping, method = "lm", point.colour = "#00BFFF", line.colour = "#1E90FF", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = point.colour) +
    geom_smooth(method = method, color = line.colour, fill = line.colour)
  p
}

ggpairs(
  env_data %>% select(-Site), 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag"),
  upper = list(continuous = wrap("cor", size = 5))
)

# According to correlations we should remove Aridity, mean Temperature, precip, NPP and soil pH

# Test using VIF
options(scipen = 999)
set.seed(123) 

# Threshold determines sensitivity, although for our data thresholds between 1 and 10 do not change the result
VIF.COR <- vifcor(env_data %>% select(-Site), th = 0.5)

# Returns a VIF object, examine different outputs.
VIF.COR.MATRIX <- data.frame(VIF.COR@corMatrix)

# Check which variables are still included.
VIF.COR@excluded

# Confirmation that we should remove Aridity, mean Temperature, precip, NPP and soil pH

### 3. Modelling LLMs and XGMs #############################

### 3a. Load and Organize Results of PMN Analysis #############################

#IF YOU ALREADY HAVE THE DATAFRAME WITH THE MODELLING DATA SKIP TO NEXT SECTION

#Load the results of the PMN acoustic analysis

# #Define a function that will allow us to add time of day to the results
# MinuteConverter <- function(r) {
#   h <- as.numeric(substring(r, 1, 2))
#   m<- as.numeric(substring(r, 3, 4))
#   s<- as.numeric(substring(r, 4, 5))
#   minuteofday <- (h*60) + (m) + (s/60)
#   print(minuteofday)
# }
# 
# #Loop through all the files in the harddrive to create a single dataframe of data
# 
# #Make a list of every file for every site
# setwd("/Volumes/Lacie/output")
# filesbig <- dir()
# #Create empty lists that will be filled with the data
# biglist <- list()
# smalllist <- list()
# for (j in 1:length(filesbig)) {
# #reset the file list at the beginning of every loop
#   setwd("/Volumes/Lacie/output")
#   upnow <- filesbig[[j]]
#   setwd(paste(getwd(), upnow, sep="/"))
# #Make a list for each file within a site folder
#   files <- dir()
#   smalllist <- list()
#   for (i in 1:length(files)) {
# #Extract the time of the recording and feed it into the Minute converter function
#     StartTime <- as.numeric(strsplit(files[[i]], "_")[[1]][3])
#     if (nchar(StartTime) == 5) {
#       StartTime <- paste(0, StartTime, sep = "")
#     } else if (nchar(StartTime) == 1) {
#       StartTime <- paste(0, 0, 0, 0, 0, StartTime, sep ="")
#     }
#     StartMinute <- MinuteConverter(StartTime)
# #Extract the date of the recording
#     RecordingDay <- strsplit(files[[i]], "_")[[1]][2]
# #Convert both day and time to a POSIT
#     dayandtime <- paste(RecordingDay, StartTime)
#     TimeStamp <- strptime(dayandtime, format = "%Y%m%d %H%M%S")
# #Now that we have all the metadeta, load the actual csv with the data
#     smalllist[[i]] <- read.csv(paste(getwd(), files[[i]], sep = "/"))
# #Add the metadeta to the dataframe
# #First the Timestamp needs to be added and the appropriate amount of seconds added based on the minute of the recording
# #Then we scale the Minute by adding the minute of the day at which the recording started
# #We then have to correct for recordings that record past midnight, by resetting the Minute counter at 1440
# #Finally we round the Minute to the nearest whole number and add the Site ID
# #To reduce the size of the output in this step of the analysis we can Sum the Noise and PMN across freq bands
# #Groupby the Minute and Sum PMN and Noise, then remove the Frequency, PMN and Noise columns and take only unique rows
#     smalllist[[i]] <- smalllist[[i]] %>% 
#       mutate(TimeStamp = TimeStamp + (Minute*60 - 60)) %>% mutate(Minute = Minute + StartMinute) %>% 
#       mutate(Minute = if_else(Minute > 1440, (Minute - 1440), Minute)) %>%
#       mutate(Minute = round(Minute, digits = 0), Site = str_sub(strsplit(files[[i]], "_")[[1]][4], start = 1, end = -5)) %>%
#       group_by(TimeStamp) %>% mutate(SummedPMN = sum(PMN), SummedNoise = sum(Noise)) %>%
#       subset(select = -c(Noise, PMN, Frequency)) %>% unique()
#   }
# #Save the processed data to the list before moving on to the next site  
#   biglist[[j]] <- rbindlist(smalllist)
#   names(biglist)[j] <- paste(filesbig[j])
# }
# 
# #Define a function to remove the first and last half-hour to avoid any sounds of placing and picking up mics
# remove_rows <- function(df) {
#   df <- df[31:( nrow(df) - 30 ),]
#   return(df)
# }
# 
# #Remove those rows from each site (not each file)
# biglist <- lapply(biglist, remove_rows)
# 
# #Combine the list into a dataframe
# PMNAllsites <- list.rbind(biglist)
# 
# #Create a data frame that has both PMN data and metadata
#ModellingData <- left_join(PMNAllsites, IncludedSitesMetaData, by = c("Site" = "Site"))
# # Any sites that should be removed will have NAs so can remove them easily by using na.omit
#ModellingData <- na.omit(ModellingData)
# 
##Do the same for Sitesincluded to include the other site metadata
# #Save this dataframe so that it can be called on again
#write.csv(ModellingData, "data/ModellingDataForFigure2oct2023.csv", row.names=FALSE)


### 3b. LMER Modelling per Minute #############################

# START HERE IF YOU HAVE THE DATAFRAME, data not uploaded to GitHub, too large
ModellingData <- fread("/Users/giacomodelgado/Documents/PhD Chapter 1/Data not in GitHub/ModellingData.csv")

# List to store output
best_models <- list()

# Remove minutes where recorder failed
`%notin%` <- Negate(`%in%`)
ModellingData <- ModellingData %>%
  filter(Minute %notin% c(385, 386, 1080)) %>% 
  rename(Human_Footprint = Human.footprint, Annual_Precipitation = Ann_Precip)

#Start by defining a function that returns the AIC of the LMEM
calculate_AIC <- function(data, response, predictors) {
  formula <- as.formula(paste(response, "~", paste(predictors, collapse = "+"), paste("+", "(1 | MicType)")))
  model <- lmer(formula, data = data)
  return(extractAIC(model)[2])
}
#Then create the loop that tries all combinations of predictive variables

#Create a list where all the outputs can be stored
best_models <- list()

#Because of Microphone behavior (slight delay between writing a file and starting a new recording) there are three minutes for which we do not have data
`%notin%` <- Negate(`%in%`)
ModellingData <- ModellingData %>%
  filter(Minute %notin% c(385, 386, 1080)) 
minutesincluded <- unique(ModellingData$Minute)
#Should also make sure that in case there are minutes that don't have the full set of 119 sites that they are removed

for (min in minutesincluded) {
  # Take the data for the current min
  currentmin <- ModellingData %>% filter(Minute == min)
  
  # Calculate the average of SummedPMN per Site over the five days for which we have data, do the same for Noise
  currentmin <- ModellingData %>% subset(Minute == min) %>% group_by(Site) %>% 
    mutate(avgSummedPMN = mean(SummedPMN), avgSummedNoise = mean(SummedNoise)) %>%
    subset(select = -c(TimeStamp, SummedPMN, SummedNoise)) %>% unique()
  currentmin[ , c(7:11, 13)] <- scale(currentmin[ , c(7:11, 13)])
  
  # Get a vector of predictor variable names
  predictors <- c("avgSummedNoise", "Annual_Precipitation", 
                  "EVI", "Elevation", 
                  "Human_Footprint", "AvgCanopyHeight", "Type")
  
  # Generate all possible combinations of predictors
  predictor_combinations <- list()
  for (i in 1:length(predictors)) {
    predictor_combinations[[i]] <- combn(predictors, i)
  }
  
  # Loop through each combination and calculate AIC
  best_AIC <- Inf
  best_model <- NULL
  
  for (combo in predictor_combinations) {
    for (i in 1:ncol(combo)) {
      predictor_subset <- combo[, i]
      current_AIC <- calculate_AIC(currentmin, "avgSummedPMN", predictor_subset)
      
      if (current_AIC < best_AIC) {
        best_AIC <- current_AIC
        best_model <- paste(predictor_subset, collapse = ", ")
      }
    }
  }
  
  best_models[[as.character(min)]] <- data.frame(predictors = best_model, AIC = best_AIC)
}

best_models <- list.rbind(best_models)

#Average data into 10min bins
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

# Results from model fitting
formerging <- best_models %>% 
  mutate(include = ifelse(grepl("Type", predictors), TRUE, FALSE)) %>%
  mutate(minute = as.numeric(rownames(best_models))) %>%
  mutate(nearest_10 = round_any(as.numeric(minute), 10, round)) %>% 
  group_by(nearest_10) %>%
  mutate(include = if_else(sum(include) > n() / 2, TRUE, FALSE)) %>% 
  select(nearest_10, include) %>% 
  unique() 

identifiedminutes <- formerging %>% subset(include == TRUE)

#Extract included timebins for use in later analyses
#write.csv(identifiedminutes, "/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/identifiedminutes.csv")


### 3c. XGM Modelling  #############################
averagedmodellingdata <- agg_data %>% 
  subset(select = -c(Minute, TimeStamp, SummedPMN, SummedNoise, MicType, Latitude, Longitude)) %>% 
  unique()

dataprep <- averagedmodellingdata 

quartiles <- quantile(dataprep$MeanPMN, probs = c(0.2, 0.4, 0.6, 0.8))

#In case we want to use categorical response variable
dataprep <- dataprep %>%
  mutate(Level = case_when(
    MeanPMN <= quartiles[1] ~ "Low",
    MeanPMN <= quartiles[2] ~ "Medium Low",
    MeanPMN <= quartiles[3] ~ "Medium",
    MeanPMN <= quartiles[4] ~ "Medium High", 
    TRUE ~ "High"
  ))

#In case we want to use binary type variable 
dataprep$Plantationtype <- ifelse(grepl("Plantation", dataprep$Type, ignore.case = TRUE), TRUE, FALSE)
dataprep$Pasturetype <- ifelse(grepl("Pasture", dataprep$Type, ignore.case = TRUE), TRUE, FALSE)
dataprep$NatRegentype <- ifelse(grepl("Natural_Regeneration", dataprep$Type, ignore.case = TRUE), TRUE, FALSE)
dataprep$Reftype <- ifelse(grepl("Reference_Forest", dataprep$Type, ignore.case = TRUE), TRUE, FALSE)
dataprep$PSA <- ifelse(grepl("Reference_Forest", dataprep$Type, ignore.case = TRUE), FALSE, ifelse(grepl("Pasture", dataprep$Type), FALSE, TRUE))


#Data
xgpreddata <- dataprep[,-c(1, 2, 11, 16)]

#Define response and features
x <- colnames(xgpreddata)[-7]
y <- "MeanPMN"

#Random split
set.seed(1) #always put a seed when doing RF,so that results are reproducible
ix <- sample(nrow(xgpreddata), (nrow(xgpreddata)*0.8)) #80% training dataset

xgpreddata <- as.data.frame(xgpreddata)
dtrain <- xgb.DMatrix(data.matrix(xgpreddata[ix, x]),
                      label = xgpreddata[ix, y])
dvalid <- xgb.DMatrix(data.matrix(xgpreddata[-ix, x]),
                      label = xgpreddata[-ix, y])


params <- list(
  objective = "reg:squarederror",
  learning_rate = 0.05,
  subsample = 0.9,
  colsample_bynode = 1,
  reg_lambda = 2,
  max_depth = 5
)

watchlist = list(train=dtrain, test=dvalid)

fit_xgb <- xgb.train(
  params,
  data = dtrain,
  watchlist = watchlist,
  early_stopping_rounds = 20,
  print_every_n = 100,
  nrounds = 10000 # early stopping
)


X <- data.matrix(xgpreddata[sample(nrow(xgpreddata), 1000), x])

shap <- shap.prep(fit_xgb, X_train = X)

# SHAP importance
shap.plot.summary(shap)

#See SHAP python script for adding the binary categories together


### 4. Supplementary Figure 3A  #############################

formerging <- fread("data/identifiedminutes.csv")

agg_data <- ModellingData %>%
  mutate(nearest_10 = round_any(Minute, 10, round)) %>%
  dplyr::group_by(Type, nearest_10) %>%
  mutate(
    MeanPMN    = mean(SummedPMN),
    SEM_PMN    = sd(SummedPMN) / sqrt(n()),
    MeanNoise  = mean(SummedNoise),
    SEM_Noise  = sd(SummedNoise) / sqrt(n())
  ) %>%
  ungroup()

forplotting <- data.frame(nearest_10 = seq(0, 1440, by = 10), time_format = sprintf("%02d:%02d", seq(0, 1440, by = 10) %/% 60, seq(0, 1440, by = 10) %% 60)) %>% 
  left_join(., agg_data, by = "nearest_10") %>% 
  mutate(Type = factor(Type, levels = c("Reference_Forest", "Natural_Regeneration", "Plantation", "Pasture"))) %>% 
  subset(select = c(time_format, nearest_10, MeanPMN, Type, SEM_PMN)) %>% 
  unique()

merged_data <- forplotting %>%
  left_join(formerging, by = c("nearest_10" = "nearest_10"))

# Calculate y range for the markers based on the data
y_min <- min(merged_data$MeanPMN, na.rm = TRUE)
y_max <- max(merged_data$MeanPMN, na.rm = TRUE)
y_range <- 100

# Filter the data for the x-axis markers
timemarker_data <- merged_data %>%
  filter(include == TRUE) %>%
  select(time_format) %>%
  distinct() %>%
  mutate(y_start = y_min - 0.02 * y_range,  # Slightly below the minimum MeanPMN
         y_end = y_max + 0.02 * y_range)     # Slightly above the maximum MeanPMN

# Define colors for each Type group
type_colors <- c("Reference_Forest" = "#228833",
                 "Natural_Regeneration" = "#4477AA",
                 "Plantation" = "#EE6677",
                 "Pasture" = "#CCBB44")

# Plot
merged_data %>%
  ggplot(aes(x = time_format, y = MeanPMN, group = Type, color = Type)) +
  geom_line() +
  # Add shaded error band (mean ± SEM)
  geom_ribbon(
    aes(ymin = MeanPMN - SEM_PMN, ymax = MeanPMN + SEM_PMN, fill = Type),
    alpha = 0.2,  # Transparency
    color = NA,    # No border line
    show.legend = FALSE  # Avoid duplicate legend entries
  ) +
  # Your existing timemarker and styling
  geom_segment(
    data = timemarker_data,
    aes(x = time_format, xend = time_format, y = 520000, yend = 570000),
    inherit.aes = FALSE,
    color = "green",
    alpha = 0.5,
    linewidth = 3 
  ) +
  labs(x = "Time of Day", y = "Mean ΣPMN") +
  scale_x_discrete(breaks = c(paste0("0", 0:9, ":00"), paste(10:23, ":00", sep = ""))) +
  scale_color_manual(name = "Type", values = type_colors) +
  scale_fill_manual(values = type_colors) +  # Match ribbon fill to line colors
  theme_minimal() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("figures/SuppFigure3A_lineplot.pdf")

### 5. Overall Differences Between Soundscape Types ##############################

# ANOVAs on each individual minute, minutes that are printed fail to show statistical significance
minutesincluded <- best_models %>% 
  mutate(include = ifelse(grepl("Type", predictors), TRUE, FALSE)) %>%
  mutate(minute = as.numeric(rownames(best_models))) %>%
  subset(include == TRUE) %>% subset(select = minute) 

minutesincluded <- minutesincluded$minute

for (mins in minutesincluded) {
  trimdata <- ModellingData %>% subset(Minute == mins)
  one.way <- aov(SummedPMN ~ Type, data = trimdata)
  if (summary(one.way)[[1]][["Pr(>F)"]][1] > 0.05) {
    print(mins)
  }
}


# Averaging the values such that each Type has only one value per timebin, perform Friedman Statistical tests for repeated measures within groups

quantify <- averagedmodellingdata %>% select(MeanPMN, Type, nearest_10) %>% unique()
friedman.test(MeanPMN~Type|nearest_10, data=quantify)
pairwise.wilcox.test(quantify$MeanPMN, quantify$Type, p.adj = "bonf")

# Friedman.test
res.friedavg <- quantify %>% friedman_test(MeanPMN~Type|nearest_10)
res.friedavg

# Posthoc test
pwcavg <- quantify %>% 
  wilcox_test(as.formula(paste("MeanPMN", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwcavg

# Boxplots with stats
pwcavg <- pwcavg %>% add_xy_position(x = "Type")
ggboxplot(quantify, x = "Type", y = "MeanPMN", add = "point") +
  stat_pvalue_manual(pwcavg, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.friedavg,  detailed = TRUE),
    caption = get_pwc_label(pwcavg)
  )

