
### 1. LOAD PACKAGES ##############################

#Pacman allows you to install and load (or just load) packages at once
#install.packages(pacman)
library(pacman)
#Now install/load rest of packages
p_load(ggplot2)
p_load(dplyr)
p_load(tidyverse)
p_load(ggbiplot)
p_load(rlist)
p_load(gridExtra)
p_load(caTools)
p_load(randomForest)
p_load(broom)
p_load(ggpubr)
p_load(seewave)
p_load(caret)
p_load(data.table)
p_load(clock)
p_load(lubridate)
p_load(ggplot2)
p_load(tseries)
p_load(ggpubr)
p_load(tmap)
p_load(sf)
p_load(corrplot)
p_load(Hmisc)
p_load(usdm)
p_load(GGally)
p_load(lme4)
p_load(readxl)
p_load(randomForest)



### 2. LIST OF SITES ##############################

#Pull together the final list of sites to be used
#Load the list of output files from PMN analysis

#This list already doesn't include any files labelled as "LowQuality" or "Failed" in initial quality review
setwd("/Volumes/Lacie/output")
sitesincluded <- dir()

#There are a few more outliers and failed recordings that need to be removed

#Below are notes on why they've been removed
#RFX16_31052022_PSA0202-2016 has inflated PMN scores due to microphone drips and clicks
#RFX37_30052022_PSA0098-2013 is a completely failed recording
#RFX38_03062022_RefForest3 Needs to be either removed or June 8th specifically (too much rain inflates PMN values artifically) needs to be removed
#ZH11_03062022_PSA0161-2017 Rains the whole time. No usable signals
#RFX26_03062022_PSA0156-2015 Constant high amplitude background noise (maybe stream) means BGN removal is probably masking any usable signals
#RFX39_22062022_PSA0166-2010 Constant high amplitude background noise (maybe stream) means BGN removal is probably masking any usable signals
#List these sites and remove them
toremove <- c("RFX16_31052022_PSA0202-2016", "RFX37_30052022_PSA0098-2013", "RFX38_03062022_RefForest3",
              "ZH11_03062022_PSA0161-2017", "RFX26_03062022_PSA0156-2015", "RFX39_22062022_PSA0166-2010")
sitesincluded <- sitesincluded[!sitesincluded %in% toremove]

#Site PSA0288-2011 is NOT real, it should always be 0049-2013, I manually fixed this in the harddrives, but always double check

#119 sites are included. Refer back to this list when creating other dataframes to make sure all and only these sites are being included in the analysis

#Make into a dataframe
Sitesincluded <- data.frame(Site = sapply(strsplit(sitesincluded, "_"), tail, 1))

#Add microphone class in case we add it into the models
Sitesincluded$MicType <- substr(sapply(strsplit(sitesincluded, "_"), head, 1), 1, 1)
Sitesincluded$MicType <- ifelse(grepl("R", Sitesincluded$MicType), "Gen1.1", "Gen1.2")

#Add Site Class
Identity <- read.csv("/Users/giacomodelgado/Documents/GitHub/CostaRica/id.csv")

#Make a few edits to ID dataset to make merging easier
Identity$ID <- paste("PSA", Identity$ID, sep="")
Identity$Type <- ifelse(grepl("R", Identity$Type), "Plantation", "Natural_Regeneration")

#Now merge and add site_type
Sitesincluded <- merge(Sitesincluded, Identity, by.x = "Site", by.y = "ID", all.x = TRUE)
Sitesincluded$Type <- ifelse(grepl("Past", Sitesincluded$Site), "Pasture", 
                                 ifelse(grepl("RefF", Sitesincluded$Site), "Reference_Forest", Sitesincluded$Type))








### 3. FIGURE 1, MAP OF SITES ##############################

#Load the coordinates of all the sites/microphones
Coordinates <- read.csv("/Users/giacomodelgado/Documents/GitHub/CostaRica/Acoustic_Index_Files/Siteswithcoords.csv")

#Add coordinates to the sites that are included in the analysis 
Sitesincluded <- merge(Sitesincluded, Coordinates, by.x = "Site", by.y = "CONTRACT", all.x = TRUE)

#Make a spatial element with the data
Sitesincluded_sf <- st_as_sf(Sitesincluded, coords = c("Longitude", "Latitude"), crs = 4326)

#Now we can create the map
site_map <- tm_shape(Sitesincluded_sf) +
  tm_basemap("OpenStreetMap") +
  tm_dots(col = "Type", size = 0.1) +
  tm_layout(title = "GPS Coordinates of Sites") +
  tm_legend(legend.show = FALSE) 
tmap_leaflet(site_map)










### 4. FIGURE 2A, MODELS FOR EACH MINUTE WITH PREDICTIVE VARIABLES ##############################


### 4a. LOAD THE CLIMATIC/PREDICTIVE VARIABLES ##############################

#Extract climatic variables for all sites included from the Crowther Lab composite 
#The code used in Google Earth Engine can be found at the following link: https://code.earthengine.google.com/50d4b0b049b1cbdc90f4d0372663efe5
#Load the climatic data
Climaticdata <- read.csv("/Users/giacomodelgado/Documents/GitHub/CostaRica/climaticvariables.csv")
#Remove unneeded columns
Climaticdata <- Climaticdata %>% subset(select = -c(system.index, .geo))
#Also add in the canopy height as measured on the ground
Canopyheight <- read_excel("/Users/giacomodelgado/Documents/GitHub/FieldDataSheet.xlsx", 
                           col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric","text", "text", "text"))

Canopyheight <- Canopyheight %>% subset(select = c("PES Contract Number", "Canopy T start", "Canopy T mid", "Canopy T end"))
#replace NA with 0
Canopyheight[is.na(Canopyheight)] = 0
#rename
colnames(Canopyheight) <- c("Site", "Tstart", "Tmid", "Tend")
#Average Canopy Height and add it to Climatic Data
Canopyheight$AvgCanopyHeight <- rowMeans(Canopyheight[, c("Tstart", "Tmid", "Tend")])
Canopyheight <- Canopyheight %>% subset(select = c(Site, AvgCanopyHeight))
#Add this to the climatic data 
Climaticdata <- merge(Climaticdata, Canopyheight, by.x = "X.CONTRACT", by.y = "Site", all.x = TRUE)
#Two copies of RefForest1 but only one is real
Climaticdata <- Climaticdata[-122,]
#Rename
colnames(Climaticdata) <- c("Site", "Aridity.Index", "Mean_Temp", "Ann_Precip", "NPP", "EVI", "Elevation", "Soil.Ph", "Human.footprint", "AvgCanopyHeight")

#Test the collinearity of the predictive variables  
cor_matrix <- cor(Climaticdata[,-1], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color")

#credit TomBL
ggpairs(
  Climaticdata[,-1], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag"),
  upper = list(continuous = wrap("cor", size = 5))
)
#According to correlation we should remove Aridity, mean Temperature, precip, NPP and soil pH

#Now test using VIF, credit TomBL
options(scipen = 999)
set.seed(123) 
#Threshold determines sensitivity, although for our data thresholds between 1 and 10 do not change the result
VIF.COR <- vifcor(Climaticdata[,-1], th = 0.5)

# Returns a VIF object, examine different outputs.
VIF.COR.MATRIX <- data.frame(VIF.COR@corMatrix)

# Check which variables are still included.
VIF.COR@excluded

#Recomends removing all the same variables, both recommend removing Annual Precip, but it seems important

ClimforAnalysis <- Climaticdata %>% subset(select = -c(NPP, Mean_Temp, Aridity.Index, Soil.Ph))

#Now merge with sites included to have a full set of metadata for all sites included in the analysis
IncludedSitesMetaData <- merge(Sitesincluded, ClimforAnalysis, by.x = "Site", by.y = "Site", all.x = TRUE)









### 4b. LOAD AND ORGANIZE THE RESULTS OF THE PMN ANALYSIS ##############################

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
#write.csv(ModellingData, "/Users/giacomodelgado/Documents/GitHub/CostaRica/ModellingDataForFigure2oct2023.csv", row.names=FALSE)








### 4c. OVERALL SOUNDSCAPE PATTERNS + VISUALIZING ##############################

#START HERE IF YOU HAVE THE DATAFRAME
ModellingData <- read.csv("/Users/giacomodelgado/Documents/GitHub/CostaRica/ModellingDataForFigure2oct2023.csv")


#We are trying to establish when Site Type is a good predictor and what other factors are driving the differences that we see
#In this graph you can see that when we predict PMN by Minute, there are large differences between Site Types
ModellingData %>% group_by(Site, Minute) %>% mutate(avgSummedPMN = mean(SummedPMN)) %>% 
  ggplot(data = ., aes(x=Minute, y = avgSummedPMN, color = Type)) + geom_smooth(method= 'gam') +
  xlab("Minute of the Day") +
  ylab("SummedPMN")

#This is a line plot version
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

agg_data <- ModellingData %>%
  mutate(nearest_10 = round_any(Minute, 10, round)) %>%
  dplyr::group_by(Type, nearest_10) %>%
  mutate(MeanPMN = mean(SummedPMN), MeanNoise = mean(SummedNoise)) %>%
  ungroup() 

df <- data.frame(nearest_10 = seq(0, 1440, by = 10), time_format = sprintf("%02d:%02d", seq(0, 1440, by = 10) %/% 60, seq(0, 1440, by = 10) %% 60))
forplotting <- agg_data %>% subset(select = c(Minute, nearest_10, MeanPMN, Site, Type)) %>% unique()
forplotting <- merge(forplotting, df, by="nearest_10")

forplotting$time_format <- as.POSIXct(forplotting$time_format, format = "%H:%M")


# Define colors for each Type group
type_colors <- c("Reference_Forest" = "#228833",
                 "Natural_Regeneration" = "#4477AA",
                 "Plantation" = "#EE6677",
                 "Pasture" = "#CCBB44")

custom_order <- c("Reference_Forest", "Natural_Regeneration", "Plantation", "Pasture")
forplotting$Type <- factor(forplotting$Type, levels = custom_order)


forplotting %>% 
  ggplot(aes(x = time_format, y = MeanPMN, group = Type, color = Type)) +
  geom_line() +
  labs(x = "Time of Day", y = "Average PMN Value") +
  scale_color_manual(name = "Type", values = type_colors) +  # Set manual colors
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M")  # Adjust the date_breaks and date_labels as per your preference







### 4d. MODEL PREDICTIVE VARIABLE EFFECTS - LINEAR MIXED MODELS ##############################

#Start by defining a function that returns the AIC of the GLM 
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
  predictors <- c("avgSummedNoise", "Ann_Precip", 
                  "EVI", "Elevation", 
                  "Human.footprint", "AvgCanopyHeight", "Type")
  
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

allbestmodels <- list.rbind(best_models)
countmodels <- allbestmodels %>% subset(select = predictors) %>% count()


### 4e. MODEL PREDICTIVE VARIABLE EFFECTS - RANDOM FOREST MODELS ##############################

#Construct a random Forest model
set.seed(1)

#Use average data and introduce timing as a variable
averagedmodellingdata <- agg_data %>% subset(select = -c(Minute, TimeStamp, SummedPMN, SummedNoise, MicType, Latitude, Longitude)) %>% unique()

quartiles <- quantile(averagedmodellingdata$MeanPMN, probs = c(0.25, 0.5, 0.75))

# Create a new column "Level" based on the quartiles
averagedmodellingdata <- averagedmodellingdata %>%
  mutate(Level = case_when(
    MeanPMN <= quartiles[1] ~ "Low",
    MeanPMN <= quartiles[2] ~ "Medium Low",
    MeanPMN <= quartiles[3] ~ "Medium High",
    TRUE ~ "High"
  ))

averagedmodellingdata$Level = factor(averagedmodellingdata$Level) 


rfmodel <- randomForest(formula = MeanPMN ~ Ann_Precip + AvgCanopyHeight + Type + EVI + Human.footprint + Elevation + MeanNoise + nearest_10, data = averagedmodellingdata)
which.min(rfmodel$mse)
sqrt(rfmodel$mse[which.min(rfmodel$mse)]) 
plot(rfmodel)
varImpPlot(rfmodel) 

#Also can performed using categorical response variable
rfmodel2 <- randomForest(formula = Level ~ Ann_Precip + AvgCanopyHeight + Type + EVI + Human.footprint + Elevation + MeanNoise + nearest_10, data = averagedmodellingdata)
which.min(rfmodel2$mse)
sqrt(rfmodel$mse[which.min(rfmodel2$mse)]) 
plot(rfmodel2)
varImpPlot(rfmodel2) 


#Also can be performed using the most commonly selected model in the previous step
rfmodelopt <- randomForest(formula = MeanPMN ~ Ann_Precip + AvgCanopyHeight + Type + nearest_10, data = averagedmodellingdata)
which.min(rfmodelopt$mse)
sqrt(rfmodelopt$mse[which.min(rfmodelopt$mse)]) 
plot(rfmodelopt)
varImpPlot(rfmodelopt) 




### 4e. LINEPLOT FOR SOUNDSCAPE TYPE WITH TIMES IDENTIFIED FROM 4d ##############################

df <- data.frame(nearest_10 = seq(0, 1440, by = 10), time_format = sprintf("%02d:%02d", seq(0, 1440, by = 10) %/% 60, seq(0, 1440, by = 10) %% 60))
forplotting <- agg_data %>% subset(select = c(Minute, nearest_10, MeanPMN, Site, Type)) %>% unique()
forplotting <- merge(forplotting, df, by="nearest_10")
forplotting$Type <- ifelse(grepl("Ref", forplotting$Type, ignore.case = TRUE), "Reference Forests", forplotting$Type)
forplotting$Type <- ifelse(grepl("Nat", forplotting$Type, ignore.case = TRUE), "Natural Regeneration", forplotting$Type)



# Define colors for each Type group
type_colors <- c("Reference Forests" = "#228833",
                 "Natural Regeneration" = "#4477AA",
                 "Plantation" = "#EE6677",
                 "Pasture" = "#CCBB44")

custom_order <- c("Reference Forests", "Natural Regeneration", "Plantation", "Pasture")
forplotting$Type <- factor(forplotting$Type, levels = custom_order)

#Add in results from model
allbestmodels$include <- ifelse(grepl("Type", allbestmodels$predictors), TRUE, FALSE)
allbestmodels$nearest <- as.numeric(rownames(allbestmodels))
allbestmodels<-allbestmodels %>%
  mutate(nearest = round_any(nearest, 10, round)) 
formerging <- allbestmodels %>% subset(select = c(nearest, include))
formerging <- formerging %>%
  group_by(nearest) %>%
  mutate(include = if_else(sum(include) > n() / 2, TRUE, FALSE)) %>% unique() 

#Extract included timebins for use in later analyses
#write.csv(formerging, "/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/identifiedminutes.csv")

merged_data <- forplotting %>%
  left_join(formerging, by = c("nearest_10" = "nearest"))


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


# Plot
merged_data %>%
  ggplot(aes(x = time_format, y = MeanPMN, group = Type, color = Type)) +
  geom_line() +
  geom_segment(
    data = timemarker_data,
    aes(x = time_format, xend = time_format, y = 520000, yend = 570000),
    inherit.aes = FALSE,
    color = "green",
    alpha = 0.5,
    linewidth = 3 
  ) +
  labs(x = "Time of Day", y = "Average ΣPMN") +
  scale_x_discrete(breaks = c(paste("0", 0:9, ":00", sep = ""), paste(10:23, ":00", sep = ""))) +
  scale_color_manual(name = "Type", values = type_colors) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate the text on the X axis
  )

ggsave("/Users/giacomodelgado/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/figures/Figure2A_lineplot.pdf")

### 5. SIMPLY EVALUATE WHETHER THERE ARE SIGNIFICANT OVERALL DIFFERENCES BETWEEN SOUNDSCAPES ##############################

#Just using simple ANOVA's on each individual minute, minutes that are printed fail to show statistical significance

for (mins in minutesincluded) {
  trimdata <- ModellingData %>% subset(Minute == mins)
  one.way <- aov(SummedPMN ~ Type, data = trimdata)
  if (summary(one.way)[[1]][["Pr(>F)"]][1] > 0.05) {
    print(mins)
  }
}

#Visualization
ggboxplot(ModellingData, x = "Type", y = "SummedPMN", group = "Minute", add = "jitter")

ggplot(ModellingData, aes(x = as.factor(Minute), y = SummedPMN, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +  # This removes outlier points if they appear
  labs(title = "Per-Minute Average by Treatment",
       x = "Minute",
       y = "Average Per Minute (avgPMN)",
       fill = "Treatment Type") +
  theme_minimal()


#By averaging the values such that each Type has only one value per timebin we can use Friedman Statistical tests for repeated measures within groups

quantify <- averagedmodellingdata %>% subset(select = c(MeanPMN, Type, nearest_10)) %>% unique()
friedman.test(MeanPMN~Type|nearest_10, data=quantify)
pairwise.wilcox.test(quantify$MeanPMN, quantify$Type, p.adj = "bonf")

#Friedman.test
res.friedavg <- quantify %>% friedman_test(MeanPMN~Type|nearest_10)
res.friedavg

#Posthoc test
pwcavg <- quantify %>% 
  wilcox_test(as.formula(paste("MeanPMN", "Type", sep="~")), paired = TRUE, p.adjust.method = "bonferroni")
pwcavg

#Boxplots with stats
pwcavg <- pwcavg %>% add_xy_position(x = "Type")
ggboxplot(quantify, x = "Type", y = "MeanPMN", add = "point") +
  stat_pvalue_manual(pwcavg, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.friedavg,  detailed = TRUE),
    caption = get_pwc_label(pwcavg)
  )

