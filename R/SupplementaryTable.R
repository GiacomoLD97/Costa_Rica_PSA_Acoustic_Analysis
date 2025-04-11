#Creating metadata info table

rm(list = ls())
library(dplyr)
library(tidyr)

##### 1. Get the list of all sites and coordinates #####

sites <- read.csv("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Siteswithcoords.csv")
colnames(sites) <- c("Site", "Latitude", "Longitude")

##### 2. Filter those that were included, other failed in recording or at quality check #####

typeandmic <- read.csv("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/sites_type_table.csv")
sitesincluded <- typeandmic$Site

#Any sites not included failed the check at some point
sites <- sites %>% mutate("Recording Failure" = !(Site %in% sitesincluded)) 
sites <- sites %>% mutate("Recording Failure" = ifelse(sites$`Recording Failure` == TRUE, "Yes", "No"))

##### 3. Add type, mic, first date of recording and average canopy height #####

#Format all the messy data from the field
fielddata <- read.csv("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/FieldDataSheet.csv")
fielddata <- fielddata %>% select(Site, PES.Type, Recorder, Date, Canopy.T.start,Canopy.T.mid,Canopy.T.end)
fielddata <- fielddata %>% mutate(across(c(Canopy.T.start,Canopy.T.mid,Canopy.T.end), ~replace_na(., 0)),
                                 "Mean Canopy Height" = rowMeans(across(c(Canopy.T.start,Canopy.T.mid,Canopy.T.end)))) %>%
  subset(select = -c(Canopy.T.start,Canopy.T.mid,Canopy.T.end))
fielddata <- fielddata %>% mutate("Land Use Type" = ifelse(grepl("Ref", PES.Type, ignore.case = T), "Reference Forest", 
                                          ifelse(grepl("Past", PES.Type, ignore.case = T), "Pasture", 
                                                 ifelse(grepl("R", PES.Type, ignore.case = T), "Plantation", 
                                                        ifelse(grepl("C", PES.Type, ignore.case = T), "Natural Regeneration", PES.Type)))))
fielddata <- fielddata %>% subset(select = -PES.Type)
#fielddata <- fielddata %>% mutate(Audiomoth = ifelse(grepl("RFX", Recorder, ignore.case = T), "Gen1.1", "Gen1.2")) %>% subset(select = -Recorder)
fielddata <- fielddata %>% mutate("Microphone Deployment Date" = as.Date(Date, format = "%m.%d.%Y")) %>% subset(select = -Date)

#Remove double row with failed Ref1
fielddata <- fielddata[-23,]

infotable <- merge(x=sites,y=fielddata, by="Site")

##### 4. Add number of annotated minutes from Annotation work #####

#For pasture
setwd("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/Pasture/DAY")
pastanot <- dir()
pastanot <- pastanot[-1]

setwd("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/Pasture/NIGHT")
pastanot2 <- dir()
pastanot2 <- pastanot2[-1]

pastanot <- c(pastanot, pastanot2)

#Natural Regeneration
setwd("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/PSA/conservation/DAY")
natanot <- dir()

setwd("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/PSA/conservation/NIGHT")
natanot <- c(natanot, dir())

#Plantation
setwd("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/PSA/reforestation/DAY")
plantanot <- dir()

setwd("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/PSA/reforestation/NIGHT")
plantanot <- c(plantanot, dir())

#Reference Forest
setwd("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/reference forest/DAY")
refanot <- dir()

setwd("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/Raven_selections/reference forest/NIGHT")
refanot <- c(refanot, dir())

#Join them all
allanots <- c(pastanot, plantanot, refanot, natanot)

#Now count
result <- sub("^[^_]*_[^_]*_[^_]*_([^.]+)\\..*", "\\1", allanots)
count_table <- table(result)
count_df <- as.data.frame(count_table, stringsAsFactors = FALSE)
colnames(count_df) <- c("Site", "Minutes Annotated")

count_df <- count_df[-1,]

#Automatic string removes the .1 from 8.1
count_df[76,1] <- "RefForest8.1"

#Fix the matching name of pasture 18
count_df[1,1] <- "Pasture18"

infotable <- merge(x=infotable, y=count_df, by="Site", all.x = T)

infotable <- infotable %>% replace(is.na(.), 0)

##### 5. Add in climatic variables and distances to road

climate <-  read.csv("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/climaticvariables.csv")
climate <- climate %>% subset(select = c(Site, CHELSA_BIO_Annual_Precipitation, EarthEnvTexture_Contrast_EVI, EarthEnvTopoMed_Elevation, WCS_Human_Footprint_2009))
colnames(climate) <- c("Site", "Mean Annual Percipitation", "Enhanced Vegitation Index (EVI)", "Elevation", "Human Footprint")
infotable <- merge(x=infotable,y=climate, by="Site")

distancetoroad <- read.csv("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/site_locations_distanceToRoad.csv")
distancetoroad <- distancetoroad %>% subset(select = c(Site, distanceToRoad))
infotable <- merge(x=infotable, y=distancetoroad, by = "Site", all.x = TRUE)


##### 6. Export into CSV to add last column (Primary, Secondary, Mature, etc.) ######
setwd("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/")
#write_csv(infotable, "SupplementaryTable1.csv")
