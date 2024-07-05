rm(list = ls())
library(foreach)
library(doParallel)
library(data.table)
library(tidyverse)

sitesincluded <- list.files('output/')


#Remove files that failed quality check
toremove <- c("RFX16_31052022_PSA0202-2016", "RFX37_30052022_PSA0098-2013", "RFX38_03062022_RefForest3",
              "ZH11_03062022_PSA0161-2017", "RFX26_03062022_PSA0156-2015", "RFX39_22062022_PSA0166-2010")

sitesincluded <- sitesincluded[!sitesincluded %in% toremove]

# Set up frequency breaks
breaks <- c(0, seq(1, 24))
labels <- paste0(head(breaks, -1), "-", tail(breaks, -1), " kHz")

# Set up cluster
num_cores <- detectCores() - 1  
registerDoParallel(cores=num_cores)

MinuteConverter <- function(r) {
  h <- as.numeric(substring(r, 1, 2))
  m <- as.numeric(substring(r, 3, 4))
  s <- as.numeric(substring(r, 5, 6)) 
  minuteofday <- (h*60) + (m) + (s/60)
}

for (folder in sitesincluded){
  files <- list.files(paste0('output/', folder), full.names = T)
  
  SiteName = strsplit(folder, "_")[[1]][3]
  
  process_file <- function(file) {
    df <- fread(file)
    
    StartTime <- sprintf("%06s", strsplit(file, "_")[[1]][6])
    
    #Extract the date of the recording
    RecordingDay <- strsplit(file, "_")[[1]][5]
    
    # Convert both day and time to a POSIT
    TimeStamp <- strptime(paste(RecordingDay, StartTime), format = "%Y%m%d %H%M%S")
    
    df <- df %>%
      mutate(TimeStamp = TimeStamp + (Minute*60 - 60)) %>% 
      mutate(Minute = Minute + MinuteConverter(StartTime)) %>%
      mutate(Minute = if_else(Minute > 1440, (Minute - 1440), Minute)) %>%
      mutate(Minute = round(Minute, digits = 0),
             Site = SiteName) #%>%
    # group_by(Site,Minute, Fre quency) %>%
    # summarise(PMN = mean(PMN), Noise = mean(Noise)) %>%
    # unique()
    
    return(df)
    
  }
  
  results <- foreach(file = files, 
                     # .combine = rbindlist, 
                     .packages=c('data.table', 'dplyr')) %dopar% {
                       process_file(file)
                     }
  
  results <- as.data.frame(bind_rows(results)) %>%
    mutate(freq = Frequency * 93.75/1000) %>%
    mutate(freq_category = cut(freq, breaks = breaks, labels = labels, right = TRUE)) %>%
    group_by(Site, TimeStamp, Minute, freq_category) %>%
    summarise(sum_PMN = sum(PMN),
              Noise = sum(Noise)) 
  
  fwrite(results, paste0('site_freq_data_byDay/', SiteName, '.csv'), sep = ",")
}

