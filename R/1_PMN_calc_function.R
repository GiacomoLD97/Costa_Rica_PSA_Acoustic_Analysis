#' @title PMN Calculation Function
#' @description This function calculates the Power Minus Noise (PMN) index for each minute of a wav file.
#' 
#' @param files A character vector of file paths to the wav files to be processed.
#' @out return a data frame containing the PMN values or write to csv.
#' @param parallel An integer specifying the number of cores to use for parallel processing. If NULL, parallel processing will not be used.
#' 
#' @return A data frame containing the PMN values for each minute of each wav file.

# Check if RcppRollMeandB is installed, if not install from github
if (!requireNamespace("RcppRollMeandB", quietly = TRUE)) {
  devtools::install_github("hooge104/RcppRollMeandB")
}

# Load packages
library(seewave)
library(tuneR)
library(warbleR)
library(zoo)
library(stringr)
library(foreach)
library(Rcpp)
library(doParallel)
library(RcppRollMeandB)
library(data.table)
library(tidyverse)

calc_PMN <- function(files = files, out = 'df', parallel = 8){ 
  # if parallel is not NULL, set up cluster with n cores
  if (parallel) {
    # Set up cluster
    cl <- makeCluster(parallel, type = "PSOCK")
    registerDoParallel(cl)
  } 
  
  # if files is a directory, get all wav files in the directory
  if (dir.exists(files)) {
    files <- list.files(files, pattern = ".wav", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  }
  
  # PMN calculation function
  results <- foreach(i = files,
                     .combine = rbind,
                     .inorder = TRUE,
                     .packages = (.packages())
  ) %dopar% {
    # Get duration of each wav file, in minutes
    length <- duration_wavs(i) %>% pull(duration) %>% `/`(60) %>% floor()
    
    # Get file name by removing '.wav' or . '.WAV' from file path with str_remove
    filename <- str_remove(str_remove(basename(i), ".wav"), ".WAV")
    
    # # Get date and time from filename. Filename must contain date time in format YYYYMMDD_HHMMSS
    # if (filename %>% str_detect("\\d{8}_[0-9]{6}")) {
    #   day <- str_extract(filename, "\\d{8}(?=_[0-9]{6})")```````````````````````
    #   start_time <- sprintf("%06s", str_extract(filename, "(?<=\\d{8}_)\\d{6}"))
    # } else {
    #   print(paste0("Filename ", filename, " does not match expected format (YYYYMMDD_HHMMSS)"))
    #   day <- NA
    #   start_time <- NA
    # }
    
    # If writing to csv file, set output path
    if (out != 'df') {
      output_path <- out
      
      # Create output directory if it does not exist
      if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE)
      }
    }
    # If output file already exists, skip
    if (file.exists(paste0(output_path, "/", filename, ".csv"))) {
      print(paste0(filename, " already processed, skipping..."))
      return(NULL)
    } else {
      # Loop over each minute of the wav file
      POWcalculation <- lapply(seq(0, length - 1), function(k) {
        # Read  wav file
        sound <- readWave(i, from = k, to = k + 1, units = 'minutes')
        
        # Calculate amplitude
        raw.spectro <- seewave::spectro(sound, wl = 512, wn = "hamming", ovlp = 0, plot = FALSE)$amp
        
        # Smooth the noise profile using a moving average filter
        raw.spectro_roll <- pmax(RcppRollMeandB::roll_meandB_wrap(raw.spectro, windowSize = 3), -90)
        
        # Calculate the mode of the smoothed noise profile
        spectro.mode <- apply(raw.spectro_roll, 1, RcppRollMeandB::dB.mode.per.row)
        
        # Smooth the mode using a moving average filter
        spectro.mode <- RcppRollMeandB::roll_meandB_vector_wrap(spectro.mode, windowSize = 5)
        
        # Subtract the resulting background noise values from the values in each frequency bin. Truncate negative values to zero.
        spectro.less.mode <- pmax(raw.spectro - spectro.mode, 0)
        
        # For each neighbourhood (3 frames × 9 frequency bins) centred on any element/pixel in the spectrogram, calculate the average spectrogram value, ā.
        # If ā is less than a user determined threshold, θ, set the value of the central element/pixel equal to the minimum in the neighbourhood
        ale.matrix <- RcppRollMeandB::roll_meandB_threshold_wrap(spectro.less.mode, windowRowSize = 9, windowColSize = 3, threshold = 3)
        
        # Calculate Power Minus Noise (PMNsp)
        PMN <- apply(ale.matrix, 1, sum) 
        
        # Return data frame with PMN values
        df <- data.table("Frequency" = 1:256, "PMN" = PMN, "Noise" = spectro.mode, "Minute" = k)
        
        return (df)
      })
      
      # Combine list of data frames into data frame
      df <- do.call(rbind,POWcalculation)
      
      if (out != 'df') {
        # Write to file
        fwrite(df, file = paste0(output_path, "/", filename, ".csv"))
      } else {
        return(df)
      }
    }
  }
  # Stop cluster
  if (parallel) {
    stopCluster(cl)
  }
  return(results)
}

# Example usage
calc_PMN(files = 'data/wav',
         out = 'tmp', 
         parallel = 14)







