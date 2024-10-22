# Costa Rica PSA Acoustic Analysis

#Script 1 contains the code to calculate PMN on every minute and frequency band of recording from the full raw dataset of recordings

#Script 2 Processes this output and organizes it by Site, also adding Recording time of Day

#Script 3 Plots the PMN values per Site-type for every 1khz band from 0-9

#Script 4 Performs the modelling which underlies the temporal trimming of acoustic space

#Script 5 Estimates the Frequency Bands of four main taxanomic groups which underlies the frequency trimming of acoustic space

#Script 6 Calculates the Wasserstein distances and records the closest type for each TFB (time frequency bin)

#Script 7 Processes the results of the Wasserstein Distance calculations (python script), testing for significance


#Note: There are some especially large files called that are not saved within the GitHub Repo. Raw Audio files can be found on the ETH LibDrive library for large datasets. All other large dataframes can be calculated with above scripts from those files, processing however, may be slow (days-weeks).

