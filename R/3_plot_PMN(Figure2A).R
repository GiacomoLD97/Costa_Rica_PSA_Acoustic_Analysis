rm(list = ls())
library(data.table)
library(dplyr)
library(lubridate)
library(patchwork)
library(scales)
library(foreach)
library(doParallel)
library(tidyverse)

`%notin%` <- Negate(`%in%`)
breaks <- c(0, seq(1, 24))
labels <- paste0(head(breaks, -1), "-", tail(breaks, -1), " kHz")
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}


type_table <- #fread('data/sites_type_table.csv')
  fread("~/Downloads/site_freq_data_full.csv")

# Read all the files in the folder
# files <- list.files('/Users/johanvandenhoogen/ETH/Projects/costa_rica/site_freq_data', full.names = T)
# df <- do.call("rbind", lapply(files, fread))

# Combine with type info
# df <- df %>% left_join(., type_table %>% select(-MicType), by = "Site")
# fwrite(df, '/Users/johanvandenhoogen/ETH/Projects/costa_rica/site_freq_data_full.csv')

df <- #fread('data/site_freq_data_full.csv') %>% 
  fread("/Users/giacomodelgado/Documents/PhD Chapter 1/Data not in GitHub/site_freq_data_full.csv") %>%
  mutate(Type = ifelse(Type == "", NA, Type)) %>% 
  na.omit() %>% 
  filter(Minute %notin% c(385, 386, 1080)) 
  
# Summarise data by type and write to file
df_type_summary <- df %>%
  group_by(Type, Minute, freq_category) %>%
  dplyr::summarise(
    mean_PMN = mean(sum_PMN, na.rm = TRUE))

#fwrite(df_type_summary, 'data/site_freq_data_perType_summarised.csv')
df_type_summary <- fread('data/site_freq_data_perType_summarised.csv')

# Plot by types, for each frequency bin
p <- df_type_summary %>% 
  subset(freq_category %in% c('0-1 kHz', '1-2 kHz', '2-3 kHz', '3-4 kHz', '4-5 kHz', '5-6 kHz', '6-7 kHz', '7-8 kHz', '8-9 kHz')) %>%
  # filter(Minute >= 285 & Minute <= 320) %>% 
  mutate(nearest_10 = round_any(Minute, 10, round)) %>%
  group_by(freq_category, Type, nearest_10) %>%
  dplyr::summarise(ten_mean_PMN = mean(mean_PMN)) %>%
  ggplot(aes(x = nearest_10, y = ten_mean_PMN, color = Type)) + 
  scale_color_manual(values = c("#4477AA", "#CCBB44", "#EE6677", "#228833")) +
  # geom_ribbon(aes(ymin = mean_PMN - sd_PMN, ymax = mean_PMN + sd_PMN), color = NA, alpha = 0.3) +
  geom_line() +  facet_wrap(~ freq_category) +
  theme_minimal() +
  ylab("Mean Power-minus-Noise")  +
  xlab("Minute of the Day")

p
#ggsave('figures/pmn_per_bin.pdf', plot = p)

#Post revisions version, adding in the means standard error and frequency 0-1 and 4-5 (was misssing previously)
fig2a <- df %>% 
  subset(freq_category %in% c('0-1 kHz', '1-2 kHz', '2-3 kHz', '3-4 kHz', '4-5 kHz', '5-6 kHz', '6-7 kHz', '7-8 kHz', '8-9 kHz')) %>%
  mutate(nearest_10 = round_any(Minute, 10, round)) %>%
  
  # Modify Type variable
  mutate(Type = factor(Type,
                       levels = c("Reference_Forest", "Natural_Regeneration", "Plantation", "Pasture"),
                       labels = c("Reference Forest", "Natural Regeneration", "Plantation", "Pasture"))) %>%
  
  group_by(freq_category, Type, nearest_10) %>%
  dplyr::summarise(
    ten_mean_PMN = mean(sum_PMN),
    sd_PMN = sd(sum_PMN),
    n = n(),
    se_PMN = sd_PMN/sqrt(n),
    lower = ten_mean_PMN - se_PMN,  # Using SD instead of SE for more visible bands
    upper = ten_mean_PMN + se_PMN
  ) %>%
  
  ggplot(aes(x = nearest_10, y = ten_mean_PMN, color = Type, fill = Type)) + 
  scale_color_manual(values = c("#228833", "#4477AA", "#EE6677", "#CCBB44")) +
  scale_fill_manual(values = c("#228833", "#4477AA", "#EE6677", "#CCBB44")) +
  
  # Ribbon with more visible parameters
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              color = NA,  # No border color
              alpha = 0.4, # More transparency
              linetype = 0) + # No border line
  
  geom_line(linewidth = 0.3) +  # Make lines slightly thicker
  facet_wrap(~ freq_category) +
  theme_minimal() +
  ylab("Mean Power-minus-Noise") +
  xlab("Minute of the Day") +
  labs(color = "Type", fill = "Type")  # Consistent legend titles

fig2a
#ggsave('figures/Figure2A_perfreqbin.pdf', plot = fig2a)

###


df_summarised_10min <- df %>%
  # filter(freq_category %in% c('5-6 kHz', '6-7 kHz', '7-8 kHz', '8-9 kHz')) %>%
    filter(freq_category %in% c('3-4 kHz', '4-5 kHz', '5-6 kHz', '6-7 kHz', '7-8 kHz', '8-9 kHz')) %>%
  mutate(nearest_10 = round_any(Minute, 10, round)) %>% 
  group_by(Type, nearest_10) %>%
  dplyr::summarise(
    mean_PMN = mean(sum_PMN, na.rm = TRUE)) %>% 
  mutate(tod = as.POSIXct("2023-12-13 00:00:00") + minutes(nearest_10))

wasserstein10min <- fread('data/wasserstein_dist_results_10minavg.csv')

### Figure 3B #####

forplot_summarised_10min <- df %>%
  filter(freq_category %in% c('3-4 kHz', '4-5 kHz', '5-6 kHz', '6-7 kHz', '7-8 kHz', '8-9 kHz')) %>%
  mutate(nearest_10 = round_any(Minute, 10, round)) %>% 
  group_by(Type, nearest_10) %>%
  dplyr::summarise(
    mean_PMN = mean(sum_PMN, na.rm = TRUE),
    sem_PMN  = sd(sum_PMN, na.rm = TRUE) / sqrt(n()),  # Standard Error
    .groups = "drop"
  ) %>% 
  mutate(tod = as.POSIXct("2023-12-13 00:00:00") + minutes(nearest_10))

p1 <- forplot_summarised_10min %>%
  ggplot(aes(x = tod, y = mean_PMN, color = Type, fill = Type)) +  # Added `fill` aesthetic
  geom_line(linewidth = 0.8) +  
  geom_ribbon(
    aes(ymin = mean_PMN - sem_PMN, ymax = mean_PMN + sem_PMN),
    alpha = 0.15,    # Subtle transparency
    color = NA,      # No border lines
    show.legend = FALSE  # Avoid duplicate legend
  ) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_x_datetime(labels = date_format("%H:%M", tz = "Europe/Amsterdam"), date_minor_breaks = "hour") +
  ylab("Mean Power-minus-Noise") + xlab("Time of Day") +
  scale_color_manual(
    values = c("#4477AA", "#CCBB44", "#EE6677", "#228833"), 
    breaks = c("Natural_Regeneration", "Pasture", "Plantation", "Reference_Forest"), 
    labels = c("Natural Regeneration", "Pasture", "Plantation", "Reference Forest")
  ) +
  scale_fill_manual(
    values = c("#4477AA", "#CCBB44", "#EE6677", "#228833"), 
    guide = 'none'
  ) +
  scale_y_continuous(labels = scales::comma, limits = c(1e5, 8.5e5)) +
  guides(color = guide_legend(position = "inside")) +
  theme(
    legend.position.inside = c(0.8, 0.9),
    legend.title = element_blank()
  ) +
  geom_vline(
    color = 'grey50', 
    xintercept = as.POSIXct("2023-12-13 00:00:00") + minutes(315), 
    linetype = 'dashed', 
    linewidth = 0.5
  ) +
  geom_vline(
    color = 'grey50', 
    xintercept = as.POSIXct("2023-12-13 00:00:00") + minutes(1070), 
    linetype = 'dashed', 
    linewidth = 0.5
  )
p1

ggsave('figures/Figure3B_closestlineplots.pdf', plot = p1)

# legend <- cowplot::get_legend(p1)

# ggsave('/Users/johanvandenhoogen/ETH/Projects/costa_rica/figures/plot_TC_1_legend.png', legend, width = 3, height = 3, dpi = 300)
# 

# p2 <- ggplot() +
#   geom_col(data = wasserstein10min %>% 
#              rename(Type = closest_type) %>% 
#              mutate(tod = as.POSIXct("2023-12-13 00:00:00") + minutes(nearest_10)), 
#            aes(x = tod, y = 10000, fill = Type), color = NA) +
#   theme_minimal() +
#   ylab('') +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank(),
#         # legend.position = "none",
#         aspect.ratio = 0.025) + 
#   scale_fill_manual(values = c("#4477AA", "#CCBB44", "#EE6677", "#228833")) +
#   scale_x_datetime(labels = date_format("%H:%M", tz = "Europe/Amsterdam"), date_breaks = "2 hours") +
#   xlab('')
# 
# p1 + p2$layers[[1]] + scale_fill_manual(values = c("#4477AA", "#CCBB44", "#EE6677", "#228833"), guide = 'none')


# Group by nearest 10 minutes and sum PMN
df_forWasserstein <- df %>%
  mutate(nearest_10 = round_any(Minute, 10, round)) %>% 
  group_by(Site, Type, nearest_10, freq_category) %>% 
  dplyr::summarise(mean_PMN = mean(sum_PMN, na.rm = TRUE))

# function to calculate Wasserstein Distance
calculate_wasserstein_freq <- function(type1, type2, minute, freq) {
  data1 <- df_freq %>% filter(Type == type1 & nearest_10 == minute) %>% pull(mean_PMN)
  data2 <- df_freq %>% filter(Type == type2 & nearest_10 == minute) %>% pull(mean_PMN)
  return(scipy$wasserstein_distance(data1, data2))
}

# If possible skip wasserstein calculation as it takes a while to complete
# Define cluster size
cl <- makeCluster(14)
registerDoParallel(cl)

out <- list()
for (freq in c('0-1 kHz', '1-2 kHz', '2-3 kHz', '3-4 kHz', '4-5 kHz', '5-6 kHz', '6-7 kHz', '7-8 kHz', '8-9 kHz')) {
  df_freq <- df_forWasserstein %>% filter(freq_category == freq)
  # Run analysis
  results <- foreach(min = seq(0,1440,10), .combine = rbind, .packages = c('tidyverse', 'reticulate')) %dopar% {
    use_condaenv(condaenv = "ee")
    scipy <- import("scipy.stats")

    distance_nat <- calculate_wasserstein_freq("Reference_Forest", "Natural_Regeneration", min)
    distance_past <- calculate_wasserstein_freq("Reference_Forest", "Pasture", min)
    distance_plant <- calculate_wasserstein_freq("Reference_Forest", "Plantation", min)

    closest_type <- which.min(c(distance_nat, distance_past, distance_plant))

    return(data.frame(nearest_10 = min, distance_nat, distance_past, distance_plant, closest_type))
  }
factor(c(1,2,3), labels = c("Natural_Regeneration", "Pasture", "Plantation"))
  # Remap names
  results$closest_type <- factor(results$closest_type, labels = c("Natural_Regeneration", "Pasture", "Plantation"))
  results$freq_category <- freq

  out[[freq]] <- results
}

stopCluster(cl)

wasserstein10min_freq <- do.call(rbind, out)
fwrite(wasserstein10min_freq, '/Users/johanvandenhoogen/ETH/Projects/costa_rica/wasserstein10min_freq_toRefForest.csv')

cl <- makeCluster(14)
registerDoParallel(cl)

out <- list()
for (freq in c('0-1 kHz', '1-2 kHz', '2-3 kHz', '3-4 kHz', '4-5 kHz', '5-6 kHz', '6-7 kHz', '7-8 kHz', '8-9 kHz')) {
  df_freq <- df_forWasserstein %>% filter(freq_category == freq)
  # Run analysis
  results <- foreach(min = seq(0,1440,10), .combine = rbind, .packages = c('tidyverse', 'reticulate')) %dopar% {
    use_condaenv(condaenv = "ee")
    scipy <- import("scipy.stats")
    
    distance_nat <- calculate_wasserstein_freq("Pasture", "Natural_Regeneration", min)
    distance_ref <- calculate_wasserstein_freq("Pasture", "Reference_Forest", min)
    distance_plant <- calculate_wasserstein_freq("Pasture", "Plantation", min)
    
    closest_type <- which.min(c(distance_nat, distance_ref, distance_plant))
    
    return(data.frame(nearest_10 = min, distance_nat, distance_ref, distance_plant, closest_type))
  }
  
  # Remap names
  results$closest_type <- factor(results$closest_type, levels = c(1,2,3), labels = c("Natural_Regeneration", "Reference_Forest", "Plantation"))
  results$freq_category <- freq
  
  out[[freq]] <- results
}

stopCluster(cl)

wasserstein10min_freq_past <- do.call(rbind, out)
fwrite(wasserstein10min_freq_past, '/Users/johanvandenhoogen/ETH/Projects/costa_rica/wasserstein10min_freq_toPasture.csv')

wasserstein10min_freq <- fread('/Users/johanvandenhoogen/ETH/Projects/costa_rica/wasserstein10min_freq_toRefForest.csv')

plot_list <- list()
# For each frequency bin, plot the closest type
for (freq in c('0-1 kHz', '1-2 kHz', '2-3 kHz', '3-4 kHz', '4-5 kHz', '5-6 kHz', '6-7 kHz', '7-8 kHz', '8-9 kHz')) {
  plot <- df %>%
    filter(freq_category == freq) %>%
    mutate(nearest_10 = round_any(Minute, 10, round)) %>% 
    group_by(Type, nearest_10) %>%
    dplyr::summarise(
      mean_PMN = mean(sum_PMN, na.rm = TRUE)) %>% 
    mutate(tod = as.POSIXct("2023-12-13 00:00:00") + minutes(nearest_10)) %>%
    ggplot(aes(x = tod, y = mean_PMN, color = Type)) + 
    geom_line() +  
    theme_classic() +
    theme(strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 12)) +
    scale_x_datetime(labels = date_format("%H:%M", tz = "Europe/Amsterdam"), date_breaks = "4 hours") +
    ylab("Mean Power-minus-Noise") + xlab("Time of Day") +
    scale_color_manual(values = c("#4477AA", "#CCBB44", "#EE6677", "#228833"),
                       labels = c("Natural Regeneration", "Pasture", "Plantation", "Reference Forest")) +
    ggtitle(freq) +
    coord_cartesian(ylim = c(0, 1e6)) +
    geom_col(data = wasserstein10min_freq %>% 
               filter(freq_category == freq) %>% 
               rename(Type = closest_type) %>% 
               mutate(tod = as.POSIXct("2023-12-13 00:00:00") + minutes(nearest_10)), 
             aes(x = tod, y = 25000, fill = Type), inherit.aes = FALSE) +
    scale_fill_manual(values = c("#4477AA", "#CCBB44", "#EE6677", "#228833"), guide = 'none') 
  
  plot_list[[freq]] <- plot
}

remove_x <- theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank())
remove_xy <- theme(axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank())
remove_y <- theme(axis.text.y = element_blank(),
                  axis.title.y = element_blank(),
                  axis.ticks.y = element_blank())

plot_list[[1]] + remove_x + 
  plot_list[[2]] + remove_xy +
  plot_list[[3]] + remove_xy +
  plot_list[[4]] + remove_x +
  plot_list[[5]] + remove_xy +
  plot_list[[6]] + remove_xy +
  plot_list[[7]] + 
  plot_list[[8]] + remove_y +
  plot_list[[9]] + remove_y +
  plot_layout(ncol = 3, guides = 'collect')

ggsave('/Users/johanvandenhoogen/ETH/Projects/costa_rica/Figure2A_perfreqbin.pdf')





