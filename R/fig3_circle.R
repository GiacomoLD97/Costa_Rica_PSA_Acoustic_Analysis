#Johan version

df_summarised_10min %>% 
  ggplot(aes(x = tod, y = mean_PMN, color = Type)) + 
  geom_line(linewidth = 0.8) +  
  theme_void() +
  theme(strip.background = element_blank()) +
  scale_x_datetime(labels = date_format("%H:%M", tz = "Europe/Amsterdam"), date_breaks = "1 hour", date_minor_breaks = "hour") +
  ylab("Mean Power-minus-Noise") + xlab("Time of Day") +
  scale_color_manual(values = c("#4477AA", "#CCBB44", "#EE6677", "#228833"), 
                     breaks = c("Natural_Regeneration", "Pasture", "Plantation", "Reference_Forest"), 
                     labels = c("Natural Regeneration", "Pasture", "Plantation", "Reference Forest")) +
  scale_fill_manual(values = c("#4477AA", "#CCBB44", "#EE6677", "#228833"), guide = 'none') +
  theme(legend.title = element_blank()) +
  # coord_polar(theta = "x", start = pi/2) +
  coord_radial(theta = "x", start = pi/2, inner.radius = 0.3, expand = F) +
  # geom_vline(color = 'grey50', xintercept = as.POSIXct("2023-12-13 00:00:00") + minutes(315), linetype = 'dashed', linewidth = 0.5) +
  # geom_vline(color = 'grey50', xintercept = as.POSIXct("2023-12-13 00:00:00") + minutes(1070), linetype = 'dashed', linewidth = 0.5) +
  theme(axis.text.x = element_text(size = 10, color = "black"),
        panel.grid.major.x = element_line(colour = "grey", size = 0.1),
  ) 


#Giacomo version
#Use forplotting from Modelling.R

# Ensure forplotting dataset is properly formatted


forplotting$time_format <- as.POSIXct(forplotting$time_format, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Amsterdam")

start_time <- as.POSIXct("2024-07-17 00:00:00", tz = "Europe/Amsterdam")
end_time <- as.POSIXct("2024-08-17 00:00:00", tz = "Europe/Amsterdam")
sunrise_time <- as.POSIXct("2024-07-17 05:20:00", tz = "Europe/Amsterdam")
sunset_time <- as.POSIXct("2024-07-17 18:05:00", tz = "Europe/Amsterdam")

# Function to find local maxima within a specified time range for each type
find_local_maxima <- function(data, start_time, end_time) {
  data %>%
    filter(time_format >= start_time & time_format <= end_time) %>%
    group_by(Type) %>%
    slice(which.max(MeanPMN))
}

# Find local maxima for each type between 4am-6am and 5:30pm-7:30pm
maxima_4_6am <- find_local_maxima(forplotting, as.POSIXct("2024-07-17 04:00:00", tz = "Europe/Amsterdam"), as.POSIXct("2024-07-17 06:00:00", tz = "Europe/Amsterdam"))
maxima_530_730pm <- find_local_maxima(forplotting, as.POSIXct("2024-07-17 17:30:00", tz = "Europe/Amsterdam"), as.POSIXct("2024-07-17 19:30:00", tz = "Europe/Amsterdam"))

y_shifts <- c("Natural_Regeneration" = 2, "Pasture" = 0.6, "Plantation" = -0.3, "Reference_Forest" = -0.6)

forplotting %>%
  ggplot(aes(x = time_format, y = MeanPMN, color = Type)) +
  geom_line(linewidth = 0.8) +
  theme_void() +
  theme(strip.background = element_blank()) +
  scale_x_datetime(
    labels = date_format("%H:%M", tz = "Europe/Amsterdam"), 
    breaks = seq.POSIXt(start_time, end_time, by = "2 hours")
  ) +
  ylab("Mean Power-minus-Noise") + 
  xlab("Time of Day") +
  scale_color_manual(values = c("#4477AA", "#CCBB44", "#EE6677", "#228833"), 
                     breaks = c("Natural_Regeneration", "Pasture", "Plantation", "Reference_Forest"), 
                     labels = c("Natural Regeneration", "Pasture", "Plantation", "Reference Forest")) +
  scale_fill_manual(values = c("#4477AA", "#CCBB44", "#EE6677", "#228833"), guide = 'none') +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = as.numeric(sunrise_time), color = '#e69b00', linetype = 'dotted', linewidth = 1, alpha = 0.7) +
  geom_vline(xintercept = as.numeric(sunset_time), color = 'darkblue', linetype = 'dotted', linewidth = 1, alpha = 0.7) +
  coord_radial(start = -0.7 * pi, end = 0.7 * pi, inner.radius = 0.2) +
  geom_text(data = maxima_4_6am[1,], aes(label = format(time_format, "%H:%M"), y = MeanPMN + y_shifts[Type]), family = "serif", size = 3, angle = 320, vjust = 2) +
  geom_text(data = maxima_4_6am[2,], aes(label = format(time_format, "%H:%M"), y = MeanPMN + y_shifts[Type]), family = "serif", size = 3, angle = 320, vjust = 2) +
  geom_text(data = maxima_4_6am[3,], aes(label = format(time_format, "%H:%M"), y = MeanPMN + y_shifts[Type]), family = "serif", size = 3, angle = 320, vjust = 4) +
  geom_text(data = maxima_4_6am[4,], aes(label = format(time_format, "%H:%M"), y = MeanPMN + y_shifts[Type]), family = "serif", size = 3, angle = 320, vjust = 5, hjust = -0.2) +
  geom_text(data = maxima_530_730pm[1,], aes(label = format(time_format, "%H:%M"), y = MeanPMN + y_shifts[Type]), family = "serif", size = 3, angle = 30, vjust= -2.1, hjust = -0.7) +
  geom_text(data = maxima_530_730pm[2,], aes(label = format(time_format, "%H:%M"), y = MeanPMN + y_shifts[Type]), family = "serif", size = 3, angle = 30, vjust= -1.9, hjust = -0.7) +
  geom_text(data = maxima_530_730pm[3,], aes(label = format(time_format, "%H:%M"), y = MeanPMN + y_shifts[Type]), family = "serif", size = 3, angle = 30, vjust= -2, hjust = -0.3) +
  geom_text(data = maxima_530_730pm[4,], aes(label = format(time_format, "%H:%M"), y = MeanPMN + y_shifts[Type]), family = "serif", size = 3, angle = 30, vjust= -1, hjust = -0.2) +
  theme(axis.text.x = element_text(size = 10, color = "black", family = "serif", margin = margin(t = 10)),
        axis.title.x = element_text(size = 12, family = "serif", margin = margin(t = 20)),
        panel.grid.major.x = element_line(colour = "grey", size = 0.1))

