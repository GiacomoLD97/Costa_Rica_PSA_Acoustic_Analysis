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
  coord_polar(theta = "x", start = pi/2) +
  # geom_vline(color = 'grey50', xintercept = as.POSIXct("2023-12-13 00:00:00") + minutes(315), linetype = 'dashed', linewidth = 0.5) +
  # geom_vline(color = 'grey50', xintercept = as.POSIXct("2023-12-13 00:00:00") + minutes(1070), linetype = 'dashed', linewidth = 0.5) +
  theme(axis.text.x = element_text(size = 10, color = "black"),
        panel.grid.major.x = element_line(colour = "grey", size = 0.1),
  ) 
  
