
###### Physical Distances Between Sites ######

rm(list = ls())

###### 1. Load required packages ######
library(geosphere)
library(pheatmap)
library(ggplot2)
library(reshape2)
library(dplyr)

###### 2. Load and process the data  ######
sites <- read.csv("~/Documents/GitHub/Costa_Rica_PSA_Acoustic_Analysis/data/sites_type_table.csv")
sites <- sites %>% mutate(Type = ifelse(Type == "Reference_Forest", "Reference Forest", ifelse(Type == "Natural_Regeneration", "Natural Regeneration", Type)))
sites <- sites %>% arrange(factor(Type, levels = c("Reference Forest", "Natural Regeneration", "Plantation", "Pasture")))

# Create distance matrix
coords <- sites[, c("Longitude", "Latitude")]
dist_matrix <- distm(coords, fun = distHaversine)
dist_matrix <- dist_matrix / 1000
rownames(dist_matrix) <- sites$Site
colnames(dist_matrix) <- sites$Site

dist_long <- melt(dist_matrix)

# Too hard to interpret, convert distances to categories
dist_long <- dist_long %>%
  mutate(category = case_when(
    value < 1 ~ "Less than one km",
    value < 20 ~ "Between 1-20 km",
    TRUE ~ "Over 20 km"
  ))

# Reorder
dist_long$category <- factor(dist_long$category, levels = c("Less than one km", "Between 1-20 km", "Over 20 km"))

# Colors for distance categories 
category_colors <- c("Less than one km" = "#f4f1de", "Between 1-20 km" = "#e07a5f", "Over 20 km" = "#3d405b")

# Use colors from rest of analysis for Land Use types
type_colors <- c("Reference Forest" = "#228833", "Natural Regeneration" = "#4477AA", "Plantation" = "#EE6677", "Pasture" = "#CCBB44")

axis_label_colors <- type_colors[sites$Type]


###### 3. Heatmap ######

# Plot the heatmap using ggplot2
heatmap_plot <- ggplot(dist_long, aes(Var1, Var2)) +
  geom_tile(aes(fill = category)) +
  scale_fill_manual(values = category_colors) +
  labs(title = "Distance between Sites (km)", x = "Site", y = "Site", fill = "Distance (km)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45, hjust = 0.5, vjust = 0.5, size = 4, color = axis_label_colors,
      margin = margin(t = 0,b = 0,)  # Increase spacing around x-axis labels
    ),
    axis.text.y = element_text(
      size = 4, color = axis_label_colors,
      margin = margin(r = 0, l = 0)  # Increase spacing around y-axis labels
    )
  )


# Legend for land use type

dummy_data <- data.frame(
  Type = names(type_colors),
  Color = type_colors
)

heatmap_plot <- heatmap_plot +
  geom_point(data = dummy_data, aes(x = 1, y = 1, color = Type), alpha = 0) +
  scale_color_manual(values = type_colors, name = "Land-Use Type") +  # Change legend title here
  guides(color = guide_legend(override.aes = list(alpha = 1)))

#Print the graph

print(heatmap_plot)

###### 4. Histogram of Distances ######

# Take only unique pairs
dist_matrix[lower.tri(dist_matrix)] <- NA 
diag(dist_matrix) <- NA 
dist_long_filtered <- melt(dist_matrix)
dist_long_filtered <- dist_long_filtered %>% filter(!is.na(value))


# Define uneven breaks for the histogram
breaks <- c(0, 0.5, 1, seq(5, max(dist_long_filtered$value), by = 5))

# Plot the histogram 
ggplot(dist_long_filtered, aes(x = value)) +
  geom_histogram(breaks = breaks, fill = "#e07a5f", color = "black") +
  scale_x_continuous(breaks = seq(0, max(dist_long_filtered$value), by = 5)) +
  scale_y_continuous(breaks = seq(0, 650, by = 50)) +
  labs(title = "Histogram of Distances Between Sites", x = "Distance (km)", y = "Number of Site Pairs") +
  theme_minimal()


