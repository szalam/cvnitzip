# Clear workspace
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(sf)
library(tigris)

# Set working directory and read in data
setwd('C:/sarfaraz/cvnitzip/data')

# Read the data files
df <- read.csv('processed/mean_nitrate_for_zips_different_welltypes.csv')
select_zips <- read.csv('raw/gastroenteritis_zipcode_2018.csv')

# Filter rows with well_type = DOMESTIC
df <- df[df$well_type == "DOMESTIC", ]

# Filter select_zips dataframe to get zipcodes
selected_zipcodes <- select_zips$ZIPCODE

# Create a new dataframe with matching zipcodes
merged_filtered_data <- subset(df, ZCTA5CE10 %in% selected_zipcodes)

# Remove rows with missing mean_nitrate values
merged_filtered_data <- merged_filtered_data[complete.cases(merged_filtered_data$mean_nitrate), ]

# Write csv containing mean domestic well nitrate at selected zipcodes
write.csv(merged_filtered_data, 'processed/mean_nitrate_for_gastroenteritis_zips_domesticwells.csv')

# Display the dimensions of the merged_filtered_data dataframe
dim(merged_filtered_data)

# Read zipcode shapefile
shapefile <- st_read("processed/shapefile/CA_zips.shp")

# Merge shapefile with merged_filtered_data based on ZCTA5CE10
merged_shape_data <- merge(shapefile, merged_filtered_data, by = "ZCTA5CE10", all.x = TRUE)

# Create the spatial plot
p <- ggplot() +
  geom_sf(data = merged_shape_data, aes(fill = mean_nitrate), color = NA) +
  geom_sf(data = merged_shape_data, aes(fill = mean_nitrate), color = 'black', size = 0.01, linetype = "dashed") +
  coord_sf() +
  labs(title = "Mean Nitrate by Zip Code") +
  scale_fill_gradient2(low = "darkgreen", mid = "yellow", high = "red",
                       midpoint = 5, name = "Mean Nitrate",
                       na.value = "white", limits = c(0, 10),
                       guide = guide_colorbar(ticks.colour = "black", ticks.linewidth = 0.5)) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),  # Remove axis text
    axis.ticks = element_blank(), # Remove axis ticks
    axis.title = element_blank(), # Remove axis titles
    plot.background = element_rect(fill = "white"), # Set plot background to white
    panel.grid = element_blank(), # Remove grid lines
    plot.title.position = "plot"  # Center the plot title
  ) +
  theme(plot.title = element_text(hjust = 0.5)) # Center alignment

# Save the ggplot image to a file (e.g., PNG format)
ggsave(filename = "visual/mean_nitrate_map_gastroenteritis_zipcode.png", plot = p, width = 10, height = 12, dpi = 300)
