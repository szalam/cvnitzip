# This script determines the mean nitrate for each zipcodes. First determines
# the mean considering all well data, then estimates mean for each well types

# Clear workspace
rm(list = ls())

# Load necessary libraries
library(tidyverse)  # For data manipulation and plotting
library(sf)         # For working with spatial data
library(tigris)     # For obtaining US Census data

# Set working directory and read in data
setwd('C:/sarfaraz/cvnitzip/data')
df = read.csv('processed/nitrate_cv.csv')[,-1]

# Remove rows with missing values
df = df[complete.cases(df$mean_nitrate), ]

# Rename columns
colnames(df)[colnames(df) == "APPROXIMATE.LATITUDE"] = "latitude"
colnames(df)[colnames(df) == "APPROXIMATE.LONGITUDE"] = "longitude"

# Convert data to an sf object
data_sf = st_as_sf(df, coords = c('longitude','latitude'), crs = 4326)

# Get zip code shapefile and transform data to the same CRS
zipshape = tigris::zctas(state = '06', year = '2010')

# Export spatial data frame to shapefile format
st_write(zipshape, "processed/shapefile/CA_zips.shp")

# Convert zipshape to a data frame for ggplot
zipshape_df = st_set_geometry(zipshape, NULL)

# Create a data frame for the points
points_df = as.data.frame(df)

# Transform data_sf to the same CRS as zipshape
data_sf_transformed = st_transform(data_sf, crs = st_crs(zipshape))

# Spatially join the points to the zipshape polygons
joined_data = st_join(zipshape, data_sf_transformed)

# Convert the joined_data to a data frame
joined_data_df = as.data.frame(joined_data)

# Group the data by ZCTA5CE10 and calculate the mean of mean_nitrate and well count for each group
mean_nitrate_and_count_by_zip_allwell = joined_data_df %>%
  group_by(ZCTA5CE10) %>%
  summarize(mean_nitrate = mean(mean_nitrate, na.rm = TRUE),
            well_count = n())

mean_nitrate_and_count_by_zip_well_type = joined_data_df %>%
  group_by(ZCTA5CE10, well_type) %>%
  summarize(mean_nitrate = mean(mean_nitrate, na.rm = TRUE),
            well_count = n())

# Merge zipshape_df and mean_nitrate_and_count_by_zip based on ZCTA5CE10
merged_data_allwell = left_join(zipshape, mean_nitrate_and_count_by_zip_allwell, by = "ZCTA5CE10")
merged_data_well_type = left_join(zipshape, mean_nitrate_and_count_by_zip_well_type, by = "ZCTA5CE10")

merged_data_allwell_df = data.frame(merged_data_allwell)
merged_data_allwell_df = merged_data_allwell_df[c('ZCTA5CE10','mean_nitrate','well_count')]

merged_data_well_type_df = data.frame(merged_data_well_type)
merged_data_well_type_df = merged_data_well_type_df[c('ZCTA5CE10','mean_nitrate','well_type','well_count')]

# Exporting data
write.csv(merged_data_allwell_df, 'processed/mean_nitrate_for_zips_allwellscombined.csv')
write.csv(merged_data_well_type_df, 'processed/mean_nitrate_for_zips_different_welltypes.csv')

# Plot the merged data
p = ggplot() +
  geom_sf(data = merged_data_allwell, aes(fill = mean_nitrate), color = NA) +
  geom_sf(data = merged_data_allwell, aes(fill = mean_nitrate), color = 'black', size = 0.01, linetype = "dashed") +
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
ggsave(filename = "visual/mean_nitrate_map_zipcode.png", plot = p, width = 10, height = 12, dpi = 300)

# Export spatial data frame to shapefile format
st_write(merged_data_well_type, "processed/shapefile/MeanNitrate_for_zipcodes_wt_well_type.shp")
st_write(merged_data_allwell, "processed/shapefile/MeanNitrate_for_zipcodes.shp")
