# This script is used for selecting necessary columns related to nitrate measurements in wells from a larger dataset.
# The selected data is then written to a new CSV file for further analysis.

# Install and load necessary packages
if (!require(dplyr)) install.packages('dplyr')
if (!require(here)) install.packages('here')
library(dplyr)
library(here)

# Define path to the dataset
dataset_path <- here("data", "raw_data", "Dataset_processed_GAMAlatest.csv")

# Read the dataset into a data frame
df <- read.csv(dataset_path)

df_sel = df %>%
  select(well_id, `APPROXIMATE.LATITUDE`, `APPROXIMATE.LONGITUDE`, mean_nitrate, median_nitrate, 
         max_nitrate, min_nitrate, measurement_count, `mean_concentration_2015.2022`, `mean_concentration_2010.2015`, 
         `mean_concentration_2005.2010`, `mean_concentration_2000.2005`, `mean_concentration_2000.2010`, `mean_concentration_2000.2022`, 
         `mean_concentration_2010.2014`, `mean_concentration_2010.2022`, `mean_concentration_2007.2009`, `mean_concentration_2012.2015`, 
         `mean_concentration_2019.2021`, `mean_concentration_2017.2018`, start_date, end_date, total_obs, well_type, well_data_source)

# Display the first two rows of the selected data frame
print(head(df_sel, 2))

# Define path to the output file
output_path <- here("data", "processed", "nitrate_cv.csv")

# Write the selected data frame to a CSV file
write.csv(df_sel, output_path, row.names = FALSE)
