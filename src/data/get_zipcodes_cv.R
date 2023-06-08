library(tigris)     # For obtaining US Census data
library(sf)         # For working with spatial data
library(dplyr)      # For data manipulation

wd = list()
wd$main = 'C:/sarfaraz/cvnitzip'
wd$data = paste0(wd$main,'/data')
wd$output = paste0(wd$data,'/processed/')

# Get zip code shapefile and transform data to same CRS
zipshape = tigris::zctas(state = '06', year = '2010')

# Load central valley shapefile
central_valley <- sf::st_read(paste0(wd$data, "/shapefile/cv.shp"))

# Make sure both shapefiles are in the same CRS
central_valley <- sf::st_transform(central_valley, sf::st_crs(zipshape))

# Create a new column to check whether each zip code is inside Central Valley
zipshape$in_Central_Valley <- ifelse(sf::st_within(zipshape, central_valley), "central_valley", "outside")

# Extract the zip codes that are inside Central Valley
zipshape_inside <- zipshape[zipshape$in_Central_Valley == "central_valley", ]

# Write the data to a csv file
write.csv(as.data.frame(sf::st_set_geometry(zipshape, NULL)), file = paste0(wd$output, "zipcode_central_valley.csv"))

# Export the shapefile of zip codes that are inside Central Valley
sf::st_write(zipshape_inside, paste0(wd$data, "/shapefile/zipcode_inside_central_valley.shp"))
