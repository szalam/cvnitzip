library(tigris)     # For obtaining US Census data
library(sf)         # For working with spatial data
library(dplyr)      # For data manipulation

wd = list()
wd$main = 'C:/sarfaraz/cvnitzip'
wd$data = paste0(wd$main,'/data')
wd$output = paste0(wd$data,'/processed/')


# Get zip code shapefile and transform data to same CRS
zipshape = tigris::zctas(state = '06', year = '2010')


