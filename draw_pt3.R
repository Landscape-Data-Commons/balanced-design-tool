######################################
#### 2: Loading shapefile data
######################################

## This will read in the shapefile called "sample_frame.shp" from the filepath in input.filepath
## The object in R will be a Spatial Polygons Data Frame called sample.frame which contains the geometry and attributes from sample_frame.shp
## sample_frame.shp should have an attribute field called "STRATUM" which will be reflected in sample.frame and used as part of the sample draw.
sample.frame <- rgdal::readOGR(dsn = input.filepath, layer = "sample_frame")

######################################
#### 3: Building the design object
######################################

## This creates a structured list called design.object. It provides the instructions for the function spsurvey::grts()
## to allocate points within the geometry of sample.frame.