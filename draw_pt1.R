######################################################################################
#### SAMPLE DESIGN SCRIPT
## When run, this script will reproduce the sample points it was packaged with using a
## Generalized Random Tessellation Stratified approach.

## The only user input necessary is to make sure that the following packages are installed
## and to make sure that the values for input.filepath and output.filepath in section 1 are correct.

## If any of these are not installed, remove the hashes and run the relevant line:
# install.packages("dplyr")
# install.packages("rgdal")
# install.packages("spsurvey")
# install.packages("stringr")

#------------------------------------------------------------------------------------#
## The contents are as follows:

#-- Section 1: Design metadata --#
# This section contains archival information about how the the design was configured (which should NOT be edited)
# and the input.filepath and output.filepath values (which MUST be edited).

#-- Section 2: Loading shapefile data --#
# This section uses the input filepath from section 1 and should not need to be edited.

#-- Section 3: Building a design object --#
# This section does not need to be edited.

#-- Section 4: Executing the sample draw and writing it out --#
# This section uses the output filepath from section 1 and should not need to be edited.
#------------------------------------------------------------------------------------#
######################################################################################
######################################
#### 1: Design metadata
######################################
# This is a spatially balanced sample draw using the generalized random tessellation stratified approach as implemented in
# the R package spsurvey::.

# The path to the folder containing the sample frame/stratification polygon shapefile named "sample_frame.shp" is:
input.filepath <- "C:/Local/filepath/to/data"
# The path to the folder to write out the resulting design into is:
output.filepath <- "C:/Local/filepath/to/data"

# The projection that the points should be returned in. NAD83 is the default for all Bureau of Land Management data
projection <- sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")