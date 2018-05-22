######################################################################################
#### SAMPLE DESIGN SCRIPT
## When run, this script will reproduce the sample points it was packaged with using a
## Generalized Random Tessellation Stratified approach.

## The only user input necessary is to make sure that the packages in section 2 are installed
## and to make sure that the values for input.filepath and output.filepath in section 1 are correct.

#------------------------------------------------------------------------------------#
## The contents are as follows:

#-- Section 1: Design metadata --#
# This section contains archival information about how the the design was configured (which should NOT be edited)
# and the input.filepath and output.filepath values (which MUST be edited).

#-- Section 2: Loading packages and creating a function --#
# This section only needs to be edited if one of the three packages is not installed already.

#-- Section 3: Loading shapefile data --#
# This section uses the input filepath from section 2 and should not need to be edited.

#-- Section 4: Building a design object --#
# The section needs to be edited to reflect the input and output filepaths.

#-- Section 5: Executing the sample draw and writing it out --#
# This section uses the output filepath from section 2 and should not need to be edited.
#------------------------------------------------------------------------------------#
######################################################################################
######################################
#### 0: Design metadata
######################################
# This is a spatially balanced sample draw using the generalized random tessellation stratified approach as implemented in
# the R package spsurvey::.

