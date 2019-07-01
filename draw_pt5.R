
######################################
#### 5: Executing the sample draw and writing it out
######################################

## This creates a Spatial Points Data Frame called sample.sites containing the sample draw
## It uses the function made in section 1, which calls spsurvey::grts() to draw the points,
## then renames the points to fit with AIM expectations
sample.sites <- grts.custom(design.object = design.object,
                            design.name = project.name,
                            sp.object = sample.frame,
                            stratum.field = "STRATUM",
                            seed.number = seed.number
)

## This creates a shapefile called "sample_draw.shp" in the filepath in output.filepath.
## If there is already a sample_draw.shp in that filepath, it will be overwritten
rgdal::writeOGR(obj = sample.sites,
                dsn = output.filpath,
                layer = 'sample_draw',
                driver = 'ESRI Shapefile',
                overwrite_layer = TRUE)