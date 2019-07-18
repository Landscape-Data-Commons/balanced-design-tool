
######################################
#### 4: Executing the sample draw and writing it out
######################################

## This creates a Spatial Points Data Frame called sample.sites containing the sample draw
## It uses the function made in section 1, which calls spsurvey::grts() to draw the points,
## then renames the points to fit with AIM expectations
set.seed(seed.number)

# Use spsurvey::grts() first to generate the points
sample.sites <- spsurvey::grts(design = design.object,
                               DesignID = project.name,
                               type.frame = "area",
                               src.frame = "sp.object",
                               sp.object = sample.frame,
                               in.shape = NULL,
                               stratum = "STRATUM",
                               shapefile = FALSE)

## Assign projection info to the sample sites from the frame used to create them
sp::proj4string(sample.sites) <- sample.frame@proj4string

## Reproject the sample sites to whatever the output projection is supposed to be
sample.sites <- sp::spTransform(sample.sites, projection)

## Update the X and Y coordinate values
sample.sites$xcoord <- sp::coordinates(sample.sites)[, 1]
sample.sites$ycoord <- sp::coordinates(sample.sites)[, 2]

## Dropping any extra fields and renaming the remaining
fields.relevant <- c("siteID", "stratum", "panel", "wgt", "xcoord", "ycoord")
sample.sites@data <- sample.sites@data[, fields.relevant]
names(sample.sites@data) <- c("PLOTID", "STRATUM", "PANEL", "IntPtWt", "xcoord", "ycoord")

## Set the panel names to "Oversample" instead of "OverSamp"
oversample_panel_names <- paste(sample_sites@data[["STRATUM"]][sample_sites@data[["PANEL"]] == "OverSamp"], "Oversample")
sample_sites@data[["PANEL"]][sample_sites@data[["PANEL"]] == "OverSamp"] <- oversample_panel_names

## Rename the plots with the strata
## Note that this does not attempt to abbreviate
sample.sites@data$PLOTID <- paste0(sample.sites@data$STRATUM,
                                   stringr::str_extract(string = sample.sites@data$PLOTID,
                                                        pattern = "-\\d{1,4}$"))

## This creates a shapefile called "sample_draw.shp" in the filepath in output.filepath.
## If there is already a sample_draw.shp in that filepath, it will be overwritten
rgdal::writeOGR(obj = sample.sites,
                dsn = output.filepath,
                layer = 'sample_draw',
                driver = 'ESRI Shapefile',
                overwrite_layer = TRUE)