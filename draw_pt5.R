
######################################
#### 4: Executing the sample draw and writing it out
######################################
## Since this script was originally written, the function spsurvey::grts() has been
## overhauled. Parts of this script (like the following chunk) are bridging the
## old code and the new code.

## This converts the old style design object into the appropriate inputs for spsurvey::grts()
## The design object will be used again later for redefining panels
base_counts <- sapply(X = design.object,
                      FUN = function(X){
                        sum(X[["panel"]])
                      })

over_counts <- sapply(X = design.object,
                      FUN = function(X){
                        sum(X[["over"]])
                      })


## This creates a Spatial Points Data Frame called sample.sites containing the sample draw
## It uses the function made in section 1, which calls spsurvey::grts() to draw the points,
## then renames the points to fit with AIM expectations
set.seed(seed.number)

## Use spsurvey::grts() first to generate the points
grts_output <- spsurvey::grts(sframe = sample.frame,
                              n_base = base_counts,
                              stratum_var = "STRATUM",
                              seltype = "equal",
                              n_over = over_counts)

## For each stratum, we'll do some point cleanup and combination
points_list <- lapply(X = names(base_counts),
                      design_object = design.object,
                      grts_output = grts_output,
                      FUN = function(X, design_object, grts_output){
                        # just for convenience and clarity
                        current_stratum <- X
                        
                        # Grab the base points
                        current_base_points <- grts_output$sites_base[grts_output$sites_base$stratum == current_stratum, ]
                        # grts() no longer supports panels out of the box,
                        # so we'll grab the info from the design object and make a vector
                        panel_info <- design_object[[current_stratum]]$panel
                        panels <- as.vector(sapply(X = names(panel_info),
                                                   panel_info = panel_info,
                                                   FUN = function(X, panel_info){
                                                     rep(x = X,
                                                         times = panel_info[X])
                                                   }))
                        current_base_points$siteuse <- panels
                        
                        current_over_points <- grts_output$sites_over[grts_output$sites_over$stratum == current_stratum, ]
                        current_over_points$siteuse <- "Oversample"
                        
                        # Combine the base and oversample points into a single sf object
                        current_points <- rbind(current_base_points,
                                                current_over_points)
                        
                        # Trim that sf object down to just the relevant variables
                        vars_to_keep <- c("siteID", "stratum", "siteuse", "wgt", "lon_WGS84", "lat_WGS84")
                        
                        current_points <- current_points[, vars_to_keep]
                        
                        # Rename the variables to be in keeping with AIM terminology
                        names(current_points)[names(current_points) == "siteID"] <- "PLOTID"
                        names(current_points)[names(current_points) == "stratum"] <- "STRATUM"
                        names(current_points)[names(current_points) == "siteuse"] <- "PANEL"
                        names(current_points)[names(current_points) == "wgt"] <- "IntPtWt"
                        
                        # Change plot IDs to use the strata names
                        current_points$PLOTID <- gsub(x = current_points$PLOTID,
                                                      pattern = "Site",
                                                      replacement = current_stratum)
                        
                        current_points
                      })

## Combine all the sf objects into a single sf object
sample.sites <- do.call(rbind,
                        points_list)


## This creates a shapefile called "sample_draw.shp" in the filepath in output.filepath.
## If there is already a sample_draw.shp in that filepath, it will be overwritten
sf::st_write(obj = sample.sites,
             dsn = output.filepath,
             layer = 'sample_draw',
             driver = 'ESRI Shapefile',
             append = FALSE)