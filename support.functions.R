# library(tidyverse)
# library(spdplyr)
# library(tools)
# library(shiny)
# library(rgdal)
# library(spsurvey)

#' Add areas to a Spatial Polygons Data Frame
#'
#' This function takes a Spatial Polygons Data Frame and calculates and adds area fields to the data frame. Areas can be calculated either treating the whole SPDF as a unit or for each polygon individually.
#' @param spdf Spatial Polygons Data Frame to calculate areas for
#' @param area.ha Logical. If \code{TRUE}, areas will be calculated and added in hectares. Default is \code{TRUE}.
#' @param area.sqkm Logical. If \code{TRUE}, areas will be calculated and added in square kilometers. Default is \code{TRUE}.
#' @param byid Logical. If \code{TRUE}, areas will be calculated and added for each polygon by ID. If \code{FALSE} the area of the whole SPDF will be calculated and added, so every value for that field will be the same, regardless of polygon ID. Default is \code{TRUE}.
#' @return The original Spatial Polygons Data Frame with an additional field for each area unit calculated.
#' @keywords area
#' @examples
#' area.add()
#' @export

area.add <- function(spdf,
                     area.ha = TRUE,
                     area.sqkm = TRUE,
                     byid = TRUE
){
  ## Make sure the SPDF is in Albers equal area projection
  spdf.albers <- sp::spTransform(x = spdf,
                                 CRSobj = CRS("+proj=aea"))
  
  ## Add the area in hectares, stripping the IDs from gArea() output
  spdf@data$AREA.HA <- unname(rgeos::gArea(spdf.albers, byid = byid) * 0.0001)
  ## Add the area in square kilometers, converting from hectares
  spdf@data$AREA.SQKM <- spdf@data$AREA.HA * 0.01
  
  if (!(area.ha)) {
    spdf@data$AREA.HA <- NULL
  }
  if (!(area.sqkm)) {
    spdf@data$AREA.SQKM <- NULL
  }
  return(spdf)
}

## This will construct the design object necessary to feed to spsurvey::grts()
# stratum.sizes Data frame with variables STRATUM and AREA
# panel.number Number of panels to allocate points for. This is partially/mostly a QA value
# panel.names A character vector of unique panel names. This needs to have a exactly as many names as there are panels specified in panel.number
# panel.sample.size = 50 The number of base points per panel. If all panels are the same, this can be a single number, otherwise it needs to be a vector of numbers, one for each panel
# points.min = 3 The minimum number of points that will be allocated to a stratum within a panel
# oversample.proportion Add oversample points per stratum per panel equal to [base point count] * oversample.proportion
# oversample.min The minimum number of points per stratum per panel. Used if this value is greater than the calculated proportional allocation
allocate.panels <- function(stratum.sizes,
                            panel.number = 5,
                            panel.names = c("Year1", "Year2", "Year3", "Year4", "Year5"),
                            panel.sample.size = 50,
                            points.min = 3,
                            oversample.proportion = 0.25,
                            oversample.min = 3
){
  # I cannibalized the following code from a package and so this next line bridges the gap between
  # legacy stuff in this tool and the code I grabbed
  workingframe <- stratum.sizes
  
  # After the minimum points are allocated, how many remain to be allocated?
  remainder <- panel.sample.size - nrow(workingframe) * points.min
  # How many panels are there?
  panel.count <- length(panel.names)
  
  ## Create all the support values then the list that goes into the design object for each stratum
  workingframe[["PROPORTION"]] <- workingframe[["AREA"]] / sum(workingframe[["AREA"]])
  workingframe[["PER.PANEL.BASE"]] <- round(workingframe[["PROPORTION"]] * remainder) + points.min
  workingframe[["PER.PANEL.OVERSAMPLE"]] <- ceiling(pmax(workingframe[["PER.PANEL.BASE"]] * oversample.proportion, oversample.min))
  workingframe[["TOTAL.OVERSAMPLE"]] <- workingframe[["PER.PANEL.OVERSAMPLE"]] * panel.count
  
  if (any(workingframe[["PER.PANEL.BASE"]] < 0)) {
    stop("One or more strata ended up with a negative number of base points allocated. Check to make sure you aren't asking for too many points.")
  }
  if (any(workingframe[["TOTAL.OVERSAMPLE"]] < 0)) {
    stop("One or more strata ended up with a negative number of oversample points allocated. Check to make sure you aren't asking for too many points.")
  }
  
  ## Create the output design object list.
  output <- lapply(split(workingframe, workingframe[["STRATUM"]]),
                   panel.names = panel.names,
                   panel.count = panel.count,
                   function(X, panel.names, panel.count) {
                     # Just for clarity because X isn't obvious
                     df <- X
                     # Make the list. It's made of a named vector of panel sizes in base point count
                     list(panel = unlist(stats::setNames(rep(df[1, "PER.PANEL.BASE"],
                                                             times = panel.count),
                                                         panel.names)),
                          # The selection type (always equal here)
                          seltype = "Equal",
                          # And total oversample points
                          over = unname(unlist(df[1, "TOTAL.OVERSAMPLE"])))
                   })
  
  # The list needs to be named by stratum
  output <- stats::setNames(output,
                            workingframe[["STRATUM"]])
  
  return(output)
}

## Run the spsurvey::grts() function in the way we always plan on for AIM. Returns the points comprising the sample design
#' Draw spatially balanced points with \code{spsurvey::grts()}, output in AIM format
#'
#' @description A wrapper for \code{spsurvey::grts()} that modifies the output SPDF to match the format expected by other parts of the AIM workflow, including distribution to field crews and populating design databases.
#' @param design_object A list of lists structured and named according to the requirements of \code{spsurvey::grts()}. Can be automatically constructed with \code{allocate.panels()}.
#' @param design_name A character string of the name to use for the design, often the name of the project.
#' @param source_frame Character string. To be passed to \code{spsurvey::grts()} as the argument \code{src.frame}; see that function's documentation for further details. Defaults to \code{"sp.object"}.
#' @param sp_object If using an SPDF instead of a shapefile, an SPDF with a variable name matching \code{stratum_field} which contains values matching the strata names in \code{design_object}. Defaults to \code{NULL}.
#' @param in_shape If using a shapefile instead of an SPDF, a string representing the filepath to a shapefile (without file extension) to pass to \code{spsurvey::grts()} as its argument \code{in_shape}. Defaults to \code{NULL}.
#' @param stratum_field A character string representing the name of the variable in either \code{sp_object} or \code{in_shape} containing the strata names matching those in \code{design_object}. Defaults to \code{"STRATUM"}.
#' @param seed_number An optional numeric value to be passed to \code{set.seed()} to make the output reproducible. Defaults to \code{NULL}.
#' @return A Spatial Points Data Frame of the sampling locations with the fields \code{PLOTID}, \code{STRATUM}, \code{PANEL}, \code{IntPtWt} (initial point weight), \code{xcoord}, and \code{ycoord}
#' @export
grts.custom <- function(design_object,
                        design_name = "Design name",
                        source_frame = "sp.object",
                        sp_object = NULL,
                        in_shape = NULL,
                        stratum_field = "STRATUM",
                        seed_number = NULL
){
  if (!is.null(seed_number)) {
    set.seed(seed_number)
  }
  if (!is.null(sp_object)) {
    source_frame <- "sp.object"
    if (!(stratum_field %in% names(sp_object))) {
      stop("The variable stratum_field was not found in sp_object. Check case and spelling.")
    }
  } else if (!is.null(in_shape)) {
    source_frame <- "shapefile"
    in_shape <- gsub(in_shape, pattern = "\\.(shp)|(shp)$", replacement = "")
  } else {
    stop("Provide either an SPDF as sp_object or a filepath to a shapefile as in_shape.")
  }
  if (source_frame == "sp.object" & is.null(sp_object)) {
    stop("Please provide an SPDF as sp_object.")
  }
  if (source_frame == "shapefile" & is.null(in_shape)) {
    stop("Please provide a filepath to a shapefile as in_shape.")
  }
  
  # Set type.frame value depending on type of spdf
  type_frame <- switch(class(sp_object),
                       "SpatialPolygonsDataFrame" = {"area"},
                       "SpatialPointsDataFrame" = {"finite"})
  
  ## Invoke spsurvey::grts() first
  sample_sites <- spsurvey::grts(design = design_object,
                                 DesignID = design_name,
                                 type.frame = type_frame,
                                 src.frame = source_frame,
                                 sp.object = sp_object,
                                 in.shape = in_shape,
                                 stratum = stratum_field,
                                 shapefile = FALSE)
  
  ## Assign projection info to the sample sites SPDF
  if (!is.null(sp_object)) {
    sp::proj4string(sample_sites) <- sp_object@proj4string
  } else {
    sp::proj4string(sample_sites) <- rgdal::readOGR(dsn = gsub(in_shape,
                                                               pattern = "/([A-z]|[0-9])+$",
                                                               replacement = ""),
                                                    layer = gsub(in_shape,
                                                                 pattern = "/([A-z]|[0-9])+$"))@proj4string
  }
  ## Reproject the sample sites to Geographic DD NAD83
  sample_sites <- sp::spTransform(sample_sites, sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
  ## update the X and Y coordinate values
  sample_sites[["xcoord"]] <- sp::coordinates(sample_sites)[, 1]
  sample_sites[["ycoord"]] <- sp::coordinates(sample_sites)[, 2]
  
  ## Dropping the extra fields and renaming the remaining fields
  fields_relevant <- c("siteID", "stratum", "panel", "wgt", "xcoord", "ycoord")
  sample_sites@data <- sample_sites@data[, fields_relevant]
  names(sample_sites@data) <- c("PLOTID", "STRATUM", "PANEL", "IntPtWt", "xcoord", "ycoord")
  
  # Change "OverSamp" to "OverSample + [STRATUM]"
  oversample_panel_names <- paste(sample_sites@data[["STRATUM"]][sample_sites@data[["PANEL"]] == "OverSamp"], "Oversample")
  sample_sites@data[["PANEL"]][sample_sites@data[["PANEL"]] == "OverSamp"] <- oversample_panel_names
  
  ## Rename the plots with the strata
  sample_sites@data[["PLOTID"]] <- paste0(sample_sites@data[["STRATUM"]], stringr::str_extract(string = sample_sites@data[["PLOTID"]], pattern = "-\\d{1,4}$"))
  
  return(sample_sites)
}
