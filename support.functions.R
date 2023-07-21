# library(tidyverse)
# library(spdplyr)
# library(tools)
# library(shiny)
# library(rgdal)
# library(spsurvey)

repair_geometry <- function(polygons,
                            verbose = FALSE) {
  if (!("sf" %in% class(polygons))) {
    stop("polygons must be an sf object")
  }
  
  validity_check <- sf::st_is_valid(polygons_sf)
  
  if (any(is.na(validity_check))) {
    stop("The geometry of the polygons is corrupt. Unable to repair.")
  }
  
  if (!all(validity_check)) {
    if (verbose) {
      message("Invalid geometry found. Attempting to repair.")
    }
    output <- sf::st_buffer(x = polygons_sf,
                            dist = 0)
  } else {
    if (verbose) {
      message("No invalid geometry found.")
    }
    output <- polygons_sf
  }
  
  return(output)
}


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

area.add <- function(sf,
                     area.ha = TRUE,
                     area.sqkm = TRUE,
                     byid = TRUE
){
  ## Make sure the SPDF is in Albers equal area projection
  sf_albers <- sf::st_transform(x = sf,
                                crs = sf::st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  
  ## Add the area in hectares, converted from square meters
  sf$AREA.HA <- as.vector(sf::st_area(x = sf_albers)) * 0.0001
  ## Add the area in square kilometers, converting from hectares
  sf$AREA.SQKM <- sf$AREA.HA * 0.01
  
  if (!(area.ha)) {
    sf$AREA.HA <- NULL
  }
  if (!(area.sqkm)) {
    sf$AREA.SQKM <- NULL
  }
  return(sf)
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
                        sample_frame = NULL,
                        stratum_field = "STRATUM",
                        seed_number = NULL
){
  if (!is.null(seed_number)) {
    set.seed(seed_number)
  }
  if (!is.null(sample_frame)) {
    if (!(stratum_field %in% names(sample_frame))) {
      stop("The variable stratum_field was not found in sample_frame. Check case and spelling.")
    }
  } else {
    stop("Provide an sf object as sample_frame.")
  }
  
  # So, design objects were the old way of feeding info into GRTS
  # This grabs the relevant information to feed to modern grts()
  base_counts <- sapply(X = design_object,
                        FUN = function(X){
                          sum(X[["panel"]])
                        })
  
  over_counts <- sapply(X = design_object,
                        FUN = function(X){
                          sum(X[["over"]])
                        })
  
  current_projection <- sf::st_crs(sample_frame)
  
  sample_frame <- sf::st_transform(sample_frame,
                                   crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  
  grts_output <- spsurvey::grts(sframe = sample_frame,
                                n_base = base_counts,
                                stratum_var = stratum_field,
                                seltype = "equal",
                                n_over = over_counts)
  
  points_list <- lapply(X = names(base_counts),
                        design_object = design_object,
                        grts_output = grts_output,
                        FUN = function(X, design_object, grts_output){
                          # just for convenience and clarity
                          current_stratum <- X
                          
                          # Grab the base points
                          current_base_points <- grts_output$sites_base[grts_output$sites_base$stratum == current_stratum, ]
                          # grts() no longer supports panels, but people still want them, so we'll grab the info
                          # from the design object and make a vector
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
                          
                          current_points <- rbind(current_base_points,
                                                  current_over_points)
                          
                          vars_to_keep <- c("siteID", "stratum", "siteuse", "wgt", "lon_WGS84", "lat_WGS84")
                          
                          current_points <- current_points[, vars_to_keep]
                          
                          names(current_points)[names(current_points) == "siteID"] <- "PLOTID"
                          names(current_points)[names(current_points) == "stratum"] <- "STRATUM"
                          names(current_points)[names(current_points) == "siteuse"] <- "PANEL"
                          names(current_points)[names(current_points) == "wgt"] <- "IntPtWt"
                          names(current_points)[names(current_points) == "lon_WGS84"] <- "xcoord"
                          names(current_points)[names(current_points) == "lat_WGS84"] <- "ycoord"
                          
                          current_points$PLOTID <- gsub(x = current_points$PLOTID,
                                                        pattern = "Site",
                                                        replacement = current_stratum)
                          
                          current_points
                        })
  
  sample_sites <- do.call(rbind,
                          points_list)
  
  message(paste0("sample_sites class is: ",
                 paste(class(sample_sites),
                       collapse = ", ")))
  
  ## Rename the plots with the strata
  sample_sites[["PLOTID"]] <- paste0(sample_sites[["STRATUM"]],
                                     stringr::str_extract(string = sample_sites[["PLOTID"]],
                                                          pattern = "-\\d{1,4}$"))
  
  return(sample_sites)
}
