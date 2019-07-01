library(tidyverse)
library(spdplyr)
library(tools)
library(shiny)
library(rgdal)
library(spsurvey)

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
  spdf.albers <- sp::spTransform(x = spdf, CRSobj = CRS("+proj=aea"))
  
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
  
  if (any(workingframe[["PER.PANEL.BASE"]]) < 0) {
    stop("One or more strata ended up with a negative number of base points allocated. Check to make sure you aren't asking for too many points.")
  }
  if (any(workingframe[["TOTAL.OVERSAMPLE"]]) < 0) {
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
grts.custom <- function(design.object, ## The output from allocate.panels()
                     design.name, ## The name of the project
                     # sp.object,
                     in.shape,
                     stratum.field = "STRATUM" ## Name of the field in the points data frame that contains the stratum assignments
){
  ## Invoke spsurvey::grts() first
  sample.sites <- spsurvey::grts(design = design.object,
                                 DesignID = design.name,
                                 type.frame = "area",
                                 src.frame = "shapefile",
                                 in.shape = in.shape,
                                 stratum = stratum.field,
                                 shapefile = FALSE
  )
  
  ## Assign projection info to the sample sites SPDF
  proj4string(sample.sites) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  ## Reproject the sample sites to Geographic DD NAD83 and output as a shapefile
  sample.sites <- spTransform(sample.sites, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
  ## update the X and Y coordinate values
  sample.sites$xcoord <- coordinates(sample.sites)[, 1]
  sample.sites$ycoord <- coordinates(sample.sites)[, 2]
  
  ## Dropping the extra fields from the master sample, just keeping the ones specific to the draw plus the master sample ID
  fields.relevant <- c("siteID", "stratum", "panel", "wgt", "xcoord", "ycoord")
  sample.sites@data <- sample.sites@data[, fields.relevant]
  names(sample.sites@data) <- c("PLOTID", "STRATUM", "PANEL", "INTL_PT_WGT", "xcoord", "ycoord")
  
  ## Last step is to rename the oversample panels so that they're broken up into the years instead of being an oversample chunk
  panel.names <- unique(sample.sites@data$PANEL[!(sample.sites@data$PANEL %in% "OverSamp")])
  oversample.df <- dplyr::summarize(dplyr::group_by(sample.sites@data[sample.sites@data$PANEL == "OverSamp",],
                                                    STRATUM),
                                    oversample.pts.per.panel = floor(n()/length(panel.names)),
                                    total.oversample.drawn = n())
  oversample.panels <- list()
  for (s in oversample.df$STRATUM) {
    oversample.count.per.panel <- oversample.df$oversample.pts.per.panel[oversample.df$STRATUM == s]
    oversample.drawn <- oversample.df$total.oversample.drawn[oversample.df$STRATUM == s]
    oversample.panels.current <- c()
    for (n in panel.names) {
      oversample.panels.current <- c(oversample.panels.current,
                                     rep(x = paste("OverSample", n),
                                         times = oversample.count.per.panel))
    }
    ## If there was a difference introduced by rounding, here's the place to put the extra points into the last panel
    if (length(oversample.panels.current) < oversample.drawn) {
      oversample.panels.current <- c(oversample.panels.current,
                                     rep(x = paste("OverSample", last(panel.names)),
                                         times = oversample.drawn - length(oversample.panels.current)))
    }
    oversample.panels[[s]] <- oversample.panels.current
  }
  
  for (s in names(oversample.panels)) {
    sample.sites@data$PANEL[sample.sites@data$PANEL == "OverSamp" & sample.sites@data$STRATUM == s] <- oversample.panels[[s]]
  }
  
  ## Rename the plots with the strata
  sample.sites@data$PLOTID <- paste0(sample.sites@data$STRATUM, stringr::str_extract(sample.sites@data$PLOTID, pattern = "-[0-9]{1,4}$"))
  
  return(sample.sites)
}
