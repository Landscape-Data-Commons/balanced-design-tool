
######################################
#### 2: Loading packages and creating a function
######################################

#### This section loads the necessary packages and makes a function called grts.aim()
## If any of these are not installed, remove the hashes and run the relevant line:
# install.packages("dplyr")
# install.packages("rgdal")
# install.packages("spsurvey")
library(dplyr) # Contains %>% and data frame manipulation functions
library(rgdal) # Contains functions to read and write in shapefiles
library(spsurvey) # Contains the grts() function

## This is a wrapper for the function spsurvey::grts() that also renames the points according to AIM expectations
grts.aim <- function(design.object,
                     design.name = "Design name",
                     src.frame = "sp.object",
                     sp.object = NULL,
                     in.shape = NULL,
                     stratum.field = "STRATUM",
                     seed.number = NULL
){
  if (!is.null(seed.number)) {
    set.seed(seed.number)
  }
  if (!is.null(sp.object)) {
    src.frame <- "sp.object"
    if (!(stratum.field %in% names(sp.object))) {
      stop("The variable stratum.field was not found in sp.object. Check case and spelling.")
    }
  } else if (!is.null(in.shape)) {
    src.frame <- "shapefile"
    in.shape <- stringr::str_replace(string = in.shape, pattern = "\\.(shp)|(shp)$", replacement = "")
  } else {
    stop("Provide either an SPDF as sp.object or a filepath to a shapefile as in.shape.")
  }
  if (src.frame == "sp.object" & is.null(sp.object)) {
    stop("Please provide an SPDF as sp.object.")
  }
  if (src.frame == "shapefile" & is.null(in.shape)) {
    stop("Please provide a filepath to a shapefile as in.shape.")
  }
  
  ## Invoke spsurvey::grts() first
  sample.sites <- spsurvey::grts(design = design.object,
                                 DesignID = design.name,
                                 type.frame = "area",
                                 src.frame = src.frame,
                                 sp.object = sp.object,
                                 in.shape = in.shape,
                                 stratum = stratum.field,
                                 shapefile = FALSE
  )
  
  ## Assign projection info to the sample sites SPDF
  if (is.null(sp.object)) {
    sp::proj4string(sample.sites) <- sp.object@proj4string
  } else {
    sp::proj4string(sample.sites) <- rgdal::readOGR(dsn = stringr::str_replace(in.shape, pattern = "/([a-Z]|[0-9]){1,256}$", replacement = ""),
                                                    layer = stringr::str_extract(in.shape, pattern = "/([a-Z]|[0-9]){1,256}$")) %>% .@proj4string
    # proj4string(sample.sites) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  }
  ## Reproject the sample sites to Geographic DD NAD83
  sample.sites <- sp::spTransform(sample.sites, sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
  ## update the X and Y coordinate values
  sample.sites$xcoord <- sp::coordinates(sample.sites)[, 1]
  sample.sites$ycoord <- sp::coordinates(sample.sites)[, 2]
  
  ## Dropping the extra fields from the master sample, just keeping the ones specific to the draw plus the master sample ID
  fields.relevant <- c("siteID", "stratum", "panel", "wgt", "xcoord", "ycoord")
  sample.sites@data <- sample.sites@data[, fields.relevant]
  names(sample.sites@data) <- c("PLOTID", "STRATUM", "PANEL", "IntPtWt", "xcoord", "ycoord")
  
  ## Last step is to rename the oversample panels so that they're broken up into the years instead of being an oversample chunk
  panel.names <- unique(sample.sites@data$PANEL[!(sample.sites@data$PANEL %in% "OverSamp")])
  oversample.df <- sample.sites@data[sample.sites@data$PANEL == "OverSamp",] %>% dplyr::group_by(STRATUM) %>% dplyr::summarize(oversample.pts.per.panel = floor(n()/length(panel.names)),
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
  sample.sites@data$PLOTID <- paste0(sample.sites@data$STRATUM, stringr::str_extract(string = sample.sites@data$PLOTID, pattern = "-[0-9]{1,4}$"))
  
  return(sample.sites)
}

######################################
#### 3: Loading shapefile data
######################################

## This will read in the shapefile called "sample_frame.shp" from the filepath in input.filepath
## The object in R will be a Spatial Polygons Data Frame called sample.frame which contains the geometry and attributes from sample_frame.shp
## sample_frame.shp should have an attribute field called "STRATUM" which will be reflected in sample.frame and used as part of the sample draw.
sample.frame <- rgdal::readOGR(dsn = input.filepath, layer = "sample_frame")

######################################
#### 4: Building the design object
######################################

## This creates a structured list called design.object. It provides the instructions for the function spsurvey::grts()
## to allocate points within the geometry of sample.frame.