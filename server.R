library(dplyr)
library(spdplyr)
library(leaflet)
library(viridis)
library(shiny)
library(rgdal)
library(spsurvey)
library(sf)
source('support.functions.R')

# Define server logic
shinyServer(function(input, output, session) {
  # Initialize temp to work within
  temp <- reactiveValues(placeholder = "placeholder",
                         # Save what the base working directory is
                         origdir = getwd(),
                         sessiontempdir = tempdir()
  )
  
  # Allow for wonking big files
  options(shiny.maxRequestSize = 30 * 1024^2)
  # Get a full stacktrace for debugging purposes, although it's unlikely to be ncessary or even helpful
  # options(shiny.fullstacktrace = TRUE)
  
  # When a valid shapefile-containing .zip gets uploaded, update the inputs that are available
  observeEvent(eventExpr = input$uploadzip,
               handlerExpr = {
                 # Display a busy message
                 showNotification(ui = "Please wait while the shapefile is extracted and loaded. This can take a bit with large and complex polygons.",
                                  duration = NULL,
                                  closeButton = FALSE,
                                  id = "busy",
                                  type = "warning")
                 
                 # Get the directory to work within
                 temp$directory <- gsub(input$uploadzip$datapath,
                                        pattern = "/\\d{1,3}$",
                                        replacement = "")
                 temp$polygons <- shape.extract()
                 if (is.null(temp$polygons) | !(class(temp$polygons) %in% c("SpatialPolygonsDataFrame"))) {
                   showNotification(ui = "No single valid polygon shapefile found. Check the uploaded .zip file to make sure it only contains one polygon shapefile",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    id = "polygonserror",
                                    type = "warning")
                   # fieldnames <- "No valid single shapefile found"
                 } else {
                   fieldnames <- names(temp$polygons@data)
                 }
                 updateSelectInput(session,
                                   inputId = "strataname",
                                   choices = unique(c("", fieldnames)),
                                   selected = "")
                 updateSelectInput(session,
                                   inputId = "allocation",
                                   choices = c("", "Proportionally", "Manually", "Equally"),
                                   selected = input$allocation)
                 updateTextInput(session,
                                 inputId = "projname",
                                 value = gsub(gsub(input$uploadzip$name,
                                                   pattern = "\\.(zip)|(ZIP)$",
                                                   replacement = ""),
                                              pattern = "\\W",
                                              replacement = ""))
                 
                 # Remove the busdy notification
                 removeNotification(id = "busy")
               })
  
  # When the user clicks the button after selecting a stratum field
  observeEvent(eventExpr = input$submitstratum,
               handlerExpr = {
                 # Display a busy message
                 showNotification(ui = "Updating stratification information.",
                                  duration = NULL,
                                  closeButton = FALSE,
                                  id = "busy",
                                  type = "warning")
                 
                 if (input$strataname != "") {
                   # Add the relevant values to STRATUM
                   temp$polygons@data$STRATUM <- as.character(temp$polygons@data[[input$strataname]])
                   # And also sanitize them WITHOUT PERMISSION
                   temp$polygons@data$STRATUM <- gsub(temp$polygons@data$STRATUM,
                                                      pattern = "\\W",
                                                      replacement = "")
                   
                   # This bit is shamelessly stolen from another one of my packages
                   # It'll dissolve the polygons by strata if they aren't already
                   unique_ids <- as.character(unique(temp$polygons@data[["STRATUM"]]))
                   if (length(unique_ids) > nrow(temp$polygons@data)) {
                     poly_list <- lapply(X = unique_ids,
                                         polygons = temp$polygons,
                                         dissolve_field = "STRATUM",
                                         FUN = function(X, polygons, dissolve_field){
                                           polygons_current <- polygons[polygons@data[[dissolve_field]] == X, ]
                                           polygons_current <- methods::as(sf::st_combine(sf::st_as_sf(polygons_current)), "Spatial")
                                           df <- data.frame(id = X,
                                                            stringsAsFactors = FALSE)
                                           names(df) <- dissolve_field
                                           rownames(df) <- polygons_current@polygons[[1]]@ID
                                           polygons_current <- sp::SpatialPolygonsDataFrame(Sr = polygons_current,
                                                                                            data = df)
                                           return(polygons_current)
                                         })
                     temp$polygons <- do.call(rbind,
                                              poly_list)
                     
                     temp$polygons <- area.add(temp$polygons,
                                               area.sqkm = FALSE)
                   }
                   
                   
                   # Write this file out to use in spsurvey::grts()
                   # This shouldn't be necessary anymore, but I'm afraid to break things I'll have to fix at this point
                   rgdal::writeOGR(obj = temp$polygons[,"STRATUM"],
                                   dsn = temp$sessiontempdir,
                                   layer = "sample_frame",
                                   driver = "ESRI Shapefile",
                                   overwrite_layer = TRUE)
                   print(list.files(path = temp$sessiontempdir, pattern = "sample_frame"))
                   
                   # Let's make a static map of these!
                   output$strata_map <- renderPlot(expr = {
                     # Convert to an sf object so ggplot can work with it
                     polygons_sf <- as(temp$polygons, "sf")
                     # Make the map as just polygons filled by stratum
                     strata_map <- ggplot(data = polygons_sf) + 
                       geom_sf(aes(fill = STRATUM)) +
                       scale_fill_viridis_d() +
                       theme(panel.background = element_rect(fill = "white",
                                                             color = "gray90"),
                             legend.position = "bottom",
                             panel.grid = element_blank(),
                             axis.title = element_blank(),
                             axis.text = element_blank(),
                             axis.ticks = element_blank()) +
                       # Sometimes the legend would be too wide and get clipped, so we'll force it to be narrower
                       guides(fill = guide_legend(title = NULL,
                                                  ncol = 3))
                     strata_map
                   })
                   
                   
                   
                   
                   
                   # Jump to the map, but only if it won't drag the user away from the allocation tab
                   if (!(input$maintabs == "Point Allocation" & input$allocation != "")) {
                     updateTabsetPanel(session,
                                       inputId = "maintabs",
                                       selected = "Point Allocation") 
                   }
                   
                   # Update the UI
                   # If there are already textInput()s for strata, remove them with the lookup table
                   if (!is.null(temp$ui.lut) & input$allocation == "Manually") {
                     print("attempting to remove ui elements")
                     for (id in 1:nrow(temp$ui.lut)) {
                       print(temp$ui.lut$STRATUM[id])
                       removeUI(selector = paste0("div:has(> #", temp$ui.lut$base[id], ")"), multiple = TRUE, immediate = TRUE)
                       removeUI(selector = paste0("div:has(> #", temp$ui.lut$over[id], ")"), multiple = TRUE, immediate = TRUE)
                     }
                   }
                   
                   # Build a lookup table of strata and their corresponding inputIds for use later
                   temp$ui.lut <-  data.frame(STRATUM = unique(temp$polygons@data$STRATUM))
                   temp$ui.lut$base <- paste0("manualbase", rownames(temp$ui.lut))
                   temp$ui.lut$over <- paste0("manualover", rownames(temp$ui.lut))
                   
                   # Add the inputs for strata using the lookup table. A loop is necessary, unfortunately
                   for (id in 1:nrow(temp$ui.lut)) {
                     print(id)
                     insertUI(
                       selector = '#mabase',
                       ui = numericInput(inputId = temp$ui.lut$base[id],
                                         label = temp$ui.lut$STRATUM[id],
                                         min = 0,
                                         value = 3,
                                         step = 1)
                     )
                     insertUI(
                       selector = '#maover',
                       ui = numericInput(inputId = temp$ui.lut$over[id],
                                         label = temp$ui.lut$STRATUM[id],
                                         min = 0,
                                         value = 3,
                                         step = 1)
                     )
                   }
                 }
                 # Remove the busy notification
                 removeNotification(id = "busy")
               })
  
  # When the user selects a new allocation scheme, as long as it's not blank, jump to the relevant tab
  observeEvent(eventExpr = input$allocation,
               handlerExpr = {
                 if (input$allocation != "") {
                   updateTabsetPanel(session,
                                     inputId = "maintabs",
                                     selected = "Point Allocation")
                 }
                 
                 output$minimumbase <- renderUI({
                   if (input$allocation == "Proportionally") {
                     tagList(
                       numericInput(inputId = "minbase",
                                    label = "Minimum number of base points per stratum per panel:",
                                    min = 0,
                                    step = 1,
                                    value = 3)
                     )
                   }
                 })
                 output$basecount <- renderUI({
                   if (input$allocation == "Proportionally" | input$allocation == "Equally") {
                     tagList(
                       numericInput(inputId = "basecount",
                                    label = "Number of base points per panel:",
                                    step = 1,
                                    value = 50)
                     )
                   } else {
                     NULL
                   }
                 })
                 output$oversamplemin <- renderUI({
                   if (input$allocation == "Proportionally" | input$allocation == "Equally") {
                     tagList(
                       numericInput(inputId = "minoversample",
                                    label = "Minimum number of oversample points per stratum per panel:",
                                    min = 0,
                                    step = 1,
                                    value = 3),
                       numericInput(inputId = "minoversampleproportion",
                                    label = "Minumum proportion of oversample points per stratum per panel:",
                                    min = 0,
                                    max = 1,
                                    step = 0.05,
                                    value = 0.25)
                     )
                   } else {
                     NULL
                   }
                 })
               })
  
  # When the user clicks the button indicating that they're done with their point allocation, generate a design object
  observeEvent(eventExpr = input$allocated,
               handlerExpr = {
                 # Display a busy message
                 showNotification(ui = "Creating design object.",
                                  duration = NULL,
                                  closeButton = FALSE,
                                  id = "busy",
                                  type = "warning")
                 
                 print(input$allocation)
                 # This gets a vector of the individual panel names from the string that the user entered
                 temp$panels <- unique(stringr::str_trim(unlist(stringr::str_split(input$panelnames,
                                                                                   pattern = ","))))
                 message(temp$panels)
                 # Sanitize the panel names
                 temp$panels <- sapply(temp$panels,
                                       gsub,
                                       pattern = "\\W",
                                       replacement = "")
                 
                 if (input$allocation == "Manually") {
                   # Get all the inputs because I can't just slice them out all at once from a reactive list
                   temp$inputs <- c()
                   for (id in c(temp$ui.lut$base, temp$ui.lut$over)) {
                     temp$inputs <- c(temp$inputs, input[[id]])
                   }
                   temp$inputs <- setNames(temp$inputs,
                                           c(temp$ui.lut$base, temp$ui.lut$over))
                   # Build the design object
                   temp$design <- lapply(temp$ui.lut$STRATUM,
                                         ui.lut = temp$ui.lut,
                                         panel.names = temp$panels,
                                         input = temp$inputs,
                                         function(X, ui.lut, panel.names, input){
                                           # How many panels?
                                           panel_count <- length(panel.names)
                                           # Get those counts
                                           base_counts <- rep(input[ui.lut$base[ui.lut$STRATUM == X]],
                                                              times = panel_count)
                                           # Oversample count
                                           over_count <- input[ui.lut$over[ui.lut$STRATUM == X]] * panel_count
                                           
                                           list(panel = setNames(base_counts,
                                                                 panel.names),
                                                seltype = "Equal",
                                                over = over_count)
                                         })
                   
                   # Set the names of that list
                   temp$design <- setNames(temp$design,
                                           temp$ui.lut$STRATUM)
                   
                   output$design <- renderText({
                     paste(temp$design)
                   })
                 } else {
                   if (input$allocation == "Proportionally") {
                     sizes <- dplyr::summarize(dplyr::group_by(temp$polygons@data, STRATUM),
                                               AREA = sum(AREA.HA))
                     basecount <- input$basecount
                     minbase <- input$minbase
                     minoversample <- input$minoversample
                     minoversampleproportion <- input$minoversampleproportion
                   }
                   if (input$allocation == "Equally") {
                     sizes <- data.frame(STRATUM = temp$polygons@data$STRATUM,
                                         AREA = rep(1,
                                                    times = length(unique(temp$polygons@data$STRATUM))))
                     basecount <- input$basecount
                     minbase <- 0
                     minoversample <- input$minoversample
                     minoversampleproportion <- input$minoversampleproportion
                   }
                   temp$design <- allocate.panels(stratum.sizes = sizes,
                                                  panel.number = length(temp$panels),
                                                  panel.names = temp$panels,
                                                  panel.sample.size = basecount,
                                                  points.min = minbase,
                                                  oversample.proportion = minoversampleproportion,
                                                  oversample.min = minoversample)
                   output$design <- renderText({
                     paste(temp$design)
                   })
                   print(temp$design)
                 }
                 # Create a string version of the design object to write out
                 temp$design.string <- paste(paste0("'",
                                                    names(temp$design), "' = ",
                                                    gsub(paste0(as.character(temp$design)),
                                                         pattern = "\\\"",
                                                         replacement = "'")),
                                             collapse = ",")
                 
                 # Add the panel names to temp$design.string because they were lost in the process
                 temp$strata.panels <- unlist(stringr::str_extract_all(string = temp$design.string,
                                                                       pattern = "panel = c[(](\\d|,| ){1,1000}[)]"))
                 for (stratum in temp$strata.panels) {
                   # I'll revisit this to make it prettier. Removing the piping was the priority in the meantime
                   # Given that this works, I'm disinclined to touch it anymore
                   # The point is that it makes a version of the design object that can be pasted into the output sample_script.R and Just Work(TM)
                   temp$design.string <- gsub(temp$design.string,
                                              pattern = gsub(gsub(stratum,
                                                                  pattern = "[(]",
                                                                  replacement = "[(]"),
                                                             pattern = "[)]",
                                                             replacement = "[)]"),
                                              replacement = paste0("panel = c(",
                                                                   paste(gsub(paste0("'",
                                                                                     temp$panels,
                                                                                     "'=",
                                                                                     stringr::str_extract_all(string = stratum,
                                                                                                              pattern = "\\d{1,4}")[[1]]),
                                                                              pattern = "\\\"",
                                                                              replacement = ""),
                                                                         collapse = ","),
                                                                   ")"))
                 }
                 # Remove the busy notification
                 removeNotification(id = "busy")
               })
  
  # When the user clicks the fetch button, generate points from the design object
  observeEvent(eventExpr = input$fetch,
               handlerExpr = {
                 # Display a busy message
                 showNotification(ui = "Please wait while the sample points are drawn. This can take a bit with large and complex designs.",
                                  duration = NULL,
                                  closeButton = FALSE,
                                  id = "busy",
                                  type = "warning")
                 
                 if (!is.null(temp$design) & !is.null(temp$polygons)) {
                   # temp$seednum <- sample(1:999999, size = 1)
                   
                   set.seed(input$seednum)
                   
                   # Write out the shapefile of the stratification polygons
                   # This is done when the stratum variable is selected, so this should be redundant??????
                   rgdal::writeOGR(obj = temp$polygons[, "STRATUM"],
                                   dsn = temp$sessiontempdir,
                                   layer = "sample_frame",
                                   driver = "ESRI Shapefile",
                                   overwrite_layer = TRUE)
                   
                   
                   # Construct the script to draw a design with the the current design object
                   # The first step is copying the script that has the initial content
                   # If overwrite = FALSE then this turns into a hot mess with multiple attempts at a design
                   file.copy(from = paste0(temp$origdir, "/draw_pt1.R"),
                             to = paste0(temp$sessiontempdir, "/sample_script.R"),
                             overwrite = TRUE)
                   
                   # This is the metadata section describing the design setup
                   temp$draw_pt2 <- c("# Project Name:",
                                      paste0("project.name <- '",
                                             gsub(input$projname,
                                                  pattern = "\\W",
                                                  replacement = ""),
                                             "'"),
                                      "",
                                      "# Original stratification shapefile name:",
                                      paste0("# ", temp$shapename),
                                      "# Original stratification attribute field name:",
                                      paste0("# ", input$strataname),
                                      "",
                                      "# Design panel names:",
                                      paste0("# ", paste(temp$panels, collapse = ", ")),
                                      "",
                                      "# Point allocation scheme:",
                                      paste0("# ", input$allocation,
                                             if (input$allocation == "Equally") {
                                               " between strata"
                                             } else if (input$allocation == "Proportionally") {
                                               " by stratum areas"
                                             }),
                                      switch(input$allocation,
                                             "Proportionally" = {
                                               c(paste0("# Total number of base points per panel: ", input$basecount),
                                                 paste0("# Minimum number of base points per stratum per panel: ", input$minbase),
                                                 paste0("# Minimum number of oversample points per stratum per panel: ", input$minoversample),
                                                 paste0("# Minimum ratio of oversample:base points per stratum per panel: ", input$minoversampleproportion),
                                                 "")
                                             },
                                             "Equally" = {
                                               c(paste0("# Total number of base points per panel: ", input$basecount),
                                                 paste0("# Minimum number of oversample points per stratum per panel: ", input$minoversample),
                                                 paste0("# Minimum ratio of oversample:base points per stratum per panel: ", input$minoversampleproportion),
                                                 "")
                                             },
                                             "Manually" = { c("# See the design object construction in section 3 for details on point allocation.",
                                                              "")}),
                                      "# Seed number used during sample draw:",
                                      paste0("seed.number <- ", input$seednum),
                                      ""
                   )
                   
                   temp$draw_pt3 <- readLines(paste0(temp$origdir, "/draw_pt3.R"))
                   
                   temp$draw_pt4 <- c("",
                                      paste0("design.object <- list(", temp$design.string,")"))
                   
                   temp$draw_pt5 <- readLines(paste0(temp$origdir, "/draw_pt5.R"))
                   
                   # Append the script components to the copy of sample_script.R
                   cat(c(temp$draw_pt2, temp$draw_pt3, temp$draw_pt4, temp$draw_pt5),
                       file = paste0(temp$sessiontempdir, "/sample_script.R"),
                       sep = "\n",
                       append = TRUE)
                   
                   # Generate the points
                   temp$points <- grts.gen()
                   
                 }
                 
                 # Make the map!
                 output$pointmap <- renderLeaflet(expr = {
                   # Initialize the map
                   map <- leaflet()
                   # Add some basic info
                   map <- addTiles(map = map)
                   # Make a strata palette to use for the map
                   strata_palette <- colorFactor(palette = "viridis",
                                                 levels = sort(unique(temp$polygons@data[["STRATUM"]])))
                   # Add the stratification polygons
                   map <- addPolygons(map = map,
                                      data = sp::spTransform(temp$polygons,
                                                             CRSobj = temp$points@proj4string),
                                      color = ~strata_palette(STRATUM),
                                      stroke = FALSE,
                                      fillOpacity = 0.7)
                   # Add in the generated points
                   map <- addCircleMarkers(map = map,
                                           data = temp$points,
                                           stroke = TRUE,
                                           opacity = 0.9,
                                           color = "white",
                                           weight = 1,
                                           fillColor = "gray20",
                                           fillOpacity = 1,
                                           radius = 3)
                   
                   # Add in a legend for the strata!
                   map <-   addLegend(map = map,
                                      position = "topright",
                                      pal = strata_palette,
                                      values = ~STRATUM,
                                      data = temp$polygons,
                                      title = "Strata",
                                      opacity = 1)
                   
                   map
                 })
                 
                 updateTabsetPanel(session,
                                   inputId = "maintabs",
                                   selected = "Point Map") 
                 
                 # Remove the busy notification
                 removeNotification(id = "busy")
               })
  
  # Extracting the contents of a .zip and returning an SPDF of the contents
  shape.extract <- reactive({
    shapes <- input$uploadzip
    # If there's no input file
    if (is.null(shapes)) {
      return(NULL)
    } 
    # If the input file is not a zip file
    if (!grepl(shapes$name,
               pattern = "\\.zip$",
               ignore.case = TRUE)) {
      return(NULL)
    }
    print("File exists and ends in .zip")
    print("The value in shapes$datapath is:")
    print(dirname(shapes$datapath))
    # Unzip with an OS-specific system call
    switch(Sys.info()[["sysname"]],
           Windows = {
             print("This is Windows.")
             # Set the new working directory to the uploaded file's datapath. Not sure why this is here, but removing it breaks stuff
             setwd(dirname(shapes$datapath))
             # Pass this argument to the OS. It changes directories. When making Windows system calls, you need to invoke "cmd.exe /c" first
             system(paste0("cmd.exe /c cd ", dirname(shapes$datapath)))
             # Pass the extraction argument to the OS. I had to aim it at my 7zip install. If yours is elsewhere, change the filepath to it, but know that those escaped quotation marks are necessary if there are spaces in your folder names. Thanks, Microsoft
             system(paste0("cmd.exe /c \"C:\\Program Files\\7-Zip\\7z\".exe e -aoa ", shapes$datapath))
             setwd(temp$origdir) # Restoring the working directory
             # Diagnostic terminal output to reassure a debugger that it is in fact reset to the original working directory
             print("Resetting working directory to:")
             print(getwd())
           },
           Linux = {
             print("This is Unix. I know this.")
             # Setting the working directory
             setwd(dirname(shapes$datapath))
             # Passing this to the OS
             system(sprintf("cd %s", dirname(shapes$datapath)))
             # Just checking for debugging
             print(getwd())
             # The unzipping argument to pass to the OS
             system(sprintf("unzip -u %s", shapes$datapath))
             # Set the working directory back
             setwd(temp$origdir)
           }
    )
    # Get the shapefile name
    extracted_files <- list.files(dirname(shapes$datapath))
    temp$shapename <- extracted_files[grepl(extracted_files, pattern = "\\.shp$", ignore.case = TRUE)]
    temp$shapename <- gsub(temp$shapename,
                           pattern = "\\.shp$",
                           replacement = "")
    
    # Are there all the necessary components of a shapefile?
    shapefile_components <- extracted_files[grepl(extracted_files,
                                                  pattern = paste0("^", temp$shapename, "\\.(dbf|DBF|prj|PRJ|shp|SHP|shx|SHX)$"),
                                                  ignore.case = TRUE)]
    
    # If there wasn't a shapefile or there was more than one, return NULL
    if (length(temp$shapename) != 1) {
      return(NULL)
    }
    # Does
    if (length(shapefile_components) != 4) {
      return(NULL)
    }
    
    # Read in the FIRST shapefile and add areas. Too bad if they included more than one!
    polygons <- rgdal::readOGR(dsn = dirname(shapes$datapath),
                               layer = temp$shapename[1])
    polygons <- area.add(polygons,
                         area.sqkm = FALSE)
    
    return(polygons)
  })
  
  # Just listening for if something that should update the map changes
  listen.map <- reactive({
    list(input$strataname, input$updatemap, temp$points)
  })
  
  # Update the points table
  observeEvent(eventExpr = temp$points,
               handlerExpr = {
                 output$pointdata <- renderTable(temp$points@data)
               })
  
  # This invokes grts.custom() and both returns and writes out the results
  grts.gen <- reactive({
    # DRAW SOME POINTS
    points <- grts.custom(design_object = temp$design,
                          design_name = gsub(input$projname,
                                             pattern = "\\W",
                                             replacement = ""),
                          sp_object = temp$polygons,
                          seed_number = input$seednum
    )
    
    # I have no idea why this save() call is here
    # save(points, file = "sample_draw")
    rgdal::writeOGR(obj = points,
                    dsn = temp$sessiontempdir,
                    layer = "sample_draw",
                    driver = "ESRI Shapefile",
                    overwrite_layer = TRUE)
    
    if (!any(grepl(x = list.files(temp$sessiontempdir), pattern = "sample_draw.shp"))) {
      stop("No shapefile called 'sample_draw' exists in the directory.")
    }
    
    # Create a .zip fle in case user wants the points, which depends on a system call
    setwd(temp$sessiontempdir)
    files_to_zip <- list.files(pattern = "^(sample_frame|sample_draw|sample_script)\\.(dbf|prj|shp|shx|r)$",
                               ignore.case = TRUE)
    
    switch(Sys.info()[["sysname"]],
           Windows = {
             system(paste0("cmd.exe /c \"C:\\Program Files\\7-Zip\\7z\".exe a -tzip results.zip ",
                           paste(files_to_zip,
                                 collapse = " ")))
           },
           Linux = {
             system(paste("zip results %s",
                          paste(files_to_zip,
                                collapse = " ")))
           })
    if (!any(grepl(x = list.files(temp$sessiontempdir), pattern = "^results\\.(zip)|(ZIP)"))) {
      stop("No valid .zip file called 'results' exists in the directory.")
    }
    temp$downloadready <- TRUE
    setwd(temp$origdir)
    return(points)
  })
  
  # Download handler for the .zip file created by grts.gen()
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(gsub(input$projname,
                  pattern = "\\W",
                  replacement = ""),
             "_results_",
             format(Sys.Date(), "%Y%m%d"),
             ".zip")
    },
    content = function(file) {
      file.copy(paste0(temp$sessiontempdir, "/results.zip"), file)
    })
})



