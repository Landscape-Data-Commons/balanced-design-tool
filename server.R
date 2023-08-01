library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(viridis)
library(shiny)
library(spsurvey)
library(sf)
library(zip)
library(shinyjs)
source('support.functions.R')

# Define server logic
shinyServer(function(input, output, session) {
  
  #### Initialize workspace to work within #######################################
  workspace <- reactiveValues(placeholder = "placeholder",
                              downloadready = FALSE,
                              # Save what the base working directory is
                              origdir = getwd(),
                              sessiontempdir = tempdir(),
                              projection = sf::st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  )
  
  #### Allow for wonking big files ###############################################
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  #### Conditional UI elements #################################################
  ##### Design tab contents ####################################################
  output$design_tab_notyet_ui <- renderUI(expr = if (req(!workspace$downloadready)) {
    fluidRow(column(width = 10,
                    p(class = "data-prompt",
                      "You have not yet drawn a design. Your design will appear here once you have finished configuring your polygons and point allocation in the",
                      a("Configuration tab.",
                        onclick = "tabJump('Configuration')"))))
  })
  
  output$design_tab_pointmap_ui <- renderUI(expr = if (req(workspace$downloadready)) {
    tagList(
      leaflet::leafletOutput(outputId = "pointmap",
                             height = "80vh"),
      helpText("Map of selected sample points")
    )
  })
  
  output$design_tab_summarytable_ui <- renderUI(expr = if (req(workspace$downloadready)) {
    tagList(
      br(),
      helpText("Summary of design as drawn"),
      DT::DTOutput(outputId = "design_summary")
    )
  })
  
  ##### When the user selects a new allocation scheme, update the UI ###########
  observeEvent(eventExpr = input$allocation,
               handlerExpr = {
                 # NO DOWNLOADING RESULTS!!!! A change in allocation invalidates any existing output
                 workspace$downloadready <- FALSE
                 
                 if (input$allocation != "") {
                   updateTabsetPanel(session,
                                     inputId = "maintabs",
                                     selected = "Point Allocation")
                 }
                 
                 output$minimumbase <- renderUI({
                   if (input$allocation == "Proportionally") {
                     fluidRow(column(width = 10,
                                     numericInput(inputId = "minbase",
                                                  label = "Minimum number of base points per stratum per panel",
                                                  min = 0,
                                                  step = 1,
                                                  value = 3)),
                              column(width = 1,
                                     actionButton(inputId = "minbase_info",
                                                  label = "",
                                                  class = "info-btn",
                                                  icon = icon("circle-question"))))
                     
                   }
                 })
                 output$basecount <- renderUI({
                   if (input$allocation == "Proportionally" | input$allocation == "Equally") {
                     fluidRow(column(width = 10,
                                     numericInput(inputId = "basecount",
                                                  label = "Total number of base points per panel",
                                                  step = 1,
                                                  value = 50)
                     ),
                     column(width = 1,
                            actionButton(inputId = "basecount_info",
                                         label = "",
                                         class = "info-btn",
                                         icon = icon("circle-question"))))
                   } else {
                     NULL
                   }
                 })
                 output$oversamplemin <- renderUI({
                   if (input$allocation == "Proportionally" | input$allocation == "Equally") {
                     tagList(
                       fluidRow(column(width = 10,
                                       numericInput(inputId = "minoversample",
                                                    label = "Minimum number of oversample points per stratum per panel",
                                                    min = 0,
                                                    step = 1,
                                                    value = 3)),
                                column(width = 1,
                                       actionButton(inputId = "minoversample_info",
                                                    label = "",
                                                    class = "info-btn",
                                                    icon = icon("circle-question")))),
                       fluidRow(column(width = 10,
                                       numericInput(inputId = "minoversampleproportion",
                                                    label = "Minumum proportion of oversample points per stratum per panel",
                                                    min = 0,
                                                    max = 1,
                                                    step = 0.05,
                                                    value = 0.25)),
                                column(width = 1,
                                       actionButton(inputId = "minoversampleproportion_info",
                                                    label = "",
                                                    class = "info-btn",
                                                    icon = icon("circle-question"))))
                     )
                   } else {
                     NULL
                   }
                 })
               })
  
  ##### Download button ########################################################
  output$download_button_ui <- renderUI(expr = if(req(workspace$downloadready)) {
    br()
    downloadButton(outputId = "download_button",
                   # class = "download-button",
                   label = "Download Points as shapefile")
  })
  
  ##### Building the links to other tabs! ######################################
  # Note that we have to do this with a() and an onclick argument that calls the
  # function defined way up at the top of all this. The a() is necessary because
  # we can't nest another layer of quotes in a string, being limited to "" and ''
  output$polygons_done_ui <- renderUI(if (input$strataname != "") {
    tagList(br(),
            fluidRow(column(width = 10,
                            p(class = "next-step-message",
                              "Your polygons are ready! The next step is to check the configuration in the",
                              a("Point Allocation tab.",
                                onclick = "tabJump('Point Allocation')")))))
  })
  
  output$configuration_done_ui <- renderUI(if (req(!is.null(workspace$points))) {
    tagList(br(),
            fluidRow(column(width = 10,
                            p(class = "next-step-message",
                              "Your design is complete! Check it out and download the points in the",
                              a("Design tab.",
                                onclick = "tabJump('Design')")))))
  })
  
  output$no_polygons_yet_ui <- renderUI(if (req(is.null(workspace$polygons))) {
    tagList(br(),
            fluidRow(column(width = 10,
                            p(class = "next-step-message",
                              "You still need to upload polygons. Please do so in the",
                              a("Polygon Setup tab.",
                                onclick = "tabJump('Polygon Setup')")))))
  })
  
  #### Help info ###############################################################
  observeEvent(eventExpr = input$polygons_info,
               handlerExpr = {
                 message("Displaying info about polygon inputs")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "The uploaded file must be a ZIP file containing either all the files making up a polygon shapefile (i.e., polygons.shp, polygons.shx, polygons.dbf, and polygons.prj) or a geodatabase containing at least one polygon feature class.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$projname_info,
               handlerExpr = {
                 message("Displaying info about projname")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "The name to give to the output file containing the final sampling design.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$polygons_layer_info,
               handlerExpr = {
                 message("Displaying info about polygon layers")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "If your uploaded polygons include more than one feature class (e.g., two shapefiles or a geodatabase with multiple feature classes), then you must select which to use as your sample frame.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$strataname_info,
               handlerExpr = {
                 message("Displaying info about strata variable")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "The name of the column/variable in the polygon attributes which contains the stratum identities to use for allocating points.",
                                            br(),
                                            br(),
                                            "If you do not wish to stratify, select 'Do not stratify'.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$allocation_info,
               handlerExpr = {
                 message("Displaying info about allocation")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "There are two ways to allocate points to strata.",
                                            br(),
                                            br(),
                                            "Proportional allocation will divide the points between your strata according to the relative area of each stratum, e.g., a stratum that makes up 40% of the design area will receive 40% of the points.",
                                            br(),
                                            br(),
                                            "Manual allocation allows you to specify exactly how many base and oversample points you want in each stratum.",
                                            br(),
                                            br(),
                                            "If you are not stratifying, you can use either approach. The tool will treat your sample frame as a single stratum.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$seednum_info,
               handlerExpr = {
                 message("Displaying info about seed number")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "The seed number is used to set the state of the computer's random number generation. The same inputs with the same seed number will produce the same unique design.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$panelnames_info,
               handlerExpr = {
                 message("Displaying info about panel names")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "Optionally, you may want to break your design into separate panels. This is not necessary, but some users find it helpful to designate expected sampling year, e.g. Year 1, Year 2, Year 3.",
                                            br(),
                                            br(),
                                            "Panels are guides, not rules. A GRTS design is intended to be sampled through in order, skipping only rejected sampling locations. If using panels, remember that the previous panel must be totally evaluated before moving on to the next.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$basecount_info,
               handlerExpr = {
                 message("Displaying info about total base point count per panel")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "The total number of base points to draw per panel in the sampling design.",
                                            br(),
                                            br(),
                                            "If you are not using panels, this is the total number of base points to draw.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$minbase_info,
               handlerExpr = {
                 message("Displaying info about minimum base points per stratum per panel")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "The minimum number of base points to draw per stratum per panel when allocating points in the sampling design. Note that setting this too high will result in requiring more base points than specified as the total number of base points in the design.",
                                            br(),
                                            br(),
                                            "If you are not using panels, this is the minimum number of base points to draw in a stratum.",
                                            br(),
                                            br(),
                                            "If not using strata, this is for the whole sample frame.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$minoversample_info,
               handlerExpr = {
                 message("Displaying info about minimum oversample points per stratum")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "The minimum number of oversample points (i.e. 'spares' to make up for rejected base points) to draw per stratum when allocating points in the sampling design.",
                                            br(),
                                            br(),
                                            "Note that this is for the whole design and not per panel.",
                                            br(),
                                            br(),
                                            "If not using strata, this is for the whole sample frame.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$minoversampleproportion_info,
               handlerExpr = {
                 message("Displaying info about minimum oversample proportion per stratum")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "The expected rejection rate for base points used to draw oversample points (i.e. 'spares' to make up for rejected base points) per stratum when allocating points in the sampling design.",
                                            br(),
                                            br(),
                                            "Once the total base point count for a stratum across all panels is determined, it is multiplied by this value and rounded up to determine the number of oversample points to draw for the stratum. This count will be used only if it exceeds the minimum oversample point count.",
                                            br(),
                                            br(),
                                            "Note that this is for the whole design and not per panel.",
                                            br(),
                                            br(),
                                            "If not using strata, this is for the whole sample frame.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$manual_allocation_info,
               handlerExpr = {
                 message("Displaying info about")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "The number of base points and oversample points (i.e. 'spares' to make up for rejected base points) to draw in each stratum per panel.",
                                            br(),
                                            br(),
                                            "This is the total number of base and oversample points to draw if you are not using panels.",
                                            br(),
                                            br(),
                                            "If not using strata, this is for the whole sample frame.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  #### When the reset button is pressed ########################################
  observeEvent(eventExpr = req(input$reset_button),
               handlerExpr = {
                 message("Reset button pressed!")
                 shinyjs::refresh()
                 message("Executed shinyjs::refresh()")
               })
  
  
  #### When a an upload happens, deal with it ##################################
  observeEvent(eventExpr = input$polygons,
               handlerExpr = {
                 # NO DOWNLOADING RESULTS!!!! An upload invalidates any existing output
                 workspace$downloadready <- FALSE
                 
                 message("Polygons file uploaded")
                 # Because it looks like I can't enforce filetype in the upload
                 # selection dialogue, check it here
                 # I'm assuming that a file extension can be 1-5 characters long
                 # although the longest I've seen is 4, I think
                 polygon_upload_extension <- toupper(stringr::str_extract(string = input$polygons$datapath,
                                                                          pattern = "(?<=\\.).{1,5}$"))
                 polygons_are_zip <- polygon_upload_extension == "ZIP"
                 
                 if (!polygons_are_zip) {
                   showNotification(ui = "Polygons must be uploaded as a zipped shapefile or zipped geodatabase.",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    id = "polygons_zip_error",
                                    type = "error")
                 } else {
                   message("Attempting to unzip file")
                   message(paste0("input$polygons$datapath is ",
                                  input$polygons$datapath))
                   utils::unzip(zipfile = input$polygons$datapath,
                                overwrite = TRUE,
                                exdir = dirname(input$polygons$datapath))
                   message("File unzipped")
                   
                   # Get the shapefile name
                   extracted_files <- list.files(dirname(input$polygons$datapath),
                                                 full.names = TRUE,
                                                 recursive = TRUE)
                   
                   # Look for extracted shapefiles
                   shp_indices <- grepl(extracted_files,
                                        pattern = "\\.shp$",
                                        ignore.case = TRUE)
                   message(paste0("Found ",
                                  sum(shp_indices),
                                  " shapefiles"))
                   upload_has_shp <- any(shp_indices)
                   
                   # Look for extracted geodatabases
                   # Now because this is recursive, we're turning up the files
                   # inside the GDBs, so we need to get just the GDB path
                   gdb_indices <- grepl(extracted_files,
                                        pattern = "\\.gdb",
                                        ignore.case = TRUE)
                   gdb_paths <- unique(stringr::str_extract(string = extracted_files[gdb_indices],
                                                            pattern = ".*(?=\\.gdb/)"))
                   gdb_paths <- paste0(gdb_paths,
                                       ".gdb")
                   gdb_paths <- gdb_paths[!(gdb_paths %in% c(".gdb"))]
                   
                   message(paste0("Found ",
                                  length(gdb_paths),
                                  " geodatabases"))
                   upload_has_gdb <- length(gdb_paths) > 0
                   
                   # Prioritize geodatabases
                   if (upload_has_gdb) {
                     workspace$polygon_filetype <- "gdb"
                     message("Working from extracted geodatabase")
                     # If there's more than one geodatabase, just use the first
                     # but warn the user
                     if (length(gdb_paths) > 1) {
                       message("Multiple GDBs detected. Using 'first' one")
                       showNotification(ui = "More than one geodatabase found in ZIP file. Please upload one at a time.",
                                        duration = NULL,
                                        closeButton = TRUE,
                                        type = "warning",
                                        id = "multiple_gdb_warning")
                     }
                     current_gdb_path <- gdb_paths[1]
                     
                     # So I can reference this when reading in layers later
                     workspace$gdb_filepath <- current_gdb_path
                     # Find which layers are available
                     available_polygons <- sf::st_layers(dsn = current_gdb_path)$name
                     message(paste0("Available layers in GDB are: ",
                                    paste(available_polygons,
                                          collapse = ", ")))
                     message("Updating selectInput(inputId = 'polygons_layer')")
                     # Update the selection options
                     updateSelectInput(session = session,
                                       inputId = "polygons_layer",
                                       choices = available_polygons,
                                       selected = available_polygons[1])
                   } else if (upload_has_shp) {
                     workspace$polygon_filetype <- "shp"
                     message("Working with extracted shapefile(s)")
                     # Which files end in .shp?
                     available_polygons <- extracted_files[shp_indices]
                     
                     message(paste0("Before checking for all files, the available shapefiles are: ",
                                    paste(basename(available_polygons),
                                          collapse = ", ")))
                     
                     # And which of those .shp files has associated
                     # .prj, .dbf, and .shx files?
                     has_all_files <- sapply(X = available_polygons,
                                             files = extracted_files,
                                             FUN = function(X, files) {
                                               required_filetypes <- c("dbf",
                                                                       "prj",
                                                                       "shp",
                                                                       "shx")
                                               file_pattern <- gsub(x = X,
                                                                    pattern = "shp$",
                                                                    replacement = "",
                                                                    ignore.case = TRUE)
                                               all(sapply(X = required_filetypes,
                                                          files = files,
                                                          file_pattern = file_pattern,
                                                          FUN = function(X, files, file_pattern) {
                                                            paste0(file_pattern,
                                                                   X) %in% files
                                                          }))
                                             })
                     # So then which shapefiles are really valid?
                     available_polygons <- available_polygons[has_all_files]
                     message(paste0("After checking for all files, the available shapefiles are: ",
                                    paste(basename(available_polygons),
                                          collapse = ", ")))
                     
                     # Update the selection options
                     # This makes a named vector of the shp filepaths
                     # so it's easy to read them in later but the options are
                     # human readable in the GUI
                     polygon_shp_options <- available_polygons
                     shp_filenames <- gsub(x = basename(available_polygons),
                                           pattern = "\\.shp$",
                                           replacement = "",
                                           ignore.case = TRUE)
                     names(available_polygons) <- shp_filenames
                     updateSelectInput(session = session,
                                       inputId = "polygons_layer",
                                       choices = available_polygons,
                                       selected = available_polygons[1])
                   } else {
                     showNotification(ui = "Uploaded file does not appear to contain either a valid shapefile or geodatabase.",
                                      duration = NULL,
                                      closeButton = TRUE,
                                      type = "error",
                                      id = "empty_upload_error")
                   }
                 }
               })
  
  #### When input$polygons_layer updates #######################################
  observeEvent(eventExpr = {req(input$polygons_layer != "")},
               handlerExpr = {
                 message("input$polygons_layer has updated")
                 
                 message("Nullifying manual point allocation UI elements")
                 # Update the UI
                 # If there are already textInput()s for strata, remove them with the lookup table
                 if (!is.null(workspace$ui.lut)) {
                   message("Attempting to remove ui elements")
                   for (id in 1:nrow(workspace$ui.lut)) {
                     message(paste0("Removing ", workspace$ui.lut$STRATUM[id]))
                     removeUI(selector = paste0("div:has(> #", workspace$ui.lut$base[id], ")"), multiple = TRUE, immediate = TRUE)
                     removeUI(selector = paste0("div:has(> #", workspace$ui.lut$over[id], ")"), multiple = TRUE, immediate = TRUE)
                   }
                 }
                 
                 if (input$polygons_layer != "") {
                   message("Reading in polygons")
                   if (workspace$polygon_filetype == "gdb") {
                     polygons <- sf::st_read(dsn = workspace$gdb_filepath,
                                             layer = input$polygons_layer)
                   } else if (workspace$polygon_filetype == "shp") {
                     polygons <- sf::st_read(dsn = input$polygons_layer)
                   }
                   
                   message("Making sure that the polygons don't have a Z dimension")
                   polygons <- sf::st_zm(polygons)
                   
                   message("Making sure the polygons are in NAD83")
                   polygons <- sf::st_transform(polygons,
                                                crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                   
                   # If we're supposed to repair
                   if (input$repair_polygons) {
                     polygons_sf <- sf::st_as_sf(polygons)
                     
                     
                     validity_check <- sf::st_is_valid(polygons_sf)
                     
                     if (any(is.na(validity_check))) {
                       showNotification(ui = "The geometry of the polygons is corrupt. Unable to repair. Please correct shapefile geometry and reupload.",
                                        duration = NULL,
                                        closeButton = TRUE,
                                        id = "corruption",
                                        type = "error")
                     } else if (!all(validity_check)) {
                       showNotification(ui = "Invalid geometry found. Attempting to repair. If this takes more than five minutes, you may want to attempt to repair the geometry yourself by buffering with a distance of 0 and reupload.",
                                        duration = NULL,
                                        closeButton = FALSE,
                                        id = "invalid",
                                        type = "warning")
                       polygons_repaired <- sf::st_buffer(x = polygons_sf,
                                                          dist = 0)
                       validity_check <- sf::st_is_valid(polygons_repaired)
                       
                       removeNotification(id = "invalid")
                       if (any(is.na(validity_check))) {
                         showNotification(ui = "The repair attempt failed and some geometry of the polygons is now corrupt. Please correct shapefile geometry and reupload.",
                                          duration = NULL,
                                          closeButton = TRUE,
                                          id = "repair_corrupt",
                                          type = "error")
                       } else if (!all(na.omit(validity_check))) {
                         showNotification(ui = "The repair attempt failed and some geometry of the polygons is still invalid. Please correct shapefile geometry and reupload.",
                                          duration = NULL,
                                          closeButton = TRUE,
                                          id = "repair_invalid",
                                          type = "error")
                       } else {
                         # showNotification(ui = "The geometry of the polygons was successfully repaired.",
                         #                  duration = NULL,
                         #                  closeButton = TRUE,
                         #                  id = "repair_success",
                         #                  type = "message")
                       }
                       
                       polygons <- polygons_repaired
                     } else {
                       # showNotification(ui = "The geometry of the polygons is valid and does not require repair.",
                       #                  duration = NULL,
                       #                  closeButton = TRUE,
                       #                  id = "valid",
                       #                  type = "message")
                     }
                   }
                   
                   fieldnames <- names(polygons)
                   
                   message("Adding area to polygons")
                   workspace$polygons <- area.add(sf = polygons)
                   
                   message(paste0("workspace$polygons class is: ",
                                  paste(class(workspace$polygons),
                                        collapse = ", ")))
                   
                   updateSelectInput(session,
                                     inputId = "strataname",
                                     choices = unique(c("Do not stratify", fieldnames)),
                                     selected = "Do not stratify")
                   updateSelectInput(session,
                                     inputId = "allocation",
                                     choices = c("", "Proportionally", "Manually"),
                                     selected = input$allocation)
                   updateTextInput(session,
                                   inputId = "projname",
                                   value = gsub(gsub(input$polygons$name,
                                                     pattern = "\\.(zip)|(ZIP)$",
                                                     replacement = ""),
                                                pattern = "\\W",
                                                replacement = ""))
                 }
               })
  
  
  #### When workspace$polygons updates #########################################
  # Update the map with the polygons so people can see what they're working with
  observeEvent(eventExpr = req(workspace$polygons),
               handlerExpr = {
                 if (req(!is.null(workspace$polygons))) {
                   message("Making the base map!")
                   output$basemap <- renderLeaflet(expr = {
                     # Initialize the map
                     message("Initializing the base map")
                     map <- leaflet()
                     # Add some basic info
                     message("Adding the tiles to the base map")
                     map <- addTiles(map = map)
                     
                     message(paste0("Current input$strataname value is: ",
                                    input$strataname))
                     if (!(input$strataname %in% c("", "Do not stratify"))) {
                       message("Looks like there's a stratum variable selection made. Creating a map with strata polygons.")
                       # Make a strata palette to use for the map
                       strata_palette <- colorFactor(palette = "viridis",
                                                     levels = sort(unique(workspace$polygons[["STRATUM"]])))
                       # Add the stratification polygons
                       map <- addPolygons(map = map,
                                          data = sf::st_transform(x = workspace$polygons,
                                                                  crs = "+proj=longlat +datum=WGS84"),
                                          color = ~strata_palette(STRATUM),
                                          stroke = FALSE,
                                          fillOpacity = 0.7)
                       
                       # Add in a legend for the strata!
                       map <- addLegend(map = map,
                                        position = "topright",
                                        pal = strata_palette,
                                        values = ~STRATUM,
                                        data = workspace$polygons,
                                        title = "Strata",
                                        opacity = 1)
                     } else {
                       message("Looks like there's no stratum variable selection made yet. Adding workspace$polygons with a single fill color.")
                       
                       # Add the stratification polygons
                       map <- addPolygons(map = map,
                                          data = sf::st_transform(x = workspace$polygons,
                                                                  crs = "+proj=longlat +datum=WGS84"),
                                          color = "#fa8943",
                                          stroke = FALSE,
                                          fillOpacity = 0.7)
                     }
                     message("Returning base map")
                     map
                   })
                 }
               })
  
  #### When the selected stratum variable updates ##############################
  observeEvent(eventExpr = req(input$strataname),
               handlerExpr = {
                 message("Stratum variable updated!")
                 # NO DOWNLOADING RESULTS!!!! A change in strata invalidates existing output
                 workspace$downloadready <- FALSE
                 
                 if (input$strataname != "") {
                   if (input$strataname == "Do not stratify") {
                     message("No stratum variable selected. Using sample frame.")
                     workspace$polygons$STRATUM <- "Sample frame"
                   } else {
                     message("Adding stratum values to 'STRATUM' variable")
                     # Add the relevant values to STRATUM
                     workspace$polygons$STRATUM <- as.character(workspace$polygons[[input$strataname]])
                   }
                   
                   # And also sanitize them WITHOUT PERMISSION
                   message("Sanitizing values in polygons$STRATUM")
                   workspace$polygons$STRATUM <- gsub(workspace$polygons$STRATUM,
                                                      pattern = "\\W",
                                                      replacement = "")
                   
                   message(paste0("workspace$polygons class is: ",
                                  paste(class(workspace$polygons),
                                        collapse = ", ")))
                   
                   # Make a list of strata so the user can see them
                   message("Rendering stratification table")
                   output$strata_table <- renderTable({setNames(data.frame("STRATA" = unique(workspace$polygons[["STRATUM"]])),
                                                                paste0("Current strata (n = ", length(unique(workspace$polygons[["STRATUM"]])), ")"))})
                   
                   
                   # Update the UI
                   # If there are already textInput()s for strata, remove them with the lookup table
                   if (!is.null(workspace$ui.lut) & input$allocation == "Manually") {
                     message("Allocation is set to manual and there are textInput()s for strata")
                     message("Attempting to remove ui elements")
                     for (id in 1:nrow(workspace$ui.lut)) {
                       message(paste0("Removing ", workspace$ui.lut$STRATUM[id]))
                       removeUI(selector = paste0("div:has(> #", workspace$ui.lut$base[id], ")"), multiple = TRUE, immediate = TRUE)
                       removeUI(selector = paste0("div:has(> #", workspace$ui.lut$over[id], ")"), multiple = TRUE, immediate = TRUE)
                     }
                   }
                   
                   # Build a lookup table of strata and their corresponding inputIds for use later
                   message("Building a lookup table of strata and inputIds for dynamic interface")
                   workspace$ui.lut <-  data.frame(STRATUM = unique(workspace$polygons$STRATUM))
                   workspace$ui.lut$base <- paste0("manualbase", rownames(workspace$ui.lut))
                   workspace$ui.lut$over <- paste0("manualover", rownames(workspace$ui.lut))
                   
                   # Add the inputs for strata using the lookup table. A loop is necessary, unfortunately
                   for (id in 1:nrow(workspace$ui.lut)) {
                     message(paste0("Now adding UI element ", id))
                     insertUI(
                       selector = '#mabase',
                       ui = numericInput(inputId = workspace$ui.lut$base[id],
                                         label = workspace$ui.lut$STRATUM[id],
                                         min = 0,
                                         value = 3,
                                         step = 1)
                     )
                     insertUI(
                       selector = '#maover',
                       ui = numericInput(inputId = workspace$ui.lut$over[id],
                                         label = workspace$ui.lut$STRATUM[id],
                                         min = 0,
                                         value = 3,
                                         step = 1)
                     )
                   }
                 }
               })
  
  #### When the fetch button is pressed ########################################
  observeEvent(eventExpr = input$fetch,
               handlerExpr = {
                 workspace$design <- NULL
                 
                 # First let's make sure that things are ready for designing
                 proportional_inputs_ready <- !any(sapply(X = c(input$basecount,
                                                                input$minbase,
                                                                input$minoversample,
                                                                input$minoversampleproportion),
                                                          is.na))
                 
                 message(paste0("The proportional parameters currently are: ",
                                paste0(sapply(X = c(input$basecount,
                                                    input$minbase,
                                                    input$minoversample,
                                                    input$minoversampleproportion),
                                              FUN = function(X){
                                                paste(X,
                                                      collapse = ", ")
                                              }),
                                       collapse = "; ")))
                 
                 message(paste0("The checks for proportional parameters resolve to: ",
                                paste0(sapply(X = c(input$basecount,
                                                    input$minbase,
                                                    input$minoversample,
                                                    input$minoversampleproportion),
                                              is.na),
                                       collapse = ", ")))
                 
                 # First up, let's make the design object
                 # OKAY! So sometimes people might accidentally ask for more point according to minbase * stratum count than they allow for in basecount
                 # This gives them an error if that happens instead of crashing the tool.
                 if (input$allocation == "Proportionally") {
                   message("Allocation will be proportional; checking to make sure the point counts requested work")
                   if (proportional_inputs_ready) {
                     if ((length(unique(workspace$polygons[["STRATUM"]])) * input$minbase) > input$basecount) {
                       pointcounts_valid <- FALSE
                       showNotification(ui = paste0("Your target number of base points is ", input$basecount,
                                                    " but with ", length(unique(workspace$polygons[["STRATUM"]])),
                                                    " strata and a minimum of ", input$minbase,
                                                    " base points per stratum you'd need ", length(unique(workspace$polygons[["STRATUM"]])) * input$minbase,
                                                    " base points. Increase your total base point count, decrease the minimum point count per stratum, or decrease the number of strata."),
                                        duration = NULL,
                                        closeButton = TRUE,
                                        id = "bad_pointcount",
                                        type = "error")
                     } else {
                       pointcounts_valid <- TRUE
                     }
                   } else {
                     showNotification(ui = paste0("Please make sure that you've specified values for all the design parameters."),
                                      duration = NULL,
                                      closeButton = TRUE,
                                      id = "missing_point_allocation_proportional",
                                      type = "error")
                     pointcounts_valid <- FALSE
                   }
                   
                 } else {
                   pointcounts_valid <- TRUE
                 }
                 if (pointcounts_valid) {
                   message(paste0("input$allocation is ",
                                  input$allocation))
                   
                   message("Dealing with panels")
                   if (input$panelnames == "") {
                     message("No panels provided. Drawing everything in one panel")
                     workspace$panels <- "1"
                   } else {
                     message("Panels requested. Wrangling them now")
                     # This gets a vector of the individual panel names from the string that the user entered
                     workspace$panels <- unique(stringr::str_trim(unlist(stringr::str_split(input$panelnames,
                                                                                            pattern = ","))))
                     
                     # Sanitize the panel names
                     workspace$panels <- sapply(workspace$panels,
                                                gsub,
                                                pattern = "\\W",
                                                replacement = "")
                     
                     message(paste0("The requested panel names are: "),
                             paste(workspace$panels,
                                   collapse = ", "))
                   }
                   
                   if (input$allocation == "Manually") {
                     # Get all the inputs because I can't just slice them out all at once from a reactive list
                     message("Getting all the relevant strata input values set up")
                     workspace$inputs <- c()
                     for (id in c(workspace$ui.lut$base, workspace$ui.lut$over)) {
                       workspace$inputs <- c(workspace$inputs, input[[id]])
                     }
                     workspace$inputs <- setNames(workspace$inputs,
                                                  c(workspace$ui.lut$base, workspace$ui.lut$over))
                     
                     manual_inputs_ready <- !any(sapply(X = workspace$inputs,
                                                        is.na))
                     
                     if (!manual_inputs_ready) {
                       showNotification(ui = paste0("Please make sure that you've specified base and oversample point counts for all strata"),
                                        duration = NULL,
                                        closeButton = TRUE,
                                        id = "missing_point_allocation",
                                        type = "error")
                       design_inputs_ready <- FALSE
                     } else {
                       design_inputs_ready <- TRUE
                     }
                     
                     if (design_inputs_ready) {
                       message("Building design object")
                       # Build the design object
                       workspace$design <- lapply(workspace$ui.lut$STRATUM,
                                                  ui.lut = workspace$ui.lut,
                                                  panel.names = workspace$panels,
                                                  input = workspace$inputs,
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
                       workspace$design <- setNames(workspace$design,
                                                    workspace$ui.lut$STRATUM)
                       
                       output$design <- renderText({
                         paste(workspace$design)
                       })
                     } else {
                       workspace$design <- NULL
                     }
                   } else {
                     message("Getting the sizes of the strata")
                     sizes <- dplyr::summarize(dplyr::group_by(sf::st_drop_geometry(workspace$polygons), STRATUM),
                                               AREA = sum(AREA.HA))
                     basecount <- input$basecount
                     minbase <- input$minbase
                     minoversample <- input$minoversample
                     minoversampleproportion <- input$minoversampleproportion
                     
                     design_inputs_ready <- !any(sapply(X = c(basecount,
                                                              minbase,
                                                              minoversample,
                                                              minoversampleproportion),
                                                        is.na))
                     
                     if (!design_inputs_ready) {
                       showNotification(ui = paste0("Please make sure that you've specified values for all the design parameters."),
                                        duration = NULL,
                                        closeButton = TRUE,
                                        id = "missing_point_allocation_proportional",
                                        type = "error")
                       workspace$design <- NULL
                     } else {
                       message("Generating design")
                       workspace$design <- allocate.panels(stratum.sizes = sizes,
                                                           panel.number = length(workspace$panels),
                                                           panel.names = workspace$panels,
                                                           panel.sample.size = basecount,
                                                           points.min = minbase,
                                                           oversample.proportion = minoversampleproportion,
                                                           oversample.min = minoversample)
                       output$design <- renderText({
                         paste(workspace$design)
                       })
                       message(workspace$design)
                     }
                     
                     # Create a string version of the design object to write out
                     if (!is.null(workspace$design)) {
                       message("Creating a string version of the design object to write into the script")
                       workspace$design.string <- paste(paste0("'",
                                                               names(workspace$design), "' = ",
                                                               gsub(paste0(as.character(workspace$design)),
                                                                    pattern = "\\\"",
                                                                    replacement = "'")),
                                                        collapse = ",")
                       
                       # Add the panel names to workspace$design.string because they were lost in the process
                       workspace$strata.panels <- unlist(stringr::str_extract_all(string = workspace$design.string,
                                                                                  pattern = "panel = c[(](\\d|,| ){1,1000}[)]"))
                       for (stratum in workspace$strata.panels) {
                         # I'll revisit this to make it prettier
                         # Given that this works, I'm disinclined to touch it anymore
                         # The point is that it makes a version of the design object
                         # that can be pasted into the output sample_script.R and
                         # Just Work(TM)
                         workspace$design.string <- gsub(workspace$design.string,
                                                         pattern = gsub(gsub(stratum,
                                                                             pattern = "[(]",
                                                                             replacement = "[(]"),
                                                                        pattern = "[)]",
                                                                        replacement = "[)]"),
                                                         replacement = paste0("panel = c(",
                                                                              paste(gsub(paste0("'",
                                                                                                workspace$panels,
                                                                                                "'=",
                                                                                                stringr::str_extract_all(string = stratum,
                                                                                                                         pattern = "\\d{1,4}")[[1]]),
                                                                                         pattern = "\\\"",
                                                                                         replacement = ""),
                                                                                    collapse = ","),
                                                                              ")"))
                       }
                     }
                   }
                   
                   # NO DOWNLOADING RESULTS!!!! Drawing new points invalidates any existing output
                   workspace$downloadready <- FALSE
                   
                   if (!is.null(workspace$design) & !is.null(workspace$polygons)) {
                     message("There's a design object and polygons ready to go")
                     set.seed(input$seednum)
                     
                     message("Writing sample frame")
                     sf::st_write(obj = sf::st_zm(x = workspace$polygons)[, "STRATUM"],
                                  dsn = workspace$sessiontempdir,
                                  layer = "sample_frame",
                                  driver = "ESRI Shapefile",
                                  append = FALSE)
                     message("Sample frame written")
                     
                     message("Building the output script")
                     
                     # Construct the script to draw a design with the the current design object
                     # The first step is copying the script that has the initial content
                     # If overwrite = FALSE then this turns into a hot mess with multiple attempts at a design
                     file.copy(from = paste0(workspace$origdir, "/draw_pt1.R"),
                               to = paste0(workspace$sessiontempdir, "/sample_script.R"),
                               overwrite = TRUE)
                     
                     # This is the metadata section describing the design setup
                     workspace$draw_pt2 <- c("# Project Name:",
                                             paste0("project.name <- '",
                                                    gsub(input$projname,
                                                         pattern = "\\W",
                                                         replacement = ""),
                                                    "'"),
                                             "",
                                             "# Original stratification shapefile name:",
                                             paste0("# ", workspace$shapename),
                                             "# Original stratification attribute field name:",
                                             paste0("# ", input$strataname),
                                             "",
                                             "# Design panel names:",
                                             paste0("# ", paste(workspace$panels, collapse = ", ")),
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
                     
                     workspace$draw_pt3 <- readLines(paste0(workspace$origdir, "/draw_pt3.R"),
                                                     warn = FALSE)
                     
                     workspace$draw_pt4 <- c("",
                                             paste0("design.object <- list(", workspace$design.string,")"))
                     
                     workspace$draw_pt5 <- readLines(paste0(workspace$origdir, "/draw_pt5.R"),
                                                     warn = FALSE)
                     
                     # Append the script components to the copy of sample_script.R
                     cat(c(workspace$draw_pt2, workspace$draw_pt3, workspace$draw_pt4, workspace$draw_pt5),
                         file = paste0(workspace$sessiontempdir, "/sample_script.R"),
                         sep = "\n",
                         append = TRUE)
                     
                     message("Deleting the script because it needs to be reworked")
                     file.remove(paste0(workspace$sessiontempdir, "/sample_script.R"))
                     
                     message(paste0("There are ", length(workspace$polygons[["STRATUM"]]), " stratum entries"))
                     message(class(workspace$polygons))
                     
                     missing_from_polys <- workspace$polygons[["STRATUM"]][!(workspace$polygons[["STRATUM"]] %in% names(workspace$design))]
                     missing_from_do <- names(workspace$design)[!(names(workspace$design) %in% workspace$polygons[["STRATUM"]])]
                     if (length(missing_from_polys) > 0) {
                       message("The following strata are missing from the design object: ", paste(missing_from_polys, collapse = ", "))
                     } else {
                       message("All strata from the polygons appear in the design object")
                     }
                     if (length(missing_from_do) > 0) {
                       message("The following strata are missing from the polygons", paste(missing_from_do, collapse = ", "))
                     } else {
                       message("All strata from the design object appear in the polygons")
                     }
                     
                     # Generate the points
                     message(paste0("workspace$polygons class is: ",
                                    paste(class(workspace$polygons),
                                          collapse = ", ")))
                     set.seed(input$seednum)
                     message("Generating points")
                     # DRAW SOME POINTS
                     # BUT ALSO MAKE SURE ERRORS DON'T KILL THE TOOL
                     grts_output <- tryCatch(grts.custom(design_object = workspace$design,
                                                         sample_frame = workspace$polygons,
                                                         stratum_field = "STRATUM",
                                                         seed_number = input$seednum),
                                             error = function(e){
                                               message("")
                                               return(paste0("ERROR ENCOUNTERED ON DRAW: ",
                                                             paste(e,
                                                                   collapse = "\n")))})
                     
                     # So if there was an error, we'll render that to the UI, otherwise proceed as normal
                     message("Checking to make sure that grts_output is an sf object")
                     
                     if (!("sf" %in% class(grts_output))) {
                       showNotification(ui = grts_output,
                                        duration = NULL,
                                        closeButton = TRUE,
                                        id = "grts_error",
                                        type = "error")
                       workspace$points <- NULL
                     } else {
                       workspace$points <- grts_output
                       sf::st_write(obj = workspace$points,
                                    dsn = workspace$sessiontempdir,
                                    layer = "sample_draw",
                                    driver = "ESRI Shapefile",
                                    append = FALSE)
                       
                       if (!any(grepl(x = list.files(workspace$sessiontempdir), pattern = "sample_draw.shp"))) {
                         stop("No shapefile called 'sample_draw' exists in the directory.")
                       }
                       
                       # Create a .zip fle in case user wants the points, which depends on a system call
                       setwd(workspace$sessiontempdir)
                       files_to_zip <- list.files(pattern = "^(sample_frame|sample_draw|sample_script)\\.(dbf|prj|shp|shx|r)$",
                                                  ignore.case = TRUE)
                       
                       zip::zip(zipfile = "results.zip",
                                files = files_to_zip)
                       if (!any(grepl(x = list.files(workspace$sessiontempdir), pattern = "^results\\.(zip)|(ZIP)"))) {
                         stop("No valid .zip file called 'results' exists in the directory.")
                       }
                       workspace$downloadready <- TRUE
                       setwd(workspace$origdir)
                       message("Points generated")
                     }
                     
                     # Make the map!
                     message(paste0("Checking to see if the class of workspace$points (",
                                    paste(class(workspace$points),
                                          collapse = ", "),
                                    ") contains 'sf' and the answer is ",
                                    "sf" %in% class(workspace$points)))
                     # But only if the points were sucessfully generated. If there was an error don't try
                     if (req("sf" %in% class(workspace$points))) {
                       message("Making the map!")
                       output$pointmap <- renderLeaflet(expr = {
                         # Initialize the map
                         map <- leaflet()
                         # Add some basic info
                         map <- addTiles(map = map)
                         # Make a strata palette to use for the map
                         strata_palette <- colorFactor(palette = "viridis",
                                                       levels = sort(unique(workspace$polygons[["STRATUM"]])))
                         # Add the stratification polygons
                         map <- addPolygons(map = map,
                                            data = sf::st_transform(x = workspace$polygons,
                                                                    crs = "+proj=longlat +datum=WGS84"),
                                            color = ~strata_palette(STRATUM),
                                            stroke = FALSE,
                                            fillOpacity = 0.7)
                         # Add in the generated points
                         map <- addCircleMarkers(map = map,
                                                 data = sf::st_transform(x = workspace$points,
                                                                         crs = "+proj=longlat +datum=WGS84"),
                                                 stroke = TRUE,
                                                 opacity = 0.9,
                                                 color = "white",
                                                 weight = 1,
                                                 fillColor = "gray20",
                                                 fillOpacity = 1,
                                                 radius = 3)
                         
                         # Add in a legend for the strata!
                         map <- addLegend(map = map,
                                          position = "topright",
                                          pal = strata_palette,
                                          values = ~STRATUM,
                                          data = workspace$polygons,
                                          title = "Strata",
                                          opacity = 1)
                         
                         map
                       })
                       message("Displaying map")
                     } else {
                       output$pointmap <- NULL
                       message("No map to display")
                     }
                   }
                 } else {
                   # The pointcount situation wasn't valid, so we don't have a design
                   workspace$design <- NULL
                 }
               })
  
  #### Extracting the contents of a .zip and returning an sf object from the contents ####
  shape.extract <- reactive({
    message("Polygons file uploaded")
    # Because it looks like I can't enforce filetype in the upload
    # selection dialogue, check it here
    # I'm assuming that a file extension can be 1-5 characters long
    # although the longest I've seen is 4, I think
    polygon_upload_extension <- toupper(stringr::str_extract(string = input$polygons$datapath,
                                                             pattern = "(?<=\\.).{1,5}$"))
    polygons_are_zip <- polygon_upload_extension == "ZIP"
    
    if (!polygons_are_zip) {
      showNotification(ui = "Polygons must be uploaded as a zipped shapefile or zipped geodatabase.",
                       duration = NULL,
                       closeButton = TRUE,
                       id = "polygons_zip_error",
                       type = "error")
    } else {
      message("Attempting to unzip file")
      message(paste0("input$polygons$datapath is ",
                     input$polygons$datapath))
      utils::unzip(zipfile = input$polygons$datapath,
                   overwrite = TRUE,
                   exdir = dirname(input$polygons$datapath))
      message("File unzipped")
      # Get the shapefile name
      extracted_files <- list.files(dirname(input$polygons$datapath),
                                    full.names = TRUE,
                                    recursive = TRUE)
      
      # Look for extracted shapefiles
      shp_indices <- grepl(extracted_files,
                           pattern = "\\.shp$",
                           ignore.case = TRUE)
      message(paste0("Found ",
                     sum(shp_indices),
                     " shapefiles"))
      upload_has_shp <- any(shp_indices)
      
      # Look for extracted geodatabases
      # Now because this is recursive, we're turning up the files
      # inside the GDBs, so we need to get just the GDB path
      gdb_indices <- grepl(extracted_files,
                           pattern = "\\.gdb",
                           ignore.case = TRUE)
      gdb_paths <- unique(stringr::str_extract(string = extracted_files[gdb_indices],
                                               pattern = ".*(?=\\.gdb/)"))
      gdb_paths <- paste0(gdb_paths,
                          ".gdb")
      gdb_paths <- gdb_paths[!(gdb_paths %in% c(".gdb"))]
      
      message(paste0("Found ",
                     length(gdb_paths),
                     " geodatabases"))
      upload_has_gdb <- length(gdb_paths) > 0
      
      # Prioritize geodatabases
      if (upload_has_gdb) {
        workspace$polygon_filetype <- "gdb"
        message("Working from extracted geodatabase")
        # If there's more than one geodatabase, just use the first
        # but warn the user
        if (length(gdb_paths) > 1) {
          message("Multiple GDBs detected. Using 'first' one")
          showNotification(ui = "More than one geodatabase found in ZIP file. Please upload one at a time.",
                           duration = NULL,
                           closeButton = TRUE,
                           type = "warning",
                           id = "multiple_gdb_warning")
        }
        current_gdb_path <- gdb_paths[1]
        
        # So I can reference this when reading in layers later
        workspace$gdb_filepath <- current_gdb_path
        # Find which layers are available
        available_polygons <- sf::st_layers(dsn = current_gdb_path)$name
        message(paste0("Available layers in GDB are: ",
                       paste(available_polygons,
                             collapse = ", ")))
        current_polygons <- sf::st_read(dsn = workspace$gdb_filepath,
                                        layer = available_polygons[1])
        current_polygons <- area.add(current_polygons,
                                     area.sqkm = FALSE)
        return(current_polygons)
      } else if (upload_has_shp) {
        workspace$polygon_filetype <- "shp"
        message("Working with extracted shapefile(s)")
        # Which files end in .shp?
        available_polygons <- extracted_files[shp_indices]
        
        message(paste0("Before checking for all files, the available shapefiles are: ",
                       paste(basename(available_polygons),
                             collapse = ", ")))
        
        # And which of those .shp files has associated
        # .prj, .dbf, and .shx files?
        has_all_files <- sapply(X = available_polygons,
                                files = extracted_files,
                                FUN = function(X, files) {
                                  required_filetypes <- c("dbf",
                                                          "prj",
                                                          "shp",
                                                          "shx")
                                  file_pattern <- gsub(x = X,
                                                       pattern = "shp$",
                                                       replacement = "",
                                                       ignore.case = TRUE)
                                  all(sapply(X = required_filetypes,
                                             files = files,
                                             file_pattern = file_pattern,
                                             FUN = function(X, files, file_pattern) {
                                               paste0(file_pattern,
                                                      X) %in% files
                                             }))
                                })
        # So then which shapefiles are really valid?
        available_polygons <- available_polygons[has_all_files]
        message(paste0("After checking for all files, the available shapefiles are: ",
                       paste(basename(available_polygons),
                             collapse = ", ")))
        
        # Update the selection options
        # This makes a named vector of the shp filepaths
        # so it's easy to read them in later but the options are
        # human readable in the GUI
        polygon_shp_options <- available_polygons
        shp_filenames <- gsub(x = basename(available_polygons),
                              pattern = "\\.shp$",
                              replacement = "",
                              ignore.case = TRUE)
        names(available_polygons) <- shp_filenames
        current_polygons <- sf::st_read(dsn = polygon_shp_options[1])
        current_polygons <- area.add(current_polygons,
                                     area.sqkm = FALSE)
        return(current_polygons)
        # updateSelectInput(session = session,
        #                   inputId = "polygons_layer",
        #                   choices = available_polygons,
        #                   selected = available_polygons[1])
      } else {
        showNotification(ui = "Uploaded file does not appear to contain either a valid shapefile or geodatabase.",
                         duration = NULL,
                         closeButton = TRUE,
                         type = "error",
                         id = "empty_upload_error")
      }
    }
  })
  
  #### Update the points table ###################################################
  observeEvent(eventExpr = workspace$points,
               handlerExpr = {
                 if ("sf" %in% class(workspace$points)) {
                   output$pointdata <- DT::renderDT(sf::st_drop_geometry(workspace$points),
                                                    height = "80vh",
                                                    rownames = FALSE,
                                                    options = list(pageLength = 10,
                                                                   fixedHeader = TRUE,
                                                                   scrollX = TRUE,
                                                                   scrollY = TRUE), 
                                                    extensions = "FixedHeader"
                   )
                   
                   # Make a summary of these things!
                   workspace$design_summary <- dplyr::summarize(.data = dplyr::group_by(sf::st_drop_geometry(workspace$points),
                                                                                        STRATUM,
                                                                                        PANEL),
                                                                point_count = dplyr::n())
                   workspace$design_summary <- tidyr::pivot_wider(data = workspace$design_summary,
                                                                  id_cols = STRATUM,
                                                                  names_from = PANEL,
                                                                  values_from = point_count)
                   # Reorder the columns
                   summary_cols <- c("STRATUM", workspace$panels, "Oversample")
                   summary_cols <- summary_cols[summary_cols %in% names(workspace$design_summary)]
                   
                   output$design_summary <- DT::renderDT(workspace$design_summary[, summary_cols],
                                                         rownames = FALSE,
                                                         options = list(fixedHeader = TRUE,
                                                                        scrollX = TRUE,
                                                                        scrollY = TRUE), 
                                                         extensions = "FixedHeader"
                   )
                 }
               })
  
  #### Download handler for the .zip file created by grts.gen() ##################
  output$download_button <- downloadHandler(
    filename = function() {
      paste0(gsub(input$projname,
                  pattern = "\\W",
                  replacement = ""),
             "_results_",
             paste0(format(Sys.Date(), "%Y-%m-%d"),
                    format(Sys.time(), "T%H%MZ", tz = "GMT")),
             ".zip")
    },
    content = function(file) {
      file.copy(paste0(workspace$sessiontempdir, "/results.zip"), file)
    })
})