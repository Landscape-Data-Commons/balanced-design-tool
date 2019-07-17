library(dplyr)
library(spdplyr)
library(tools)
library(shiny)
library(rgdal)
library(spsurvey)
source('support.functions.r')

# Define server logic
shinyServer(function(input, output, session) {
  
  # This is the id of the current notification (used to tell the user that the server's working on something)
  # id <- NULL
  
  ## Initialize temp to work within
  temp <- reactiveValues(placeholder = "placeholder",
                         ## Save what the base working directory is
                         origdir = getwd(),
                         sessiontempdir = tempdir()
                         )
  
  # allow for wonking big files
  options(shiny.maxRequestSize = 30 * 1024^2)
  ## Get a full stacktrace for debugging purposes, although it's unlikely to be ncessary or even helpful
  # options(shiny.fullstacktrace = TRUE)
  
  ## When a valid shapefile-containing .zip gets uploaded, update the inputs that are available
  observeEvent(eventExpr = input$uploadzip,
               handlerExpr = {
                 # Display a busy message
                 showNotification(ui = "Please wait while the shapefile is extracted and loaded. This can take a bit with large and complex polygons.",
                                  # ui = '<img src="busy.gif" class="w3-round" alt="BUSY">',
                                  duration = NULL,
                                  closeButton = FALSE,
                                  id = "busy",
                                  type = "warning")

                 ## Get the directory to work within
                 temp$directory <- gsub(input$uploadzip$datapath,
                                        pattern = "/[0-9]{1,3}$",
                                        replacement = "")
                 temp$spdf <- shape.extract()
                 if (is.null(temp$spdf)) {
                   fieldnames <- "No valid single shapefile found"
                 } else {
                   fieldnames <- names(temp$spdf@data)
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
                                 value = stringr::str_replace(input$uploadzip$name,
                                                              pattern = "\\.(zip)|(ZIP)$",
                                                              replacement = ""))

                 # Remove the busdy notification
                 removeNotification(id = "busy")
               })
  
  ## When the user clicks the button after selecting a stratum field
  observeEvent(eventExpr = input$submitstratum,
               handlerExpr = {
                 # Display a busy message
                 showNotification(ui = "Updating stratification information.",
                                  duration = NULL,
                                  closeButton = FALSE,
                                  id = "busy",
                                  type = "warning")
                 
                 if (input$strataname != "") {
                   ## Add the relevant values to STRATUM
                   temp$spdf@data$STRATUM <- as.character(temp$spdf@data[, input$strataname])
                   
                   ## Write this file out to use in spsurvey::grts()
                   rgdal::writeOGR(obj = temp$spdf[,"STRATUM"],
                                   dsn = temp$sessiontempdir,
                                   layer = "current",
                                   driver = "ESRI Shapefile",
                                   overwrite_layer = TRUE)
                   print(list.files(path = temp$sessiontempdir, pattern = "current"))
                   
                   ## Jump to the map, but only if it won't drag the user away from the allocation tab
                   if (!(input$maintabs == "Point Allocation" & input$allocation != "")) {
                     updateTabsetPanel(session,
                                       inputId = "maintabs",
                                       selected = "Point Allocation") 
                   }
                   
                   ## Update the UI
                   ## If there are already textInput()s for strata, remove them with the lookup table
                   if (!is.null(temp$ui.lut) & input$allocation == "Manually") {
                     print("attempting to remove ui elements")
                     for (id in 1:nrow(temp$ui.lut)) {
                       print(temp$ui.lut$STRATUM[id])
                       removeUI(selector = paste0("div:has(> #", temp$ui.lut$base[id], ")"), multiple = TRUE, immediate = TRUE)
                       removeUI(selector = paste0("div:has(> #", temp$ui.lut$over[id], ")"), multiple = TRUE, immediate = TRUE)
                     }
                   }
                   
                   ## Build a lookup table of strata and their corresponding inputIds for use later
                   temp$ui.lut <-  data.frame(STRATUM = unique(temp$spdf@data$STRATUM))
                   temp$ui.lut$base <- paste0("manualbase", rownames(temp$ui.lut))
                   temp$ui.lut$over <- paste0("manualover", rownames(temp$ui.lut))
                   
                   ## Add the inputs for strata using the lookup table. A loop is necessary, unfortunately
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
  
  ## When the user selects a new allocation scheme, as long as it's not blank, jump to the relevant tab
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
  
  ## When the user clicks the button indicating that they're done with their point allocation, generate a design object
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
                 if (input$allocation == "Manually") {
                   ## Get all the inputs because I can't just slice them out all at once from a reactive list
                   temp$inputs <- c()
                   for (id in c(temp$ui.lut$base, temp$ui.lut$over)) {
                     temp$inputs <- c(temp$inputs, input[[id]])
                   }
                   temp$inputs <- setNames(temp$inputs,
                                           c(temp$ui.lut$base, temp$ui.lut$over))
                   ## Build the design object
                   temp$design <- lapply(temp$ui.lut$STRATUM,
                                         function(X, ui.lut, panel.names, input){
                                           list(
                                             panel = setNames(rep(input[ui.lut$base[ui.lut$STRATUM == X]],
                                                                  times = length(panel.names)),
                                                              panel.names),
                                             seltype = "Equal",
                                             over = input[ui.lut$over[ui.lut$STRATUM == X]] * length(panel.names)
                                           )
                                         },
                                         ui.lut = temp$ui.lut,
                                         panel.names = temp$panels,
                                         input = temp$inputs
                   )
                   temp$design <- setNames(temp$design,
                                           temp$ui.lut$STRATUM)
                   output$design <- renderText({
                     paste(temp$design)
                   })
                 } else {
                   if (input$allocation == "Proportionally") {
                     sizes <- dplyr::summarize(dplyr::group_by(temp$spdf@data,STRATUM),
                                               AREA = sum(AREA.HA))
                     basecount <- input$basecount
                     minbase <- input$minbase
                     minoversample <- input$minoversample
                     minoversampleproportion <- input$minoversampleproportion
                   }
                   if (input$allocation == "Equally") {
                     sizes <- data.frame(STRATUM = temp$spdf@data$STRATUM,
                                         AREA = rep(1,
                                                    times = length(unique(temp$spdf@data$STRATUM))))
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
                 ## Create a string version of the design object to write out
                 temp$design.string <- paste(paste0("'",
                                                    names(temp$design), "' = ",
                                                    stringr::str_replace_all(string = paste0(as.character(temp$design)),
                                                                             pattern = "\\\"",
                                                                             replacement = "'")),
                                             collapse = ",")
                 
                 ## Add the panel names to temp$design.string because they were lost in the process
                 temp$strata.panels <- unlist(stringr::str_extract_all(string = temp$design.string,
                                                                       pattern = "panel = c[(]([0-9]|,| ){1,1000}[)]"))
                 for (stratum in temp$strata.panels) {
                   # I'll revisit this to make it prettier. Removing the piping was the priority in the meantime
                   temp$design.string <- stringr::str_replace_all(replacement = paste0("panel = c(",
                                                                                       paste(stringr::str_replace_all(paste0("'",
                                                                                                                             temp$panels,
                                                                                                                             "'=",
                                                                                                                             stringr::str_extract_all(string = stratum,
                                                                                                                                                      pattern = "[0-9]{1,4}")[[1]]),
                                                                                                                      pattern = "\\\"",
                                                                                                                      replacement = ""),
                                                                                             collapse = ","),
                                                                                       ")"),
                                                                  string = temp$design.string,
                                                                  pattern = stringr::str_replace_all(stringr::str_replace_all(stratum,
                                                                                                                              pattern = "[(]",
                                                                                                                              replacement = "[(]"),
                                                                                                     pattern = "[)]",
                                                                                                     replacement = "[)]"))
                 }
                 # Remove the busy notification
                 removeNotification(id = "busy")
               })
  
  ## When the user clicks the fetch button, generate points from the design object
  observeEvent(eventExpr = input$fetch,
               handlerExpr = {
                 # Display a busy message
                 showNotification(ui = "Please wait while the sample points are drawn. This can take a bit with large and complex designs.",
                                  duration = NULL,
                                  closeButton = FALSE,
                                  id = "busy",
                                  type = "warning")
                 
                 if (!is.null(temp$design) & !is.null(temp$spdf)) {
                   temp$seednum <- sample(1:999999)
                   # temp$seednum <- runif(ceiling(runif(n = 1, min = 100000, max = 999998)))
                   set.seed(temp$seednum)
                   
                   ## Write out the shapefile of the stratification polygons
                   rgdal::writeOGR(obj = temp$spdf[, "STRATUM"],
                                   dsn = temp$sessiontempdir,
                                   layer = "sample_frame",
                                   driver = "ESRI Shapefile",
                                   overwrite_layer = TRUE)
                   
                   ## Construct the script to draw a design with the the current design object
                   ## The first step is copying the script that has the initial content
                   file.copy(from = paste0(temp$origdir, "/draw_pt1.r"),
                             to = paste0(temp$sessiontempdir, "/sample_script.r"))
                   
                   ## This is the metadata section describing the design setup
                   temp$draw_pt2 <- c("# Project Name:",
                                      paste0("project.name <- ", input$projname),
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
                                      if (input$allocation == "Proportionally") {
                                        c(paste0("# Total number of base points per panel: ", input$basecount),
                                          paste0("# Minimum number of base points per stratum per panel: ", input$minbase),
                                          paste0("# Minimum number of oversample points per stratum per panel: ", input$minoversample),
                                          paste0("# Minimum ratio of oversample:base points per stratum per panel: ", input$minoversampleproportion),
                                          ""
                                        )
                                      } else if (input$allocation == "Equally") {
                                        c(paste0("# Total number of base points per panel: ", input$basecount),
                                          paste0("# Minimum number of oversample points per stratum per panel: ", input$minoversample),
                                          paste0("# Minimum ratio of oversample:base points per stratum per panel: ", input$minoversampleproportion),
                                          "")
                                      } else if (input$allocation == "Manually") {
                                        c("# See the design object construction in section 4 for details on point allocation.",
                                          "")
                                      },
                                      "# Seed number used during sample draw:",
                                      paste0("seed.number <- ", temp$seednum),
                                      ""
                   )
                   
                   temp$draw.pt3 <- readLines(paste0(temp$origdir, "/draw_pt3.r"))
                   
                   temp$draw.pt4 <- c("",
                                      paste0("design.object <- list(", temp$design.string,")"))
                   
                   temp$draw.pt5 <- readLines(paste0(temp$origdir, "/draw_pt5.r"))
                   
                   ## Append the script components to the copy of sample_script.r
                   cat(c(temp$draw.pt2, temp$draw.pt3, temp$draw.pt4, temp$draw.pt5), file = paste0(temp$sessiontempdir, "/sample_script.r"), sep = "\n", append = T)
                   
                   ## Generate the points
                   temp$points <- grts.gen()
                   
                 }
                 
                 # Remove the busy notification
                 removeNotification(id = "busy")
               })
  
  ## Extracting the contents of a .zip and returning an SPDF of the contents
  shape.extract <- reactive({
    shapes <- input$uploadzip
    ## If there's no input file
    if (is.null(shapes)) {
      return(NULL)
    } 
    ## If the input file is not a zip file
    if (!grepl(pattern = "\\.(zip)|(ZIP)$", shapes$name)) {
      return(NULL)
    }
    print("File exists and ends in .zip")
    print("The value in shapes$datapath is:")
    print(dirname(shapes$datapath))
    ## Unzip with an OS-specific system call
    switch(Sys.info()[["sysname"]],
           Windows = {
             print("This is Windows.")
             ## Set the new working directory to the uploaded file's datapath. Not sure why this is here, but removing it breaks stuff
             setwd(dirname(shapes$datapath))
             ## Pass this argument to the OS. It changes directories. When making Windows system calls, you need to invoke "cmd.exe /c" first
             system(paste0("cmd.exe /c cd ", dirname(shapes$datapath)))
             ## Pass the extraction argument to the OS. I had to aim it at my 7zip install. If yours is elsewhere, change the filepath to it, but know that those escaped quotation marks are necessary if there are spaces in your folder names. Thanks, Microsoft
             system(paste0("cmd.exe /c \"C:\\Program Files\\7-Zip\\7z\".exe e -aoa ", shapes$datapath))
             setwd(temp$origdir) ## Restoring the working directory
             ## Diagnostic terminal output to reassure a debugger that it is in fact reset to the original working directory
             print("Resetting working directory to:")
             print(getwd())
           },
           Linux = {
             print("This is Unix. I know this.")
             ## Setting the working directory
             setwd(dirname(shapes$datapath))
             ## Passing this to the OS
             system(sprintf("cd %s", dirname(shapes$datapath)))
             ## Just checking for debugging
             print(getwd())
             ## The unzipping argument to pass to the OS
             system(sprintf("unzip -u %s", shapes$datapath))
             ## Set the working directory back
             setwd(temp$origdir)
           }
    )
    ## Get the shapefile name
    temp$shapename <- list.files(dirname(shapes$datapath), pattern = "\\.shp$")
    temp$shapename <- stringr::str_replace(temp$shapename,
                                           pattern = "\\.shp$",
                                           replacement = "")
    ## If there wasn't a shapefile or there was more than one, return NULL
    if (length(temp$shapename) != 1) {
      return(NULL)
    }
    ## Read in the shapefile and add areas
    spdf <- rgdal::readOGR(dsn = dirname(shapes$datapath),
                           layer = temp$shapename)
    spdf <- area.add(spdf,
                     area.sqkm = FALSE)
    ## Store the shapefile location to reference later to read in the shapefiles within spsurvey::grts()
    temp$shapefile.location <- paste0(shapes$directory, temp$shapename, sep = "/")
    return(spdf)
  })
  
  ## Just listening for if something that should update the map changes
  listen.map <- reactive({
    list(input$strataname, input$updatemap, temp$points)
  })
  
  ## Update the points table
  observeEvent(eventExpr = temp$points,
               handlerExpr = {
                 output$pointdata <- renderTable(temp$points@data)
               })
  
  ## This invokes grts.custom() and both returns and writes out the results
  grts.gen <- reactive({
    
    points <- grts.custom(design.object = temp$design,
                          design.name = input$projname,
                          in.shape = paste0(temp$sessiontempdir, "/current")
    )
    
    save(points, file = "results")
    rgdal::writeOGR(obj = points,
                    dsn = temp$sessiontempdir,
                    layer = "results",
                    driver = "ESRI Shapefile",
                    overwrite_layer = TRUE)
    
    if (!any(grepl(x = list.files(temp$sessiontempdir), pattern = "results"))) {
      stop("No shapefile called 'results' exists in the directory.")
    }
    
    ## Create a .zip fle in case user wants the points, which depends on a system call
    setwd(temp$sessiontempdir)
    switch(Sys.info()[["sysname"]],
           Windows = {
             system(paste0("cmd.exe /c \"C:\\Program Files\\7-Zip\\7z\".exe a -tzip results.zip ",
                           paste(list.files(pattern = "(dbf|prj|shp|shx|r)$",
                                                       ignore.case = TRUE),
                                 collapse = " ")))
           },
           Linux = {
             system(paste("zip results %s",
                          paste(list.files(pattern = "\\.(dbf|prj|shp|shx|r)$",
                                           ignore.case = TRUE),
                                collapse = " ")))
           })
    if (!any(grepl(x = list.files(temp$sessiontempdir), pattern = "^results\\.(zip)|(ZIP)"))) {
      stop("No valid .zip file called 'results' exists in the directory.")
    }
    temp$downloadready <- TRUE
    updateTabsetPanel(session,
                      inputId = "maintabs",
                      selected = "Map")
    setwd(temp$origdir)
    return(points)
  })
  
  ## Download handler for the .zip file created by grts.gen()
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(stringr::str_trim(input$projname), "_results-", format(Sys.Date(), "%d%m%y"), ".zip")
    },
    content = function(file) {
      file.copy(paste0(temp$sessiontempdir, "/results.zip"), file)
    })
})



