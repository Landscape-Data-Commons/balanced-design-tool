library(shiny)

fluidPage(
  # This changes the position and styling of the notification
  # Old RGB: 204,169,44
  tags$head(
    tags$style(
      HTML(
        ".shiny-notification {
          position:fixed;
          bottom: calc(60%);
          left: calc(5%);
          width: calc(25%);
          opacity: 1;
          font-weight: bold;
          box-shadow: 0 0 0 rgba(181,181,181, 0.4);
          animation: pulse 2s infinite;
        }
        @-webkit-keyframes pulse {
          0% {
            -webkit-box-shadow: 0 0 0 0 rgba(181,181,181, 0.4);
          }
          70% {
            -webkit-box-shadow: 0 0 0 10px rgba(181,181,181, 0);
          }
          100% {
            -webkit-box-shadow: 0 0 0 0 rgba(181,181,181, 0);
          }
        }
        @keyframes pulse {
          0% {
            -moz-box-shadow: 0 0 0 0 rgba(181,181,181, 0.4);
            box-shadow: 0 0 0 0 rgba(181,181,181, 0.4);
          }
          70% {
            -moz-box-shadow: 0 0 0 10px rgba(181,181,181, 0);
            box-shadow: 0 0 0 10px rgba(181,181,181, 0);
          }
          100% {
            -moz-box-shadow: 0 0 0 0 rgba(181,181,181, 0);
            box-shadow: 0 0 0 0 rgba(181,181,181, 0);
          }
        }"
      )
    )
  ),
  titlePanel("Spatially Balanced Sampling Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "uploadzip",
                label = "Upload a polygon shapefile in a .ZIP",
                multiple = FALSE,
                accept = c("application/zip")),
      checkboxInput(inputId = "repair",
                    label = "Check shapefile geometry and attempt to repair if necessary",
                    value = TRUE),
      textInput(inputId = "projname",
                label = "Name for the project:",
                value = ""),
      selectInput(inputId = "strataname",
                  label = "Select a stratum field:",
                  choices = c("")),
      conditionalPanel(condition = "input.strataname != ''",
                       actionButton(inputId = "submitstratum",
                                    label = "Update stratum information")),
      # The empty string is just to take up space and buffer the buttons
      helpText(""),
      conditionalPanel(condition = "input.allocated > 0",
                       actionButton(inputId = "fetch",
                                    label = "Fetch points!")),
      # The empty string is just to take up space and buffer the buttons
      helpText(""),
      conditionalPanel(condition = "input.fetch > 0",
                       downloadButton(outputId = 'downloadData',
                                      label = 'Download Points as shapefile'))
    ),
    mainPanel(
      tabsetPanel(
        id = "maintabs",
        
        tabPanel("Instructions",
                 includeHTML("instructions.html"),
                 HTML('<img style="float: right"; src="JornadaLogo_1.png"/>')
        ),
        
        tabPanel(title = "Point Allocation",
                 column(width = 6,
                        # This is just a buffer to make things prettier
                        helpText(""),
                        selectInput(inputId = "allocation",
                                    label = "Allocate points proportionally by strata areas, equally across strata, or manually:",
                                    choices = c(""),
                                    selected = ""),
                        numericInput(inputId = "seednum",
                                     label = "Seed number for reproducibility:",
                                     value = 419,
                                     min = 0),
                        conditionalPanel(condition = "input.allocation != ''",
                                         conditionalPanel(condition = "input.panelnames == '' && input.allocation != ''",
                                                          helpText("Panel names are still required")),
                                         textInput(inputId = "panelnames",
                                                   label = "Names of the panels, separated by commas",
                                                   value = ""),
                                         # helpText("Enter the number of points to draw."),
                                         uiOutput("basecount"),
                                         uiOutput("minimumbase"),
                                         uiOutput("oversamplemin"),
                                         conditionalPanel(condition = "input.allocation == 'Manually'",
                                                          tagList(
                                                            column(width = 6,
                                                                   helpText("Number of base points per panel"),
                                                                   tags$div(id = 'mabase')),
                                                            column(width = 6,
                                                                   helpText("Number of oversample points per panel"),
                                                                   tags$div(id = 'maover'))
                                                          )
                                         )
                        )
                        ),
                 column(width = 4,
                        conditionalPanel(condition = "input.allocation != '' && input.panelnames != ''",
                                         helpText("Once you've finished setting the allocation for points, click the button below to store the design scheme"),
                                         actionButton("allocated", "Update point allocation"),
                                         helpText("After updating the design scheme, click the 'Fetch points!' button on the left to draw your points.")
                        ),
                        plotOutput(outputId = "strata_map")
                 )
                 
        ),
        
        tabPanel(title = "Point Map",
                 helpText("Map of selected sample points"),
                 leaflet::leafletOutput(outputId = "pointmap",
                                        height = "80vh")
        ),
        
        tabPanel(title = "Point Table",
                 helpText("Table of selected sample points"),
                 tableOutput(outputId = "pointdata")
        ),
        tabPanel("About",
                 includeHTML("about.html"))
        ,
        tabPanel("Glossary",
                 includeHTML("glossary.html"))
      )
    )
  )
)
