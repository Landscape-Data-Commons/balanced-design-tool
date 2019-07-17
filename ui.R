library(shiny)

fluidPage(
  # This changes the position and styling of the notification
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
          box-shadow: 0 0 0 rgba(204,169,44, 0.4);
          animation: pulse 2s infinite;
        }
        @-webkit-keyframes pulse {
          0% {
            -webkit-box-shadow: 0 0 0 0 rgba(204,169,44, 0.4);
          }
          70% {
            -webkit-box-shadow: 0 0 0 10px rgba(204,169,44, 0);
          }
          100% {
            -webkit-box-shadow: 0 0 0 0 rgba(204,169,44, 0);
          }
        }
        @keyframes pulse {
          0% {
            -moz-box-shadow: 0 0 0 0 rgba(204,169,44, 0.4);
            box-shadow: 0 0 0 0 rgba(204,169,44, 0.4);
          }
          70% {
            -moz-box-shadow: 0 0 0 10px rgba(204,169,44, 0);
            box-shadow: 0 0 0 10px rgba(204,169,44, 0);
          }
          100% {
            -moz-box-shadow: 0 0 0 0 rgba(204,169,44, 0);
            box-shadow: 0 0 0 0 rgba(204,169,44, 0);
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
      textInput(inputId = "projname",
                label = "Name for the project:",
                value = ""),
      selectInput(inputId = "strataname",
                  label = "Select a stratum field:",
                  choices = c("")),
      conditionalPanel(condition = "input.strataname != ''",
                       actionButton(inputId = "submitstratum",
                                    label = "Update stratum information")),
      conditionalPanel(condition = "input.allocated > 0",
                       actionButton(inputId = "fetch",
                                    label = "Fetch points!")
      ),
      conditionalPanel(condition = "input.fetch > 0",
                       downloadButton(outputId = 'downloadData',
                                      label = 'Download Points as shapefile')
                       ),
      # conditionalPanel(condition = "output.busy == 'yes'",
                       # img(src='busy.gif', align = "left")
      # )
      
      imageOutput("busy")
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
                        selectInput(inputId = "allocation",
                                    label = "Allocate points proportionally by strata areas, equally across strata, or manually:",
                                    choices = c(""),
                                    selected = "")
                        ),
                 column(width = 4,
                        conditionalPanel(condition = "input.panelnames == '' && input.allocation != ''",
                                         helpText("Panel names are still required")),
                        conditionalPanel(condition = "input.allocation != '' && input.panelnames != ''",
                                         helpText("Once you've finished setting the allocation for points, click the button below to store the design scheme"),
                                         actionButton("allocated", "Update point allocation"),
                                         helpText("After updating the design scheme, click the 'Fetch points!' button on the left to draw your points.")
                        )
                 ),
                 conditionalPanel(condition = "input.allocation != ''",
                                  textInput(inputId = "panelnames",
                                            label = "Names of the panels, separated by commas",
                                            value = ""#,
                                            #placeholder = "2018, 2019, 2020"),
                                  ),
                                  helpText("Enter the number of points to draw."),
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
        
        tabPanel(title = "View Points",
                 helpText("Selected sample points"),
                 tableOutput(outputId = "pointdata")
        ),
        tabPanel("About",
                 includeHTML("about.html"))
      )
    )
  )
)
