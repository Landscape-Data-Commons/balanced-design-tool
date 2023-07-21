library(shiny)

fluidPage(
  title = "Balanced Design Tool",
  shinyjs::useShinyjs(),
  tags$head(
    # Use the styles.css file for (nearly) all our styling needs
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # A function that lets us create links to tabs since there's no
    # equivalent to updateTabsetPanel() like updateTabPanel() for some reason.
    # This lets us make links with a(onclick = "tabJump('Tab Name')")
    # This comes from StackOverflow, I think?
    tags$script(HTML('
        var tabJump = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      '))
  ),
  
  navbarPage(
    title = tags$div(class = "tool-title",
                     "Balanced Design Tool"),
    id = "navbar-full",
    position = "static-top",
    footer = tags$div(class = "footer",
                      p(column(width = 4,
                               p(a(href = 'mailto:nelson.stauffer@usda.gov',
                                   'Contact us with questions',
                                   target = "_blank"))),
                        column(width = 8,
                               align = "right",
                               p(img(src = "combined_logos_hires.png",
                                     width = "60%"))))),
    tabPanel(title = "Configuration",
             sidebarLayout(sidebarPanel = sidebarPanel(
               HTML(
                 "<div class = 'app-info'>
                    <h3>About</h3>
                    This tool enables users to draw spatially-balanced, statistically valid sampling designs within an uploaded area, with or without stratification. It leverages the Generalized Random Tessellation Stratified (GRTS) algorithm as implemented in the <a href='https://cran.r-project.org/web/packages/spsurvey/'>spsurvey R package</a>.
                    <br>
                 </div>"
               ),
               tabsetPanel(
                 id = "start_sidebar",
                 tabPanel(title = "Polygon Setup",
                          br(),
                          fluidRow(column(width = 10,
                                          fileInput(inputId = "polygons",
                                                    label = "Upload a polygon shapefile in a .ZIP",
                                                    multiple = FALSE,
                                                    accept = c("application/zip"))),
                                   column(width = 1,
                                          actionButton(inputId = "polygons_info",
                                                       label = "",
                                                       class = "info-btn",
                                                       icon = icon("circle-question")))),
                          fluidRow(column(width = 10,
                                          checkboxInput(inputId = "repair_polygons",
                                                        label = "Check shapefile geometry and attempt to repair if necessary",
                                                        value = FALSE)),
                                   column(width = 1)),
                          br(),
                          conditionalPanel(condition = "input.projname != ''",
                                           fluidRow(column(width = 10,
                                                           textInput(inputId = "projname",
                                                                     label = "Project name",
                                                                     value = "")),
                                                    column(width = 1,
                                                           actionButton(inputId = "projname_info",
                                                                        label = "",
                                                                        class = "info-btn",
                                                                        icon = icon("circle-question")))),
                                           fluidRow(column(width = 10,
                                                           selectInput(inputId = "polygons_layer",
                                                                       label = "Polygon name",
                                                                       choices = c(""),
                                                                       selected = "")),
                                                    column(width = 1,
                                                           actionButton(inputId = "polygons_layer_info",
                                                                        label = "",
                                                                        class = "info-btn",
                                                                        icon = icon("circle-question")))),
                                           fluidRow(column(width = 10,
                                                           selectInput(inputId = "strataname",
                                                                       label = "Stratification variable",
                                                                       choices = c(""))),
                                                    column(width = 1,
                                                           actionButton(inputId = "strataname_info",
                                                                        label = "",
                                                                        class = "info-btn",
                                                                        icon = icon("circle-question"))))),
                          uiOutput(outputId = "polygons_done_ui")
                 ),
                 tabPanel(title = "Point Allocation",
                          uiOutput(outputId = "no_polygons_yet_ui"),
                          conditionalPanel(condition = "input.projname != ''",
                                           br(),
                                           fluidRow(column(width = 10,
                                                           selectInput(inputId = "allocation",
                                                                       label = "Point allocation approach",
                                                                       choices = c(""),
                                                                       selected = "")),
                                                    column(width = 1,
                                                           actionButton(inputId = "allocation_info",
                                                                        label = "",
                                                                        class = "info-btn",
                                                                        icon = icon("circle-question")))),
                                           fluidRow(column(width = 10,
                                                           numericInput(inputId = "seednum",
                                                                        label = "Seed number",
                                                                        value = 46290,
                                                                        min = 0)),
                                                    column(width = 1,
                                                           actionButton(inputId = "seednum_info",
                                                                        label = "",
                                                                        class = "info-btn",
                                                                        icon = icon("circle-question")))),
                                           conditionalPanel(condition = "input.allocation != ''",
                                                            fluidRow(column(width = 10,
                                                                            textInput(inputId = "panelnames",
                                                                                      label = "Panel names (optional)",
                                                                                      value = "")),
                                                                     column(width = 1,
                                                                            actionButton(inputId = "panelnames_info",
                                                                                         label = "",
                                                                                         class = "info-btn",
                                                                                         icon = icon("circle-question")))),
                                                            uiOutput("basecount"),
                                                            uiOutput("minimumbase"),
                                                            uiOutput("oversamplemin"),
                                                            conditionalPanel(condition = "input.allocation == 'Manually'",
                                                                             br(),
                                                                             fluidRow(column(width = 5,
                                                                                             HTML("<div class=allocation-column-label>Number of base points</div>")),
                                                                                      column(width = 5,
                                                                                             HTML("<div class=allocation-column-label>Number of oversample points</div>")),
                                                                                      column(width = 1,
                                                                                             actionButton(inputId = "manual_allocation_info",
                                                                                                          label = "",
                                                                                                          class = "info-btn",
                                                                                                          icon = icon("circle-question")))),
                                                                             # This creates two columns where
                                                                             # one contains all the procedurally
                                                                             # generated inputs with the tag
                                                                             # "mabase" and the other with
                                                                             # the tag "maover"
                                                                             fluidRow(column(width = 5,
                                                                                             hr(),
                                                                                             tags$div(id = "mabase")),
                                                                                      column(width = 5,
                                                                                             hr(),
                                                                                             tags$div(id = "maover")))
                                                            )#,
                                                            # fluidRow(column(width = 12,
                                                            #                 align = "center",
                                                            #                 br(),
                                                            #                 actionButton(inputId = "allocated",
                                                            #                              label = "Update point allocation")))
                                           ),
                                           conditionalPanel(
                                             condition = "!($('html').hasClass('shiny-busy'))",
                                             uiOutput(outputId = "configuration_done_ui")
                                           )
                                           
                          )
                 ),
                 conditionalPanel(condition = "input.allocation != ''",
                                  fluidRow(column(width = 12,
                                                  align = "center",
                                                  br(),
                                                  actionButton(inputId = "fetch",
                                                               label = "Fetch points!"))),
                 ),
                 conditionalPanel(
                   condition = "$('html').hasClass('shiny-busy')",
                   br(),
                   HTML(
                     "<div class = 'load-message'><img src = 'busy_icon_complex.svg' height = '40rem'>Working! Please wait.<img src = 'busy_icon_complex.svg' height = '40rem'></div>"
                   )
                 ),
                 fluidRow(column(width = 12,
                                 align = "center",
                                 br(),
                                 actionButton(inputId = "reset_button",
                                              label = "Reset this tool")
                 )
                 )
               )
             ),
             mainPanel = mainPanel(
               leaflet::leafletOutput(outputId = "basemap",
                                      height = "80vh")#,
               # includeHTML("instructions.html")
             )
             )
    ),
    tabPanel(title = "Design",
             sidebarLayout(sidebarPanel = sidebarPanel(
               uiOutput("design_tab_notyet_ui"),
               fluidRow(column(width = 3,
                               align = "center",
                               uiOutput(outputId = "download_button_ui"))),
               uiOutput("design_tab_summarytable_ui")
             ),
             mainPanel = mainPanel(
               fluidRow(column(width = 10,
                               align = "center",
                               uiOutput("design_tab_pointmap_ui")))
             ))
    ),
    tabPanel(title = "Glossary",
             fluidRow(column(width = 10,
                             offset = 1,
                             # includeHTML("about.html"),
                             tags$div(class = "main-panel-body",
                                      includeHTML("glossary.html"))
             )
             )
    )
  )
)