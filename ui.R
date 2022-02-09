
library(shiny)
library(dplyr)
library(rIMPD)
library(glue)
library(leaflet)
library(sf)
library(lubridate)
library(shinyjs)
library(shinyWidgets)

impd_meta <- read.csv("./data/impd_meta.csv") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)

# Define UI

ui <- navbarPage(
    title = "TreeFire",
    id = "navbar",
    # Intro tab ----
    tabPanel(title = "Introduction", value = "tab0",
             mainPanel(
               includeMarkdown("docs/text_intro.rmd")
             )
    ),
    # 1st tab -- Search IMPD ----
    tabPanel(title = "Search the IMPD", value="tab1",
             sidebarLayout(
                 sidebarPanel(
                     useShinyjs(),
                     id = "impd_search_panel",
                     pickerInput(
                         inputId = "location",
                         label = h5("Country or State/Province"),
                         choices = c("", "Canada", "Mexico",
                                     "United States of America",
                                     get_search_params("location")$state
                                     ),
                         multiple = FALSE,
                         options = list(
                             `actions-box` = TRUE,
                             size = 10,
                             `selected-text-format` = "count > 3"
                         )),
                     fileInput(inputId = "filemap",
                               label = h5("Upload a polygon file"),
                               accept=c('kml', '.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg'),
                               multiple=TRUE,
                               width = '100%'
                               ),
                     pickerInput(
                         inputId = "species",
                         label = h5("Species"),
                         choices = "",
                         multiple = FALSE,
                         options = list(
                             `actions-box` = TRUE,
                             size = 10,
                             `selected-text-format` = "count > 3"
                         )),
                     pickerInput(inputId = "investigator",
                                 label = h5("Investigator"),
                                 choices = c("", get_search_params("investigators")),
                                 multiple = FALSE,
                                 options = list(
                                     `actions-box` = TRUE,
                                     size = 10,
                                     `selected-text-format` = "count > 3"
                                 )),

                     fluidRow(
                         div(style="display: inline-block;vertical-align:top; width: 75px;", h5("Year range")),
                         div(style="display: inline-block;vertical-align:top; width: 75px;",
                             textInput(
                                 inputId = "firstYear",
                                 label = NULL,
                                 value = "",
                                 width = NULL
                             )),
                         div(style="display: inline-block;vertical-align:top; width: 10px;", h5("to")),
                         div(style="display: inline-block;vertical-align:top; width: 75px;",
                             textInput(
                                 inputId = "lastYear",
                                 label = NULL,
                                 value = "",
                                 width = NULL
                             )),
                     ),
                     sliderInput(
                         inputId = "elevation",
                         label = h5("Elevation range (in masl)"),
                         min = 0,
                         max = ceiling(max(impd_meta$elevation, na.rm=TRUE)),
                         value = c(0,
                                   ceiling(max(impd_meta$elevation, na.rm=TRUE))),
                         step = 100,
                         round = -2),
                     sliderInput(
                         inputId = "latitude",
                         label = h5("Latitudinal range"),
                         min = 10,
                         max = 65,
                         value = c(18, 52),
                         step = .25,
                         round = 2),
                     sliderInput(
                         inputId = "longitude",
                         label = h5("Longitudinal range"),
                         min = -125,
                         max = -75,
                         value = c(-125, -75),
                         step = .25,
                         round = 2),
                     fluidRow(
                         align = "center",
                         column(6,
                                actionButton("search_button", "Search")
                         ),
                         column(6,
                                actionButton("reset_impd_search", "Reset inputs")
                         )
                     ),
                     checkboxInput(inputId="useDemoData", label="Or use example data",
                                   value=FALSE),
                 ),
                 # Main panel, tab1
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Map",
                                  textOutput("meta_n"),
                                  br(),
                                  br(),
                                  leafletOutput("impd_map")),
                         tabPanel("Table",
                                  downloadButton("downloadData", "Download table as .csv"),
                                  br(),
                                  br(),
                                  dataTableOutput("meta_tbl"))
                     )
                 )
             )
    ),
    # 2nd page -- Get FHX data ----
    tabPanel(title = "Retrieve FHX files", value = "tab2",
             fluidPage(
                 fluidRow(
                     column(6,
                            pickerInput(inputId = "sitePicker",
                                        label = "Select sites to import",
                                        choices = "",
                                        multiple = TRUE,
                                        options = list(
                                            `actions-box` = TRUE,
                                            size = 10,
                                            `selected-text-format` = "count > 3"
                                        ))
                     ),
                 ),
                 fluidRow(
                     column(6,
                            actionBttn(
                                inputId = "get_fhx",
                                label = "Retrieve selected fire history site files from IMPD"
                            )
                     )
                 ),
                 br(),
                 br(),
                 fluidRow(
                     column(6,
                            h4("Imported fire history site files"),
                            tableOutput("in_file_FHX")
                            )
                 ),
                 fluidRow(
                     column(6,
                            downloadButton("downloadFHX", "Download FHX files")
                            )
                 )
             ) # end-fluidpage
    ),
    # 3rd page -- Fire history graphics ----
    tabPanel(title = "Graphics", value = "tab3",
             sidebarLayout(
                     sidebarPanel(
                         useShinyjs(),
                         id = "fire_graph_control_panel",
                         # Choose sites to plot
                         pickerInput(inputId = "firePlotPicker",
                                     label = "Sites to graph",
                                     choices = "",
                                     multiple = TRUE,
                                     options = list(
                                         `actions-box` = TRUE,
                                         size = 10,
                                         `selected-text-format` = "count > 3"
                                     )),
                         # Choose facet plot
                         radioButtons(inputId = "facetPlot",
                                       label = "Plot all sites togther or in separate panels?",
                                       choices = c("One panel" = "single_plot",
                                                   "Multi-panel" = "facet_plot"),
                                       selected = "single_plot"),
                         # Remove y-axis labels
                         checkboxInput(inputId="plot_removeY",
                                       label="Remove tree ID labels",
                                       value=FALSE),
                         # Resort series
                         fluidRow(
                             column(6,
                                    radioButtons(inputId = "plot_sorting",
                                                 label = "Re-order trees by",
                                                 choiceNames = c("First year",
                                                                 "Last year"),
                                                 choiceValues = c("first_year",
                                                                  "last_year"),
                                                 selected = character(0))
                             ),
                             column(6,
                                    checkboxInput(inputId = "sort_decrease",
                                                  label = "Decreasing order",
                                                  value = FALSE)
                             )
                         ),
                         # Add composite
                         checkboxInput(inputId = "plot_composite_on",
                                       label = "Add plot composite",
                                       value = FALSE),
                         # Show legend?
                         checkboxInput(inputId="plot_legend",
                                       label="Show plot legend",
                                       value=FALSE),
                         # X-axis range
                         sliderInput(
                             inputId = "plot_yr_range",
                             label = "Year range (X-axis)",
                             min = 1600,
                             max = 2000,
                             value = c(1600, 2000),
                             step = 10,
                             round = 10,
                             timeFormat = '%Y')
                     ),
                 mainPanel(
                     plotOutput("fire_graphics",
                                height = "800px")
                 )
             )
    )

    # end tabs ----
)
