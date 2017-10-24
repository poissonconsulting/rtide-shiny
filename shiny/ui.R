fluidPage(
  useShinyjs(),
  tags$head(
    # Include custom CSS
    includeCSS("style.css")
  ),
  
  tags$style(type = "text/css", "#map {height: calc(100vh - 15px) !important;}"),
  leafletOutput(outputId = 'map'),
  
  tags$head(tags$style(
    HTML('
         #search_panel {background-color: rgba(0,0,0,0);}')
    )),
  
  absolutePanel(id = "search_panel", class = "panel panel-default", 
                draggable = TRUE, top = 9, right = 'auto', left = 70, bottom = 'auto',
                width = 250, height = 20,
                selectizeInput(inputId = "search_site", label = NULL, choices = sites$Station,
                               options = list(
                                 placeholder = 'Search stations...',
                                 onInitialize = I('function() { this.setValue(""); }')
                               ))),
  
  # conditionalPanel(condition = 'input.map_marker_click || input.search_site',
                   
                   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                 draggable = T, top = 10, right = 25, left = 'auto', bottom = 20,
                                 width = 430, height = "auto",
                                 # HTML('<button data-toggle="collapse" data-target="#view">Collapsible</button>'),
                                 # div(id = 'view',  class = "collapse in",
                                          br(),
                                          # div(id = 'top_row',
                                          #     fluidRow(
                                          #       column(2,
                                          #              downloadButton(outputId = "download", label = "")
                                          #       ),
                                          #       column(4,
                                          #              materialSwitch(inputId = "table_display", label = "Feet", inline = T)),
                                          #       column(5,
                                          #              materialSwitch(inputId = "unit_conversion", label  = "Low/high", inline = T))
                                          #       )),
                                 downloadButton(outputId = "download", label = "csv"),
                                 
                                          h4(textOutput('station_title')),
                                          
                                          div(id = 'date_range',
                                              fluidRow(
                                                column(4,
                                                       dateInput("from", "From:", format = "M d, yyyy")
                                                ),
                                                column(4,
                                                       dateInput("to", "To:", format = "M d, yyyy", value = Sys.Date() + 7)
                                                ),
                                                column(4,
                                                       numericInput("interval", "Interval (minutes):", value = 10, min = 0, max = 60, step = 5)
                                                ))),
                                          
                                          # plotlyOutput("tide_plot", height = 250),
                                          #metricsgraphicsOutput("tide_plot"),
                                          tabsetPanel(
                                            tabPanel(title = "Plot",
                                                     br(),
                                                     # fluidRow(
                                                     #   column(2,
                                                     #          downloadButton(outputId = "download_plot", label = "")),
                                                     #   column(8,
                                                     #          radioButtons('units_plot', label = "", choices = c("Meters", "Feet"),
                                                     #                       selected = "Meters", inline = T)
                                                     #   )),
                                                     # br(),  br(),
                                                     dygraphOutput("tide_plot")),
                                            tabPanel(title = "Table",
                                                     br(),
                                                     # fluidRow(
                                                     #   column(2,
                                                     #          downloadButton(outputId = "download_table", label = "")),
                                                     #   column(8,
                                                     #          radioButtons('units_table', label = "", choices = c("Meters", "Feet"),
                                                     #                       selected = "Meters", inline = T)
                                                     #   )),
                                                     # br(),br(),
                                                     DT::dataTableOutput('tide_table')),
                                            tabPanel(title = "Daily Low/High",
                                                     br(),
                                                     # fluidRow(
                                                     #   column(2,
                                                     #          downloadButton(outputId = "download_daily", label = "")),
                                                     #   column(8,
                                                     #          radioButtons('units_daily', label = "", choices = c("Meters", "Feet"),
                                                     #                       selected = "Meters", inline = T)
                                                     #   )),
                                                     # br(),br(),
                                                     DT::dataTableOutput('daily_table'))),
                                          style = "overflow-x:scroll"
                                 )
                   )
                   
  

