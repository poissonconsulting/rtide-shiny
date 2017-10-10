fluidPage(
  useShinyjs(),
  tags$head(
    # Include custom CSS
    includeCSS("style.css")
  ),
  
  fluidRow(
    column(12,
           tags$style(type = "text/css", "#map {height: calc(100vh - 15px) !important;}"),
           leafletOutput(outputId = 'map')
           
    ),
    tags$head(tags$style(
      HTML('
           #search_panel {background-color: rgba(0,0,0,0);}')
      )),
    absolutePanel(id = "search_panel", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 9, right = 'auto', left = 70, bottom = 'auto',
                  width = 250, height = 20,
      selectizeInput(inputId = "search_site", label = NULL, choices = sites$Station,
                     options = list(
                       placeholder = 'Search stations...',
                       onInitialize = I('function() { this.setValue(""); }')
                     ))),
    
    conditionalPanel(condition = 'input.map_marker_click || input.search_site',
                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                   draggable = TRUE, top = 8, right = 25, left = 'auto', bottom = 20,
                                   width = 420, height = "auto",
                                   br(),
                                   downloadButton(outputId = "download", label = ""),
                                   h4(textOutput('station_title')),                                         

                                   div(id = 'date_range',
                                       fluidRow(
                                         column(4,
                                                dateInput("from", "From:")
                                         ),
                                         column(4,
                                                dateInput("to", "To")
                                         ),
                                         column(4,
                                                numericInput("interval", "Interval (minutes):", value = 10, min = 0, max = 60, step = 5)
                                         ))),
                                   
                                   plotlyOutput("tide_plot", height = 250),
                                   br(), br(),
                                   DT::dataTableOutput('tide_table'),
                                   style = "overflow-x:scroll"
                                   
                     )
    )
    
  )
)


