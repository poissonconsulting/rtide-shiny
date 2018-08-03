mapUI <- function(id, label = 'map') {
  ns <- NS(id)
  
  fluidRow(
    absolutePanel(leafletOutput(ns('leaflet')), top = 0, left = 0, 
                  right = 0, bottom = 0, height = 'auto'),
    absolutePanel(id = ns("search-panel"), class = "panel panel-default", 
                  draggable = TRUE, top = 60, right = 'auto', left = 60, bottom = 'auto',
                  width = 250, height = 36,
                  selectizeInput(inputId = ns("search-site"), label = NULL, 
                                 choices = c("", sites$Station),
                                 selected = "",
                                 options = list(
                                   placeholder = 'Search stations...')
                                 )),
    
    bsModal(ns('modal'), title = NULL, trigger = 'click2', size = "large",
            div(id = ns("top_row"),
                                  
                              h4(textOutput(ns('station-title'))),

                              div(id = ns('date-range'),
                                  fluidRow(
                                    column(4,
                                           dateInput(ns("from"), "From:", format = "M d, yyyy")
                                    ),
                                    column(4,
                                           dateInput(ns("to"), "To:", format = "M d, yyyy", value = Sys.Date() + 2)
                                    ),
                                    column(2,
                                           numericInput(ns("interval"), "Interval (minutes):", 
                                                        value = 10, min = 0, max = 60, step = 5)
                                    ),
                                    column(2,
                                           selectInput(ns("units"), label = "Units:", 
                                                       choices = c("meters", "feet"), selected = "meters")))),
                              tabsetPanel(
                                tabPanel(title = "Plot",
                                         br(),
                                         dygraphOutput(ns("tide-plot"), height = "375px")),
                                tabPanel(title = "Table",
                                         br(),
                                         DT::dataTableOutput(ns('tide-table')))
                                # tabPanel(title = "Daily Low/High",
                                #          br(),
                                #          DT::dataTableOutput('daily_table'))),
                              ),
                downloadButton(outputId = ns("download"), label = "Download data (csv)", class = 'small-dl')))
            )
}