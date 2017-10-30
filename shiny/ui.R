fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
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
                draggable = F, top = 10, right = 25, left = 'auto', bottom = 20,
                width = 430, height = "auto",
                br(),
                div(id = "top_row",
                    fluidRow(
                      column(2,
                             conditionalPanel(condition = "input.tabselected == 1", uiOutput("download1")),
                             conditionalPanel(condition = "input.tabselected == 2", uiOutput("download2")),
                             conditionalPanel(condition = "input.tabselected == 3", uiOutput("download3"))
                      ),
                             
                      column(7, 
                             shinyWidgets::switchInput("units", label = "Units", onLabel = "m", 
                                                       offLabel = "ft", value = T, size = "mini")),
                      column(3,
                             actionLink("feedback", label = "Feedback?"))
                    )),
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
                tabsetPanel(id = "tabselected",
                  tabPanel(title = "Plot", value = 1,
                           br(),
                           dygraphOutput("tide_plot", height = "375px")),
                  tabPanel(title = "Table", value = 2,
                           br(),
                           DT::dataTableOutput('tide_table')),
                  tabPanel(title = "Daily Low/High", value = 3,
                           br(),
                           DT::dataTableOutput('daily_table'))),
                style = "overflow-x:scroll"
  )
  
)



