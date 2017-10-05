fluidPage(
  useShinyjs(),
  tags$head(tags$style(
    HTML('#input_date_control {background-color: rgba(0,0,0,0.3);;}
             #sel_date {background-color: rgba(0,0,0,1);}'))),
  # theme = "yeti.css",
  fluidRow(
    column(12,
             tags$style(type = "text/css", "#map {height: calc(100vh - 15px) !important;}"),
             leafletOutput(outputId = 'map')
    ),
    bsCollapse(id = "tide_plot", open = "",
               bsCollapsePanel("plot1", style = "info"))
  )

  )


# fluidRow(
#   column(12,
#          conditionalPanel(condition = 'input.map_marker_click',
#                           bottom = '50%', width = '100%', height = '20%',
#                           # style="padding-left: 10px; padding-right: 8px; padding-top: 8px; padding-bottom: 8px",
#                           h4(textOutput("click_text")))
#          
#   )