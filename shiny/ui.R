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
    )
  )
  # absolutePanel(
  #   id = "input_date_control",
  #   draggable = T, top = '15%', left = '3%', right = 'auto', 
  #   bottom = 'auto', width = '10%', height = '7%')
)
