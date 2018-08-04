mapUI <- function(id, label = 'map') {
  ns <- NS(id)
  
  absolutePanel(
    top = "auto", left = "auto", right = 20, bottom = 20,
    width = "auto", height = "auto",
    actionButton(ns("map_zoom_in"), "+"),
    actionButton(ns("map_zoom_out"), "-")
  )
  
  fluidRow(
    splitLayout(cellWidths = c("1%", "99%"),
      div(),
      selectizeInput(inputId = ns("search"), label = NULL, 
                   choices = c("", sites$Station),
                   selected = "",
                   options = list(
                     placeholder = 'Search stations or click marker to begin...'),
                   width = 300)),
    absolutePanel(leafletOutput(ns('leaflet')), top = 0, left = 0, 
                  right = 0, bottom = 0, height = 'auto'),
    # absolutePanel(id = ns("searchPanel"), class = "panel panel-default", 
    #               draggable = TRUE, top = 60, right = 'auto', left = 60, bottom = 'auto',
    #               width = 250, height = 'auto'
    #               ),
                  # actionLink(ns('zoom'), "Zoom to station")),
    uiOutput(ns('uiModal')))
    
    
}