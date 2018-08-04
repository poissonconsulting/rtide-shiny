mapUI <- function(id, label = 'map') {
  ns <- NS(id)
  
  fluidRow(
    absolutePanel(class = 'zoomPanel',
      top = 60, left = "auto", right = 10, bottom = 'auto',
      width = "auto", height = "auto",
      verticalLayout(actionButton(ns("map_zoom_in"), "+", class = 'small-button', width = 35),
      actionButton(ns("map_zoom_out"), "-", class = "small-button", width = 35))
    ),
    
    fluidRow(
      absolutePanel(
                    top = 60, left = 10, right = 'auto', bottom = 'auto',
                    width = "auto", height = "auto",
                    selectizeInput(inputId = ns("search"), label = NULL, 
                                    choices = c("", sites$Station),
                                    selected = "",
                                    options = list(
                                      placeholder = 'Search stations or click marker to begin...'),
                                    width = 300))
      ),
   
      
    absolutePanel(leafletOutput(ns('leaflet')), top = 0, left = 0,
                  right = 0, bottom = 0, height = 'auto'),
    # absolutePanel(id = ns("searchPanel"), class = "panel panel-default", 
    #               draggable = TRUE, top = 60, right = 'auto', left = 60, bottom = 'auto',
    #               width = 250, height = 'auto'
    #               ),
                  # actionLink(ns('zoom'), "Zoom to station")),
    uiOutput(ns('uiModal')))
    
    
}