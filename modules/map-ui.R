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
      absolutePanel(class = 'zoomPanel',
                    top = 60, left = 10, right = 'auto', bottom = 'auto',
                    width = "auto", height = "auto",
                    div(id = "divSearch",
                      inline(selectizeInput(inputId = ns("search"), label = NULL, 
                                    choices = c("", sites$Station),
                                    selected = "",
                                    options = list(
                                      placeholder = 'Search stations or click marker to begin...'),
                                    width = 300)),
                    inline(actionButton(ns("zoom_to"), label = NULL, icon  = icon('search'))),
                    bsTooltip(ns("zoom_to"), "Zoom to location", placement = "bottom", trigger = "hover",
                              options = NULL)))
      ),
    
   
    absolutePanel(leafletOutput(ns('leaflet')), top = 0, left = 0,
                  right = 0, bottom = 0, height = 'auto'),

    uiOutput(ns('uiModal')))
    
    
}