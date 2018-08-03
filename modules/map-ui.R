mapUI <- function(id, label = 'map') {
  ns <- NS(id)
  
  fluidRow(
    absolutePanel(leafletOutput(ns('leaflet')), top = 0, left = 0, 
                  right = 0, bottom = 0, height = 'auto'),
    absolutePanel(id = ns("searchPanel"), class = "panel panel-default", 
                  draggable = TRUE, top = 60, right = 'auto', left = 60, bottom = 'auto',
                  width = 250, height = 36,
                  selectizeInput(inputId = ns("search"), label = NULL, 
                                 choices = c("", sites$Station),
                                 selected = "",
                                 options = list(
                                   placeholder = 'Search stations...')
                                 )),
    uiOutput(ns('uiModal')))
    
    
}