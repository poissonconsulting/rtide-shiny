mapUI <- function(id, label = 'map') {
  ns <- NS(id)
  
  fluidRow(
    absolutePanel(leafletOutput(ns('leaflet')), top = 0, left = 0, 
                  right = 0, bottom = 0, height = 'auto'))
  
}