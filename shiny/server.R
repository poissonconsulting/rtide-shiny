
# change max file size upload to 10mb
options(shiny.maxRequestSize = 10*1024^2)

function(input, output, session) {

  session$onFlushed(once = T, function() {
    output$map <- leaflet::renderLeaflet({
      leaflet() %>%
        
        setView(lat = initial_lat, lng = initial_long, zoom = initial_zoom) %>%
        leaflet::addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 1), group = "Basemap") %>%
        leaflet::addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 1), group = "Satelite") %>%
       
         leaflet::addCircleMarkers(
          data = sites,
          lng = sites$X, lat = sites$Y,
          label = sites$Station,
          color = "blue",
          radius = 5,
          weight = 1,
          opacity = 1,
          fillOpacity = 0.3,
          group = 'sites',
          clusterOptions = markerClusterOptions(showCoverageOnHover = F)) %>%
        
        leaflet::addLayersControl(
          baseGroups = c("Basemap", "Satelite"),
          options = layersControlOptions(collapsed = T),
          position = 'topleft') %>%
        
        leaflet::addMiniMap(position = "topright",
                   zoomLevelOffset = -8,
                   # centerFixed = c(initial_lat, initial_long),
                   toggleDisplay = T,
                   autoToggleDisplay = T,
                   tiles = "CartoDB.Positron") %>%
      
        addResetMapButton() %>%
      
        leaflet.extras::addSearchFeatures(targetGroups = 'sites',
                                        options = searchFeaturesOptions(
                                          zoom = click_zoom, openPopup = TRUE, firstTipSubmit = TRUE,
                                          autoCollapse = TRUE, hideMarkerOnCollapse = TRUE))
    })
  })
  
  ### update setView on site click
  markerLong <- reactive({
    click <- input$map_marker_click
    long <- click$lng
    long
    print(long)
  })

  markerLat <- reactive({
    click <- input$map_marker_click
    lat <- click$lat
    lat
    print(lat)
  })

  observeEvent(input$map_marker_click,
               {leafletProxy('map') %>%
                   setView(lat = markerLat(), lng = markerLong(), zoom = click_zoom)})
  
  observeEvent(input$map_marker_click,
               ({
    updateCollapse(session, "tide_plot", open = "plot1")
  }))
  
  # observeEvent(input$map_marker_click,
  #              {output$click_text <- renderText({markerLat()})})

  
}



