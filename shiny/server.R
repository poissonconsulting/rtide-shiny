
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
      addResetMapButton() %>%
      leaflet.extras::addSearchFeatures(targetGroups = 'sites',
                                        options = searchFeaturesOptions(
                                          zoom = 10, openPopup = TRUE, firstTipSubmit = TRUE,
                                          autoCollapse = TRUE, hideMarkerOnCollapse = TRUE))
    })
  })
  
}



