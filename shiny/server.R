
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
        
        leaflet::addMiniMap(position = "bottomleft",
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
  
  # update setView on site click
  markerLong <- reactive({
    click <- input$map_marker_click
    long <- click$lng
    long
  })
  
  markerLat <- reactive({
    click <- input$map_marker_click
    lat <- click$lat
    lat
  })
  
  observeEvent(input$map_marker_click,
               {leafletProxy('map') %>%
                   setView(lat = markerLat(), lng = markerLong(), zoom = click_zoom)})
  
  # update station label in panel
  stationLabel <- reactive({
    click <- input$map_marker_click
    lng <- round(click$lng, 4)
    lat <- round(click$lat, 4)
    label <- filter(sites, X == lng & Y == lat)$Station
    print(label)
  })
  
  observeEvent(input$map_marker_click,
               {output$station_title <- renderText({stationLabel()})})
  
  # create plotly graph
  filterData <- reactive({
    click <- input$map_marker_click
    lng <- round(click$lng, 4)
    lat <- round(click$lat, 4)
    data <- filter(sites, X == lng & Y == lat)
    data
  })
  
  prettySite <- reactive({
    station <- filterData()$Station
    label <- strsplit(station, ",")[[1]][1] %>%
      sub(" ", "", .)
    label
  })
  
  tideData <- reactive({
    filt <- filterData()
    label <- filt$Station
    tz <- filt$TZ
    
    data <- rtide::tide_height(
      label, from = input$from, to = input$to, 
      minutes = input$interval, tz = tz)
    
    data$TideHeight %<>% round(2)
    data$TimeZone <- tz

    data
  })
  
  tidePlot <- reactive({
    gp <- ggplot(data = tideData(), aes(x = DateTime, y = TideHeight)) + 
      geom_line() + 
      scale_x_datetime(name = "Date") +
      scale_y_continuous(name = "Tide Height (m)") +
      theme_bw() 
    
    gp
  })
  
  tideTable <- reactive({
    data <- tideData() %>%
      dplyr::mutate(Date = lapply(strsplit(as.character(DateTime), " "), "[", 1) %>% unlist()) %>%
      dplyr::mutate(Time = lapply(strsplit(as.character(DateTime), " "), "[", 2) %>% unlist()) %>%
      dplyr::select(Date, Time, `Tide Height (m)` = TideHeight)
    
    data
  })
    
  observeEvent(input$map_marker_click,
               {output$tide_plot <- renderPlotly({
                 ggplotly(tidePlot())
               })})
  
  observeEvent(input$map_marker_click,
               {output$tide_table <- DT::renderDataTable({
                 tideTable()
               })})
  
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(prettySite(), "_", fromDate(), "_", toDate(), ".csv")
    },
    content <- function(file) {
      readr::write_csv(tideData(), file)
    }
  )
}



