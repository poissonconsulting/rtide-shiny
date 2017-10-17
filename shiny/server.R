
# change max file size upload to 10mb
options(shiny.maxRequestSize = 10*1024^2)

function(input, output, session) {
  
  session$onFlushed(once = T, function() {
    output$map <- leaflet::renderLeaflet({
      leaflet() %>%
        
        setView(lat = initial_lat, lng = initial_long, zoom = initial_zoom) %>%
        
        addResetMapButton() %>%
        
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
                            tiles = "CartoDB.Positron") 
    })
  })
  
  ### reactive functions when map click
  # update setView on site click
  location <- reactive({
    if(!is.null(input$map_marker_click)){
      click <- input$map_marker_click
      loc <- c(click$lng, click$lat)
      loc
    } else if(!is.null(input$search_site)){
      station <- input$search_site
      lng <- filter(sites, Station == station)$X
      lat <- filter(sites, Station == station)$Y
      loc <- c(lng, lat)
      loc
    }
    print(loc)
  })
  
  # update station label in panel
  stationLabel <- reactive({
    loc <- location()
    label <- filter(sites, X == loc[1] & Y == loc[2])$Station
    print(label)
  })
  
  # create plotly graph
  filterData <- reactive({
    loc <- location()
    data <- filter(sites, X == round(loc[1], 4) & Y == round(loc[2], 4))
    data
  })
  
  prettyLabel <- reactive({
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
      dplyr::mutate(Year = lubridate::year(DateTime),
                    Month = lubridate::month(DateTime, label = T, abbr = T),
                    Day = lubridate::day(DateTime),
                    Time2 = lapply(strsplit(as.character(DateTime), " "), "[", 2) %>% unlist()) %>%
      dplyr::mutate(Hour = lapply(strsplit(as.character(Time2), ":"), "[", 1) %>% unlist(),
                    Minute = lapply(strsplit(as.character(Time2), ":"), "[", 2) %>% unlist()) %>%
      dplyr::mutate(Time = paste0(Hour, ":", Minute),
                    Date = paste0(Month, " ", Day, ", ", Year)) %>%
      dplyr::select(Date, Time, `Tide Height (m)` = TideHeight)
    
    data
  })
  
  ### when click on map
  observeEvent(input$map_marker_click,
               {leafletProxy('map') %>%
                   setView(lat = location()[2], lng = location()[1], zoom = click_zoom)})
  
  observeEvent(input$map_marker_click,
               {output$station_title <- renderText({stationLabel()})})
  
  observeEvent(input$map_marker_click,
               {output$tide_plot <- renderPlotly({
                 ggplotly(tidePlot())
               })})
  
  observeEvent(input$map_marker_click,
               {output$tide_table <- DT::renderDataTable({
                 tideTable()
               })})
  
  ### when search site
  observeEvent(input$search_site,
               {leafletProxy('map') %>%
                   setView(lat = location()[2], lng = location()[1], zoom = click_zoom)})
  
  observeEvent(input$search_site,
               {output$station_title <- renderText({stationLabel()})})
  
  observeEvent(input$search_site,
               {output$tide_plot <- renderPlotly({
                 ggplotly(tidePlot())
               })})
  
  observeEvent(input$search_site,
               {output$tide_table <- DT::renderDataTable({
                 tideTable()
               })})
  
  #   output$download <- downloadHandler(
  #     filename = function() {
  #       paste0(prettyLabel(), "_", fromDate(), "_", toDate(), ".csv")
  #     },
  #     content <- function(file) {
  #       readr::write_csv(tideData(), file)
  #     }
  #   )
  # }
  
  observe({
    input$reset_view
    leafletProxy("map") %>% setView(lat = initial_lat, lng = initial_long, zoom = initial_zoom)
  })
  
  ### download
  #   output$download <- downloadHandler(
  #     filename = function() {
  #       paste0(prettyLabel(), "_", fromDate(), "_", toDate(), ".csv")
  #     },
  #     content <- function(file) {
  #       readr::write_csv(tideData(), file)
  #     }
  #   )
}



