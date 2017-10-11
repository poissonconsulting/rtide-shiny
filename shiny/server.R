
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
  
  ### reactive funcitons when map click
  # update setView on site click
  locationMap <- reactive({
    click <- input$map_marker_click
    loc <- c(click$lng, click$lat)
    loc
  })
  
  # update station label in panel
  stationLabelMap <- reactive({
    click <- input$map_marker_click
    lng <- round(click$lng, 4)
    lat <- round(click$lat, 4)
    label <- filter(sites, X == lng & Y == lat)$Station
    print(label)
  })
  
  # create plotly graph
  filterDataMap <- reactive({
    click <- input$map_marker_click
    lng <- round(click$lng, 4)
    lat <- round(click$lat, 4)
    data <- filter(sites, X == lng & Y == lat)
    data
  })
  
  prettyLabelMap <- reactive({
    station <- filterDataMap()$Station
    label <- strsplit(station, ",")[[1]][1] %>%
      sub(" ", "", .)
    label
  })
  
  tideDataMap <- reactive({
    filt <- filterDataMap()
    label <- filt$Station
    tz <- filt$TZ
    
    data <- rtide::tide_height(
      label, from = input$from, to = input$to, 
      minutes = input$interval, tz = tz)
    
    data$TideHeight %<>% round(2)
    data$TimeZone <- tz

    data
  })
  
  tidePlotMap <- reactive({
    gp <- ggplot(data = tideDataMap(), aes(x = DateTime, y = TideHeight)) + 
      geom_line() + 
      scale_x_datetime(name = "Date") +
      scale_y_continuous(name = "Tide Height (m)") +
      theme_bw() 
    
    gp
  })
  
  tideTableMap <- reactive({
    data <- tideDataMap() %>%
      dplyr::mutate(Date = lapply(strsplit(as.character(DateTime), " "), "[", 1) %>% unlist()) %>%
      dplyr::mutate(Time = lapply(strsplit(as.character(DateTime), " "), "[", 2) %>% unlist()) %>%
      dplyr::select(Date, Time, `Tide Height (m)` = TideHeight)
    
    data
  })
  
  ### reactive funcitons when select station
  # update setView on site click
  locationSite <- reactive({
    station <- input$search_site
    lng <- filter(sites, Station == station)$X
    lat <- filter(sites, Station == station)$Y
    loc <- c(lng, lat)
    loc
  })
  
  # update station label in panel
  stationLabelSite <- reactive({
    label <- input$search_site
    label
  })
  
  # create plotly graph
  filterDataSite <- reactive({
    station <- input$search_site
    data <- filter(sites, Station == station)
    data
  })
  
  prettyLabelSite <- reactive({
    station <- input$search_site
    label <- strsplit(station, ",")[[1]][1] %>%
      sub(" ", "", .)
    label
  })
  
  tideDataSite <- reactive({
    filt <- filterDataSite()
    label <- filt$Station
    tz <- filt$TZ
    
    data <- rtide::tide_height(
      label, from = input$from, to = input$to, 
      minutes = input$interval, tz = tz)
    
    data$TideHeight %<>% round(2)
    data$TimeZone <- tz
    
    data
  })
  
  tidePlotSite <- reactive({
    gp <- ggplot(data = tideDataSite(), aes(x = DateTime, y = TideHeight)) + 
      geom_line() + 
      scale_x_datetime(name = "Date") +
      scale_y_continuous(name = "Tide Height (m)") +
      theme_bw() 
    
    gp
  })
  
  tideTableSite <- reactive({
    data <- tideDataSite() %>%
      dplyr::mutate(Year = lubridate::year(DateTime),
                    Month = lubridate::month(DateTime, label = T, abbr = T),
                    Day = lubridate::day(DateTime),
                    Date = paste0(Month, " ", Day, ", ", Year)) %>%
      dplyr::mutate(Time = lapply(strsplit(as.character(DateTime), " "), "[", 2) %>% unlist()) %>%
      dplyr::select(Date, Time, `Tide Height (m)` = TideHeight)
    
    data
  })
    
  # observeEvent(input$map_click,
  #              {removeUI(
  #                selector = 'div:has(> #data_panel)'
  #              )})
  
  ### when click on map
  observeEvent(input$map_marker_click,
               {leafletProxy('map') %>%
                   setView(lat = locationMap()[2], lng = locationMap()[1], zoom = click_zoom)})
  
  observeEvent(input$map_marker_click,
               {output$station_title <- renderText({stationLabelMap()})})
  
  observeEvent(input$map_marker_click,
               {output$tide_plot <- renderPlotly({
                 ggplotly(tidePlotMap())
               })})
  
  observeEvent(input$map_marker_click,
               {output$tide_table <- DT::renderDataTable({
                 tideTableMap()
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
  ### when search site
  observeEvent(input$search_site,
               {leafletProxy('map') %>%
                   setView(lat = locationSite()[2], lng = locationSite()[1], zoom = click_zoom)})

  observeEvent(input$search_site,
               {output$station_title <- renderText({stationLabelSite()})})

  observeEvent(input$search_site,
               {output$tide_plot <- renderPlotly({
                 ggplotly(tidePlotSite())
               })})

  observeEvent(input$search_site,
               {output$tide_table <- DT::renderDataTable({
                 tideTableSite()
               })})

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



