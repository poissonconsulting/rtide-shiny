
# change max file size upload to 10mb
options(shiny.maxRequestSize = 10*1024^2)

function(input, output, session) {
  
  session$onFlushed(once = T, function() {
    output$map <- leaflet::renderLeaflet({
      leaflet() %>%
        
        setView(lat = initial_lat, lng = initial_long, zoom = initial_zoom) %>%
        
        addEasyButton(easyButton(icon = "ion-arrow-shrink",
                                 title = "Reset View", onClick = JS("function(btn, map){ map.setView(new L.LatLng(40.6157777, -127.311505), 3, { animation: true });}"))) %>%

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
  
  ### when map click or search site
  location <- reactiveValues(loc = NULL)
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    loc <- c(click$lng, click$lat)           
    location$loc <- loc})
  
  observeEvent(input$search_site, {
    station <- input$search_site
    lng <- filter(sites, Station == station)$X
    lat <- filter(sites, Station == station)$Y
    loc <- c(lng, lat)       
    location$loc <- loc})
  
  # update station label in panel
  stationLabel <- reactive({
    loc <- location$loc
    label <- filter(sites, X == loc[1] & Y == loc[2])$Station
    print(label)
  })
  
  # create plotly graph
  filterData <- reactive({
    loc <- location$loc
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
    req(location$loc)
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
  
  dailyData <- reactive({
    data <- tideData()
    
    high <- data[find_peaks(data$TideHeight, m = 3), c("DateTime","TideHeight")] %>%
      setNames(c("DateTime", "Height"))
    
    low <- data[find_peaks(-data$TideHeight, m = 3), c("DateTime","TideHeight")] %>%
      setNames(c("DateTime", "Height"))
    
    daily <- rbind(high, low)
    
  })
  
  # metrics
  # tidePlot <- reactive({
  #  dat <- tideData()
  # 
  #  gp <- mjs_plot(data = dat, x = DateTime, y = TideHeight, height = 10, left = 10, top = 0) %>%
  #    mjs_line() %>%
  #    mjs_labs(y = "Tide Height (m)")
  #  gp
  # })
  
  # dygraph
  tidePlot <- reactive({
    dat <- tideData()
    # if(input$unit_conversion == T){
    #   dat %<>% mutate(TideHeight = TideHeight * 2.2)
    #   dat
    # } else dat <- dat

    pad <- (max(dat$TideHeight) - min(dat$TideHeight))/7

    dat %<>% select(`Tide Height` = TideHeight, `Date-Time` = DateTime)
    xtsdat <- xts::xts(dat, order.by = dat$`Date-Time`)

    dygraph(xtsdat, height = "10px") %>%
      dyOptions(strokeWidth = 1.5, drawGrid = F, includeZero = F,
                useDataTimezone = T, drawGapEdgePoints = T, rightGap = 0) %>%
      dyRangeSelector() %>%
      dyAxis("y", valueRange = c(min(dat$`Tide Height`) - pad, max(dat$`Tide Height`) + pad),
             label = "Tide Height (m)")

  })
  
  # # highcharts
  # tidePlot <- reactive({
  #   dat <- tideData() 
  #   
  #   pad <- (max(dat$TideHeight) - min(dat$TideHeight))/7
  #   
  #   # dat %<>% select(`Tide Height` = TideHeight, `Date-Time` = DateTime)
  #   
  #   hc <- highchart() %>%
  #     hc_add_series_df(data = dat, type = "line", aes(x  = DateTime, y = TideHeight))
  #   
  # })
  
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
      dplyr::select(Date, Time, `Height (m)` = TideHeight)
    
    data
  })
  
  
  dailyTable <- reactive({
    data <- dailyData() 
    
    data %<>% mutate(Year = lubridate::year(DateTime),
                     Month = lubridate::month(DateTime, label = T, abbr = T),
                     Day = lubridate::day(DateTime),
                     Time = lapply(strsplit(as.character(DateTime), " "), "[", 2) %>% unlist(),
                     Height = round(Height, 2)) %>%
      mutate(Date = paste0(Month, " ", Day, ", ", Year)) %>%
      arrange(Date, Time) %>%
      group_by(Date, Height) %>%
      slice(1) %>%
      select(Date, Time, `Height (m)` = Height) %>%
      arrange(Date, Time)
      
    data
  })
  
  ### when click on map or search site
  observeEvent(c(input$map_marker_click, input$search_site),
               {leafletProxy('map') %>%
                   setView(lat = location$loc[2], lng = location$loc[1], zoom = click_zoom)})
  
  observeEvent(c(input$map_marker_click, input$search_site),
               {output$station_title <- renderText({stationLabel()})})
  
  observeEvent(c(input$map_marker_click, input$search_site),
               {output$tide_plot <- renderDygraph({
                 tidePlot()
               })})
  
  observeEvent(c(input$map_marker_click, input$search_site),
               {output$tide_table <- DT::renderDataTable({
                 tideTable()
               })})
  
  observeEvent(c(input$map_marker_click, input$search_site),
               {output$daily_table <- DT::renderDataTable({
                 dailyTable()
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



