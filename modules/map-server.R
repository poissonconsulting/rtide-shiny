map <- function(input, output, session) {
  ns <- session$ns
  
  ############### --------------- Reactives --------------- ###############
  observe({
    print(station$location)
  })
  
  ### filter data
  filter_data <- reactive({
    sites[which(sites$Station == station$location), ]
  })
  
  ### reactive labels
  station_label <- reactive({
    filter_data()$Station
  })
  
  pretty_label <- reactive({
    strsplit(station_label(), ",")[[1]][1] %>%
      sub(" ", "", .)
  })
  
  unit_label <- reactive({
    if(input$units == "meters") {
      return("Tide Height (m)")
    }
    "Tide Height (ft)"
  })
  
  tide_data <- reactive({
    req(station$location)
    data <- filter_data()
    station <- station_label()
    tz <- data$TZ
    print(tz)
    print(data)
    
    data <- rtide::tide_height(
      station, from = input$from, to = input$to,
      minutes = input$interval, tz = tz)
    
    data$TideHeight %<>% round(2)
    data$TimeZone <- tz
    
    if(input$units == "meters"){
      return(data)
    } 
    data %>% mutate(TideHeight = round(TideHeight * 3.3333, 2))
  })
  
  download_data <- reactive({
    data <- tide_data()
    data$DateTime <- as.character(data$DateTime) 
    if(input$units == "meters"){
      return(data %>% setNames(c("Station", "DateTime", "TideHeight_m", "TimeZone")))
    } 
    data %>% setNames(c("Station", "DateTime", "TideHeight_ft", "TimeZone"))
  })
  
  tide_plot <- reactive({
    data <- tide_data()
    time <- Sys.time()
    time %<>% lubridate::with_tz(tz = data$TimeZone[1])
    
    pad <- (max(data$TideHeight) - min(data$TideHeight))/7
    padrange <- c(min(data$TideHeight) - pad, max(data$TideHeight) + pad)
    
    
    data  <- data[,c('TideHeight', 'DateTime')] %>%
      setNames(c(unit_label(), "Date-Time"))
    xtsdata <- xts::xts(data, order.by = data$`Date-Time`)
    
    dygraph(xtsdata, height = "10px") %>%
      dyOptions(strokeWidth = 1.5, drawGrid = F, includeZero = F,
                useDataTimezone = T, drawGapEdgePoints = T, rightGap = 15) %>%
      dyRangeSelector() %>%
      dyAxis("y", valueRange = padrange,
             label = unit_label()) %>%
      dyEvent(x = time, label = "Current time", labelLoc = "bottom")
  })
  
  tide_table <- reactive({
    data <- tide_data() 
    data$Time <- strftime(data$DateTime, format = "%H:%M %p")
    data$Date <- strftime(data$DateTime, format = "%B %d, %Y")
    data[,c("Date", "Time", "TideHeight")] %>% 
      setNames(c("Date", "Time", unit_label()))
  })
  
  ############### --------------- Reactive Values --------------- ###############
  station <- reactiveValues(location = NULL)
  
  observeEvent(input$leaflet_marker_click, {
    station$location <- input$leaflet_marker_click$id
  })
  
  observeEvent(input$search, {
    station$location <- input$search
  })
  
  ############### --------------- Observers --------------- ###############
  observeEvent(c(input$leaflet_marker_click, input$search), {
    toggleModal(session, "modal", "open")
  })
  
  # zoom to site on click or search
  observeEvent(input$zoom,
               {leafletProxy('leaflet') %>%
                   setView(lat = filter_data()$Y, lng = filter_data()$X, zoom = click_zoom)})
  
  ############### --------------- Leaflet --------------- ###############
  output$leaflet <- leaflet::renderLeaflet({
    leaflet() %>%
      
      setView(lat = initial_lat, lng = initial_long, zoom = initial_zoom) %>%
      
      addEasyButton(easyButton(icon = "ion-arrow-shrink",
                               title = "Reset View", onClick = JS(paste0("function(btn, map){ map.setView(new L.LatLng(", initial_lat, ", ", initial_long, "), ", initial_zoom, ", { animation: true });}")))) %>%
      
      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 1), group = "Satelite") %>%
      addTiles(urlTemplate = mapbox_moon, group = "Basemap") %>%
      
      addLayersControl(
        baseGroups = c("Basemap", "Satelite"),
        options = layersControlOptions(collapsed = TRUE),
        position = leaf.pos) %>%
      leaflet::addMarkers(
        data = sites,
        lng = sites$X, 
        lat = sites$Y,
        label = sites$Station,
        layerId = sites$Station,
        icon = makeIcon(
          iconUrl = "input/marker1.png",
          iconWidth = 30, iconHeight = 30
        ),
        group = 'sites',
        clusterOptions = markerClusterOptions(showCoverageOnHover = F)
      ) 
    # leaflet::addMiniMap(position = "bottomleft",
    #                     zoomLevelOffset = -8,
    #                     toggleDisplay = T, 
    #                     autoToggleDisplay = T, aimingRectOptions = list(weight = 1),
    #                     tiles =  mapbox_moon)  %>%
  })
  
  ############### --------------- Outputs --------------- ###############
  # plot
  output$plot <- renderDygraph({
    tide_plot()
  })
  
  # table
  output$table <- DT::renderDataTable({
    tide_table()
  })
  
  output$uiModal <- renderUI({
    bsModal(ns('modal'), title = station_label(), trigger = 'click2', size = "large",
            div(id = ns("top_row"),
                
                div(id = ns('dates'),
                    fluidRow(
                      column(4,
                             dateInput(ns("from"), "From:", format = "M d, yyyy")
                      ),
                      column(4,
                             dateInput(ns("to"), "To:", format = "M d, yyyy", value = Sys.Date() + 1)
                      ),
                      column(2,
                             numericInput(ns("interval"), "Interval (minutes):", 
                                          value = 10, min = 0, max = 60, step = 5)
                      ),
                      column(2,
                             selectInput(ns("units"), label = "Units:", 
                                         choices = c("meters", "feet"), selected = "meters")))),
                tabsetPanel(id = ns("tabs"),
                  tabPanel(title = "Plot",
                           br(),
                           dygraphOutput(ns("plot"), height = "375px")),
                  tabPanel(title = "Table",
                           br(),
                           DT::dataTableOutput(ns('table')))
                ),
                br(),
                downloadButton(outputId = ns("download"), label = "Download data (csv)", class = 'small-dl')))
  })
  
  ############### --------------- Download handlers --------------- ###############
  output$download <- downloadHandler(
    filename = function() {
      paste0(pretty_label(), "_", gsub("-", "", as.character(input$from)), "_", gsub("-", "", as.character(input$to)), ".csv")
    },
    content <- function(file) {
      readr::write_csv(download_data(), file)
    }
  )
  
}