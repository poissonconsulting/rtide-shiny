map <- function(input, output, session) {
  ns <- session$ns
  
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
        lng = sites$X, lat = sites$Y,
        label = sites$Station,
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

  
  observeEvent(c(input$leaflet_marker_click, input$search), {
    toggleModal(session, "modal", "open")
  })
  
  # ############ Reactives ------
  # filter data
  filter_data <- reactive({
    loc <- location$loc
    sites[which(sites$X == round(loc[1], 4) & sites$Y == round(loc[2], 4)),]
  })
  
  ### reactive labels
  station_label <- reactive({
    loc <- location$loc
    sites[which(sites$X == loc[1] & sites$Y == loc[2]),]$Station
  })
  # 
  pretty_label <- reactive({
    station <- filter_data()$Station
    strsplit(station, ",")[[1]][1] %>%
      sub(" ", "", .)
  })

  unit_label <- reactive({
    if(input$units == "meters") {
      return("Tide Height (m)")
    }
    "Tide Height (ft)"
  })

  ### Tide data
  tide_data <- reactive({
    req(location$loc)
    filtered <- filter_data()
    label <- filtered$Station
    tz <- filtered$TZ

    data <- rtide::tide_height(
      label, from = input$from, to = input$to,
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
      return(data %>% setNames(c("Station", "DateTime", "TideHeight_m", "TimeZone")))} 
    data %>% setNames(c("Station", "DateTime", "TideHeight_ft", "TimeZone"))
  })

  feedback_data <- reactive({
    data.frame(Name = input$name,
                       Email = input$email,
                       Comment = input$comment)
  })

  # metrics
  # tide_plot <- reactive({
  #  dat <- tide_data()
  # 
  #  gp <- mjs_plot(data = dat, x = DateTime, y = TideHeight, height = 10, left = 10, top = 0) %>%
  #    mjs_line() %>%
  #    mjs_labs(y = "Tide Height (m)")
  #  gp
  # })

  ############ outputs -------
  # dygraph
  tide_plot <- reactive({
    dat <- tide_data()
    time <- Sys.time()
    time %<>% lubridate::with_tz(tz = dat$TimeZone[1])

    pad <- (max(dat$TideHeight) - min(dat$TideHeight))/7
    padrange <- c(min(dat$TideHeight) - pad, max(dat$TideHeight) + pad)


    dat  <- dat[,c('TideHeight', 'DateTime')] %>%
      setNames(c(unit_label(), "Date-Time"))
    xtsdat <- xts::xts(dat, order.by = dat$`Date-Time`)

    dygraph(xtsdat, height = "10px") %>%
      dyOptions(strokeWidth = 1.5, drawGrid = F, includeZero = F,
                useDataTimezone = T, drawGapEdgePoints = T, rightGap = 15) %>%
      dyRangeSelector() %>%
      dyAxis("y", valueRange = padrange,
             label = unit_label()) %>%
      dyEvent(x = time, label = "Current time", labelLoc = "bottom")
  })

  tide_table <- reactive({
    data <- tide_data() 
    data$Year <- lubridate::year(data$DateTime)
    data$Month <- lubridate::month(data$DateTime, label = TRUE, abbr = TRUE)
    data$day <- lubridate::day(data$DateTime)
    data$Time2 <- lapply(strsplit(as.character(data$DateTime), " "), "[", 2) %>% unlist()
    data$Hour <- lapply(strsplit(as.character(data$Time2), ":"), "[", 1) %>% unlist()
    data$Minute <- lapply(strsplit(as.character(data$DateTime), " "), "[", 2) %>% unlist()
    data$Time <- paste0(data$Hour, ":", data$Minute)
    data$Date <- paste0(data$Month, " ", data$Day, ", ", data$Year)
    data[,c("Date", "Time", "TideHeight")] %>% 
      setNames(c("Date", "Time", unit_label()))
  })

  ############ Observers ------

  # get location of site from search or click
  location <- reactiveValues(loc = NULL)

  observeEvent(input$leaflet_marker_click, {
    click <- input$leaflet_marker_click
    loc <- c(click$lng, click$lat)
    location$loc <- loc
    })

  observeEvent(input$search, {
    station <- input$search
    lng <- filter(sites, Station == station)$X
    lat <- filter(sites, Station == station)$Y
    loc <- c(lng, lat)
    location$loc <- loc
    })

  # zoom to site on click or search
  observeEvent(c(input$leaflet_marker_click, input$search),
               {leafletProxy('leaflet') %>%
                   setView(lat = location$loc[2], lng = location$loc[1] + 0.12, zoom = click_zoom)})

  # render label
  observeEvent(c(input$leaflet_marker_click, input$search),
               {output$station <- renderText({station_label()})})

  # plot
  observeEvent(c(input$leaflet_marker_click, input$search),
               {output$plot <- renderDygraph({
                 tide_plot()
               })})

  # table
  observeEvent(c(input$leaflet_marker_click, input$search),
               {output$table <- DT::renderDataTable({
                 tide_table()
               })})

  ### csv download
  output$download <- downloadHandler(
    filename = function() {
      paste0(pretty_label(), "_", gsub("-", "", as.character(input$from)), "_", gsub("-", "", as.character(input$to)), ".csv")
    },
    content <- function(file) {
      readr::write_csv(download_data(), file)
    }
  )
  
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
                tabsetPanel(
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
  
}