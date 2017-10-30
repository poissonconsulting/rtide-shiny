
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
  
  ### dynamic download button
  output$download1 <- renderUI({
    downloadButton("down1", "png")
  })
  
  output$download2 <- renderUI({
    downloadButton("down2", "csv")
  })
  
  output$download3 <- renderUI({
    downloadButton("down3", "csv")
  })
  
  # filter data
  filterData <- reactive({
    loc <- location$loc
    data <- filter(sites, X == round(loc[1], 4) & Y == round(loc[2], 4))
    data
  })
  
  ### reactive labels
  stationLabel <- reactive({
    loc <- location$loc
    label <- filter(sites, X == loc[1] & Y == loc[2])$Station
    print(label)
  })
  
  prettyLabel <- reactive({
    station <- filterData()$Station
    label <- strsplit(station, ",")[[1]][1] %>%
      sub(" ", "", .)
    label
  })
  
  unitLabel <- reactive({
    if(input$units) { u <- "Tide Height (m)"} else {u <- "Tide Height (ft)"}
    u
  })
  
  ### Tide data
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
    
    if(input$units){
      data  <- data
    } else {
      data %<>% mutate(TideHeight = round(TideHeight * 3.3333, 2)) 
    }
    
    data

  })
  
  dailyData <- reactive({
    data <- tideData()
    
    high <- data[find_peaks(data$TideHeight, m = 3), c("Station", "DateTime", "TideHeight", "TimeZone")] 
    low <- data[find_peaks(-data$TideHeight, m = 3), c("Station", "DateTime", "TideHeight", "TimeZone")] 
    daily <- rbind(high, low)
  })
  
  ### create download csv datasets
  # all data
  download2Data <- reactive({
    data <- tideData()
    
    data %<>% mutate(DateTime = as.character(DateTime))
    
    if(input$units){data %<>% setNames(c("Station", "DateTime", "TideHeight_m", "TimeZone"))} else {
      data %<>% setNames(c("Station", "DateTime", "TideHeight_ft", "TimeZone"))
    }
    data
  })
  
  # daily low/high
  download3Data <- reactive({
    data <- dailyData()
    
    data %<>% mutate(Date = lubridate::date(DateTime)) %>%
      group_by(Date, TideHeight) %>%
      arrange(DateTime) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(DateTime) %>%
      select(-Date)
    

    if(input$units){data %<>% setNames(c("Station", "DateTime", "TideHeight_m", "TimeZone"))} else {
      data %<>% setNames(c("Station", "DateTime", "TideHeight_ft", "TimeZone"))
    }
    data
  })
  
  ### generate data.frame from user-submitted form
  feedbackData <- reactive({
    data <- data.frame(Name = input$name,
                       Email = input$email,
                       Comment = input$comment)
    data
  })
  
  ### upload feedbackdata() to dropbox
  uploadData <- function(df){
    
    withProgress(message = "Sending to administrator...", value = 0, {
      incProgress(amount = .25)
      time <- as.integer(Sys.time())
      # Create a unique file name
      file_name <- paste0("rtide-shiny-feedback-", time, "-", digest::digest(df), ".csv")

      # Write the data to a temporary file locally
      file_path <- file.path(tempdir(), file_name)
      readr::write_csv(df, path = file_path)
      
      # Upload the files to Dropbox
      rdrop2::drop_upload(file_path, path = feedback_folder, dtoken = token)
      incProgress(amount = .75)
      
      # send email
      message <- gmailr::mime(From = fromadd,
                              To = toadd,
                              Subject = "Shiny upload: rtide app feedback",
                              body = "Feedback has been submitted by a user of the rtide shiny app. Check '~/Poisson/Shiny/rtide/feedback/' dropbox folder to view csv.")
      
      gmailr::send_message(message)
      incProgress(amount = 1)
      

    })
  }
  
  # only allow form submission when comments field is filled out
  observe({
    shinyjs::toggleState(id = "submit_feedback", condition = input$comment)
  })
  
  
  ### plots/tables
  # dygraph
  tidePlot <- reactive({
    dat <- tideData()

    pad <- (max(dat$TideHeight) - min(dat$TideHeight))/7
    padrange <- c(min(dat$TideHeight) - pad, max(dat$TideHeight) + pad)
              

    dat %<>% select(TideHeight, DateTime) %>%
      setNames(c(unitLabel(), "Date-Time"))
    xtsdat <- xts::xts(dat, order.by = dat$`Date-Time`)

    dygraph(xtsdat, height = "10px") %>%
      dyOptions(strokeWidth = 1.5, drawGrid = F, includeZero = F,
                useDataTimezone = T, drawGapEdgePoints = T, rightGap = 0) %>%
      dyRangeSelector() %>%
      dyAxis("y", valueRange = padrange,
             label = unitLabel())

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
      dplyr::select(Date, Time, TideHeight) %>%
      setNames(c("Date", "Time", unitLabel()))
    
    data
  })
  
  dailyTable <- reactive({
    data <- dailyData() 
    
    data %<>% mutate(Year = lubridate::year(DateTime),
                     Month = lubridate::month(DateTime, label = T, abbr = T),
                     Day = lubridate::day(DateTime),
                     Time2 = lapply(strsplit(as.character(DateTime), " "), "[", 2) %>% unlist(),
                     Height = round(TideHeight, 2)) %>%
      dplyr::mutate(Hour = lapply(strsplit(as.character(Time2), ":"), "[", 1) %>% unlist(),
                    Minute = lapply(strsplit(as.character(Time2), ":"), "[", 2) %>% unlist()) %>%
      mutate(Time = paste0(Hour, ":", Minute),
             Date = paste0(Month, " ", Day, ", ", Year)) %>%
      group_by(Date, Height) %>%
      arrange(DateTime) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(DateTime) %>%
      select(Date, Time, Height) %>%
      setNames(c("Date", "Time", unitLabel()))
    
    data
  })
  
  ### when click on map or search site
  observeEvent(c(input$map_marker_click, input$search_site),
               {leafletProxy('map') %>%
                   setView(lat = location$loc[2], lng = location$loc[1] + 0.12, zoom = click_zoom)})
  
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
  
  observeEvent(input$submit_feedback,
               {uploadData(feedbackData())
                 removeModal()})
  
  observeEvent(input$feedback,
               {showModal(modalDialog(title = "Please fill out fields and submit.", 
                                      size = "m", easyClose = T,
                                        textInput("name", "Name (optional):", width = "30%"),
                                        textInput("email", "Email (optional):", width = "30%"),
                                        textInput("comment", labelMandatory("Comment:"), width = "80%"),
                                        actionButton("submit_feedback", "Submit")))})
  
  
  ### png plot download
    output$down1 <- downloadHandler(
      filename = function() {
        paste0(prettyLabel(), "_", gsub("-", "", as.character(input$from)), "_", gsub("-", "", as.character(input$to)), ".csv")
      },
      content <- function(file) {
        readr::write_csv(downloadData(), file)
      }
    )
  
  ### csv all data download
    output$down2 <- downloadHandler(
      filename = function() {
        paste0(prettyLabel(), "_", gsub("-", "", as.character(input$from)), "_", gsub("-", "", as.character(input$to)), ".csv")
      },
      content <- function(file) {
        readr::write_csv(download2Data(), file)
      }
    )
    
  ### csv low/high download
    output$down3 <- downloadHandler(
      filename = function() {
        paste0("DailyLowHigh", "_", prettyLabel(), "_", gsub("-", "", as.character(input$from)), "_", gsub("-", "", as.character(input$to)), ".csv")
      },
      content <- function(file) {
        readr::write_csv(download3Data(), file)
      }
    )
}






