search <- function(input, output, session) {
  search <- reactiveValues(station = NULL)
  observe({search$station <- input$search})
  return(search)
}