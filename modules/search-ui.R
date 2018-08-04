searchUI <- function(id, label = 'search') {
  ns <- NS(id)
  
  selectizeInput(inputId = ns("search"), label = NULL, 
                 choices = c("", sites$Station),
                 selected = "",
                 options = list(
                   placeholder = 'Search stations...'),
                 width = 300)
}

