aboutUI <- function(id, label = 'About') {
  ns <- NS(id)
  
  fluidRow(
    column(1,
           HTML("")),
    column(11,
           h4(paste("Welcome!")),
           br(),
           p("This app was not designed for use on a mobile."),
           br(),
           h5(disclaimer),
           h5(proj.description), hr(),
           actionLink(ns("info1"), "About the package"),
           hidden(div(id = ns("div1"),  h6("info here"))), 
           br(),
           actionLink(ns('info2'), "Citation"),
           hidden(div(id = ns("div2"),
                      h6("info again"))),
           br(),
           actionLink(ns('contactUs'), label = 'Contact us:'),
           shinyjs::hidden(div(id = ns('feedbackForm'),
                               br(),
                               wellPanel(div(textAreaInput(ns("comment"), label = NULL, width = "100%", height = '100px'),
                                             class = 'error-message'),
                                         actionButton(ns("submitFeedback"), "Submit")))),
           hr(),
           h6('Developed by Poisson Consulting.')),
    tags$footer(actionLink(inputId = 'poisson', 
                           label = img(src = 'poisson-logo.png',
                                       height = 177/5,
                                       width = 739/5,
                                       onclick = "window.open('http://www.poissonconsulting.ca', '_blank')")),
                align = "center", 
                style = "
                position: relative;
                bottom:1;
                width:100%;
                height:50px; /* Height of the footer */
                color: #2f4f4f;
                padding: 10px;
                background-color: white;
                z-index: -1000;
                font-size: 12px"))
}