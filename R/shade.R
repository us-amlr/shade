#' shade
#'
#' Runs shade, the shiny-shadow-fixer R Shiny App
#' 
#' @param ... passed to \code{\link[shiny]{shinyApp}}
#' 
#' @details
#' todo
#' 
#' @return 
#' todo
#'  
#' @examples
#' if (interactive()) shade()
#' 
#' @export
shade <- function(...) {
  csv.accept <- c(
    "text/csv",
    "text/comma-separated-values,text/plain",
    ".csv"
  )
  
  
  ui <- fluidPage(
    ### Use shinybusy to indicate when plot work is being done
    shinybusy::add_busy_spinner(
      spin = "double-bounce", position = "top-right", margin = c(20, 20),
      height = "100px", width = "100px"
    ),
    
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "File input:"),
        textInput("txt", "Text input:", "general"),
        sliderInput("slider", "Slider input:", 1, 100, 30),
        tags$h5("Default actionButton:"),
        actionButton("action", "Search"),
        
        tags$h5("actionButton with CSS class:"),
        actionButton("action2", "Action button", class = "btn-primary")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Tab 1",
                   h4("Table"),
                   tableOutput("table"),
                   h4("Verbatim text output"),
                   verbatimTextOutput("txtout"),
                   h1("Header 1"),
                   h2("Header 2"),
                   h3("Header 3"),
                   h4("Header 4"),
                   h5("Header 5")
          ),
          tabPanel("Tab 2", "This panel is intentionally left blank"),
          tabPanel("Tab 3", "This panel is intentionally left blank")
        )
      )
    )
  )
  
  
  server <- function(input, output, session) {
    #---------------------------------------------------------------------------- 
    
  }
  
  shiny::shinyApp(ui = ui, server = server, ...)
}