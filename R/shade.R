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
    
    tags$head(
      tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))
    ),
    
    rclipboardSetup(),
    
    # ### Use shinybusy to indicate when plot work is being done
    # shinybusy::add_busy_spinner(
    #   spin = "double-bounce", position = "top-right", margin = c(20, 20),
    #   height = "100px", width = "100px"
    # ),
    
    titlePanel("Shiny Shadow Fixer"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput("viame_csv", "VIAME CSV input"),
        selectInput("glider_deployment", "Glider deployment", 
                    choices = shade::glider.deployments$deployment, 
                    selected = "amlr08-20220513"), 
        selectInput("segmentation_method", "Segmentation method", 
                    choices = c("regions", 
                                "regions-tmser"), 
                    selected = "regions"),
        textInput("directory_name", "Directory name", placeholder = "Dir####")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Parent Image Finder",
                   tags$h5("This tab allows you generate the URL of the full shadowgraph image. ", 
                           "Specify the 'glider deployment' ", 
                           "and 'Directory name'in the left sidebar, ", 
                           "and then also make selections in this tab"), 
                   tags$h5("Specifically:", tags$br(), 
                           "1) select if this is a raw or processed image", tags$br(), 
                           "2) select the image type", tags$br(), 
                           "3) paste in the full ROI image name. ", 
                           "This name will be parsed to determine the parent image name"), 
                   tags$h3("Inputs"), 
                   fluidRow(
                     radioButtons("raw_proc", "Raw or processed image?", 
                                  choices = c("raw", "proc")), 
                     uiOutput("image_type_uiOut_select"), 
                     textInput("image_name", "Image name", 
                               placeholder = "SG01 20220513-190538-011.jpg")
                   ), 
                   tags$h3("Authenticated URL"), 
                   textOutput("image_url"), 
                   uiOutput("clip"),
                   tags$br(), tags$br()
          ), 
          tabPanel("VIAME CSV Fixer", "This panel is intentionally left blank")
        )
      )
    )
  )
  
  
  server <- function(input, output, session) {
    #---------------------------------------------------------------------------
    output$image_type_uiOut_select <- renderUI({
      req(input$raw_proc)
      
      choices.list <- switch(
        input$raw_proc, 
        raw  = c("images"), 
        proc = c("images-ffPCG", "images-imgff", "jpgorig-regions")
      )
      
      selectInput("image_type", "Image type", choices = choices.list)
    })
    
    image_name_parse <- reactive({
      validate(
        need(!identical(input$image_name, ""), "Please enter an image name")
      )
      paste(head(
        unlist(str_split_1(input$image_name, "-")), 
        -1
      ), collapse = "-")
    })
    
    image_url_text <- reactive({
      validate(
        need(!identical(input$directory_name, "") & nchar(input$directory_name) == 7, 
             "Please enter a directory name that follows the format Dir####")
      )
      
      bucket.name <- switch(input$raw_proc, 
                            raw  = "amlr-gliders-imagery-raw-dev", 
                            proc = "amlr-gliders-imagery-proc-dev")
      
      gcs_url(bucket.name, input$glider_deployment, input$image_type, 
              input$directory_name, image_name_parse())
    })
    
    output$image_url <- renderText({
      image_url_text()
    })
    
    output$clip <- renderUI({
      rclipButton(
        inputId = "clipbtn",
        label = "Copy URL",
        clipText = image_url_text(), 
        icon = icon("clipboard"),
        # tooltip = "Click me... I dare you!",
        placement = "top"
        # options = list(delay = list(show = 800, hide = 100), trigger = "hover")
      )
    })
    
  }
  
  shiny::shinyApp(ui = ui, server = server, ...)
}