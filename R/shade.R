#' shade
#'
#' Runs shade, the shiny-shadow-fixer R Shiny App
#' 
#' @param ... passed to \code{\link[shiny]{shinyApp}}
#' 
#' @details
#' Shiny app that allows users to generate the parent GCS URL for a shadowgraph
#' region image, and fix a VIAME CSV file using user-provided thresholds
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
    tags$h5("Find parent image URL, or fix VIAME CSV output"), 
    
    sidebarLayout(
      sidebarPanel(
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
                   column(
                     width = 12, 
                     fluidRow(
                       column(6, radioButtons("raw_proc", "Raw or processed image?", 
                                              choices = c("raw", "proc"))), 
                       column(6, uiOutput("image_type_uiOut_select"))
                     ), 
                     textInput("image_name", "Region (ROI) image name", 
                               placeholder = "SG01 20220513-190538-011.jpg-out0.jpg")
                   ), 
                   tags$h3("Authenticated URL"), 
                   textOutput("image_url"), 
                   uiOutput("clip"),
                   tags$br(), tags$br()
          ), 
          tabPanel("VIAME CSV Fixer", 
                   tags$h5("instructions todo"), 
                   fileInput("viame_csv", "VIAME CSV input", accept = csv.accept),
                   tags$h3("Threshold corrections"), 
                   column(
                     width = 12,                    
                     fluidRow(
                       column(6, numericInput("threshold_base", "Base Confidence Threshold",
                                              value = 0.1, min = 0, max = 1, step = 0.01)), 
                       column(6, checkboxInput("threshold_individual", 
                                               "Do you have individual threshold values?", 
                                               value = FALSE))
                     ), 
                     conditionalPanel(
                       condition = "input.threshold_individual == true", 
                       tags$h5("individual todo"), 
                       fluidRow(
                         column(6, uiOutput("threshold_classes_uiOut_select")), 
                         column(6, uiOutput("threshold_individual_widgets"))
                       )
                     )
                   ), 
                   tags$h3("Corrected CSV file"), 
                   uiOutput("export_corrected_button"), 
                   tags$br(), 
                   DTOutput("vcsv_corrected")
          )
        )
      )
    )
  )
  
  
  server <- function(input, output, session) {
    session$onSessionEnded(function() {
      stopApp()
    })
    
    options(shiny.maxRequestSize = 100 * 1024^2)
    
    #---------------------------------------------------------------------------
    # "Parent Image Finder" tab
    output$image_type_uiOut_select <- renderUI({
      req(input$raw_proc)
      
      ### RenderUI for image type - depends on raw/proc selection
      choices.list <- switch(
        input$raw_proc, 
        raw  = c("images"), 
        proc = c("images-ffPCG", "images-imgff", "jpgorig-regions")
      )
      
      selectInput("image_type", "Image type", choices = choices.list)
    })
    
    ### Reactive wrapper to generate bucket name
    bucket_name <- reactive({
      switch(req(input$raw_proc), 
             raw  = "amlr-gliders-imagery-raw-dev", 
             proc = "amlr-gliders-imagery-proc-dev")
    })
    
    ### Reactive wrapper around region image parse function
    region_image_name_parse <- reactive({
      validate(
        need(!identical(input$image_name, ""), "Please enter an image name")
      )
      
      shade::region_image_name_parse(
        input$image_name, bucket_name(), input$image_type
      )
    })
    
    image_url_text <- reactive({
      validate(
        need(!identical(input$directory_name, "") & nchar(input$directory_name) == 7, 
             "Please enter a directory name that follows the format Dir####")
      )
      
      gcs_url(bucket_name(), input$glider_deployment, input$image_type, 
              input$directory_name, region_image_name_parse())
    })
    
    output$image_url <- renderText(image_url_text())
    
    output$clip <- renderUI({
      rclipButton(
        inputId = "clipbtn",
        label = "Copy URL",
        clipText = image_url_text(), 
        icon = icon("clipboard"),
        placement = "top"
      )
    })
    
    
    #---------------------------------------------------------------------------
    # "VIAME CSV Fixer" tab
    
    #------------------------------------------------------
    ### Read CSV
    vcsv_csv <- reactive({
      read_viame_csv(req(input$viame_csv$datapath))
    })
    
    ### Get classes from the VIAME CSV
    vcsv_classes <- reactive({
      unique(sort(vcsv_csv()$class_01))
    })
    
    ### selectInput list of classes found in the loaded CSV file
    output$threshold_classes_uiOut_select <- renderUI({
      selectInput("threshold_classes", 
                  "Classes with individual threshold values", 
                  choices = vcsv_classes(), selected = NULL, 
                  multiple = TRUE, selectize = TRUE)
    })
    
    #------------------------------------------------------
    ### Generate dynamic UI elements for individual class thresholds
    output$threshold_individual_widgets <- renderUI({
      req(vcsv_classes(), 
          input$threshold_individual)
      
      validate(
        need(input$threshold_classes, 
             "Please select at least one 'class with individual threshold value'")
      )
      
      # Extract unique values from a specific column
      # vcsv.classes <- vcsv_classes()[vcsv_classes() %in% input$threshold_classes]
      vcsv.classes <- input$threshold_classes
      
      # Create a widget for each unique value
      widget.list <- lapply(vcsv.classes, function(class) {
        numericInput(inputId = paste0("widget_", class),
                     label = class,
                     value = 0.1, min = 0, max = 1, step = 0.01)
      })
      
      # Return the list of widgets
      do.call(tagList, widget.list)
    })
    
    # Reactive expression to store values from widgets
    threshold_individual_df <- reactive({
      validate(
        need(input$threshold_classes, 
             "Please select at least one 'class with individual threshold value'")
      )
      
      bind_rows(lapply(input$threshold_classes, function(class) {
        value <- input[[paste0("widget_", class)]]
        value.error <- str_glue("Please make sure the individual threshold value", 
                                "for {value} is a numeric greater than 0")
        validate(
          need(value, value.error), 
          need(value > 0, value.error)
        )
        data.frame(class = class, threshold = value)
      }))
    })
    
    #------------------------------------------------------
    ### Correct VIAME CSV
    vcsv_correct <- reactive({
      req(vcsv_csv())
      validate(
        need(is.numeric(input$threshold_base), 
             "Please make sure the base threshold value is a numeric")
      )
      
      # Run the fixer function
      if (input$threshold_individual) {
        fix_viame_csv(vcsv_csv(), input$threshold_base, threshold_individual_df())
      } else {
        fix_viame_csv(vcsv_csv(), input$threshold_base)
      }
    })
    
    #------------------------------------------------------
    ### Outputs 
    
    # Render UI for export button
    output$export_corrected_button <- renderUI({
      validate(
        need(input$glider_deployment, 
             "To export, please select a glider deployment"), 
        need(input$segmentation_method, 
             "To export, please select a segmentation method"), 
        need(!identical(input$directory_name, "") & nchar(input$directory_name) == 7, 
             paste("To export, please enter", 
                   "a directory name that follows the format Dir####"))
      )
      req(vcsv_correct())
      
      downloadButton("export_corrected", "Export corrected VIAME CSV file")
    })
    
    
    # Table, for sanity checking
    output$vcsv_corrected <- renderDT({
      validate(
        need(input$viame_csv$datapath, 
             "Please load a VIAME CSV via the sidebar")
      )
      vcsv_correct()
    }, 
    rownames = FALSE, 
    options = list(
      pageLength = 20,
      # lengthMenu = list("20" = 20, "50" = 50, "100" = 100, 'All' = -1)
      lengthMenu = list(c(10, 20, 50, -1),
                        c('10', '20', '50', 'All'))
    ))
    
    # Download
    output$export_corrected <- downloadHandler(
      filename = function() {
        paste(input$glider_deployment, input$segmentation_method, 
              input$directory_name, input$viame_csv$name, 
              sep = "_")
      },
      content = function(file) {
        write_csv(vcsv_correct(), file)
      }
    )
  }
  
  shiny::shinyApp(ui = ui, server = server, ...)
}
