## app.R ##
library(shiny)
library(shinydashboard)
library(DT)

rawdata <- NULL

ui <- dashboardPage(
  dashboardHeader(title = "DDConvertor"),
  ## Sidebar Menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload_tab"),
      menuItem("Variables", tabName = "variables_tab"),
      menuItem("Define Levels", tabName = "define_levels_tab")
    )
  ),
  ## Upload Data ----
  dashboardBody(
    tabItems(
    tabItem(tabName = "upload_tab",
      fluidRow(
        box(
          title = "Upload Data",
          width = 12,
          p("This is a working demo and many functions do not work yet."),
          fileInput("inFile", "CSV Data File", 
                    multiple = FALSE, width = NULL,
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    ), 
                    buttonLabel = "Browse...", 
                    placeholder = "No file selected"
          ),
          DTOutput("display_rawdata")
        )
      )
    ),
    ## Variables ----
    tabItem(tabName = "variables_tab",
      fluidRow(
        box(
          title = "Variables",
          width = 12,
          DTOutput("vars_table")
        )
      )
    ),
    ## Define Levels ----
    tabItem(tabName = "define_levels_tab",
      fluidRow(
        box(
          title = "Define Levels",
          width = 12,
          # TODO:add support for multiple edit
          selectInput("level_col_select",
                      label = "Columns",
                      choices = c(),
                      multiple = FALSE), 
          DTOutput("level_col_table")
        )
      )
    )
  ))
)

server <- function(input, output, session) { 
  ## Load data ----
  dat <- reactive({
    inFile <- input$inFile
    if (is.null(inFile)) return(NULL)
    
    rawdata <<- read.csv(inFile$datapath)
  })
  
  ## output$display_rawdata ----
  output$display_rawdata <- renderDataTable({
    dat()
    datatable(rawdata)
  })
  
  ## output$vars_table ----
  output$vars_table <- renderDataTable({
    colnames <- names(rawdata)
    
    updateSelectInput(session, "level_col_select", 
                      label = NULL, choices = colnames,
                      selected = NULL)
    
    types <- sapply(rawdata, class)
    
    unique_val_n <- apply(rawdata, 2, function(x) { 
      length(unique(x)) 
    })
    
    unique_vals <- apply(rawdata, 2, function(x) {
      uv <- unique(x)
      if (length(uv) < 11) {
        paste(sort(uv), collapse = ", ")
      } else {
        ""
      }
    })
    
    vartable <- data.frame(
      Column.Name = colnames,
      Description = NA,
      Data.Type = types,
      Unique.Values = unique_val_n,
      Levels = unique_vals
    )
    
    datatable(vartable, editable = TRUE)
  })
  
  ## output$level_col_table ----
  output$level_col_table <- renderDataTable({
    theCol <- input$level_col_select
    
    unique_vals <- sort(unique(rawdata[,theCol]))
    unique_desc <- unique_vals
    # TODO: get unique_desc from attributes if they exist
    
    level_col_data <- data.frame(
      "values" = unique_vals,
      "description" = unique_desc
    )
    
    datatable(level_col_data, editable = TRUE)
    
    # TODO: save values on change to attributes
  })
}

shinyApp(ui, server)