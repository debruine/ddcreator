## app.R ##
library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Data Codebook Convertor"),
  dashboardSidebar(
    sidebarMenu(
      fileInput("inFile", "Your Data File", 
                multiple = FALSE, accept = NULL, width = NULL,
                buttonLabel = "Browse...", placeholder = "No file selected"),
      menuItem("Contents", tabName = "contents"),
      menuItem("Variables", tabName = "variables"),
      menuItem("Define Levels", tabName = "define_levels")
    )
  ),
  # Contents ----
  dashboardBody(
    tabItems(
    tabItem(tabName = "contents",
      fluidRow(
        box(
          title = "Data contents",
          DTOutput("contents")
        )
      )
    ),
    # Variables ----
    tabItem(tabName = "variables",
      fluidRow(
        box(
          title = "Variables",
          DTOutput("vars")
        )
      )
    ),
    # Define Levels ----
    tabItem(tabName = "define_levels",
            fluidRow(
              box(
                title = "Define Levels",
                selectInput("level_col_select",
                            label = "Columns",
                            choices = c(),
                            multiple = FALSE), # add support for multiple edit
                DTOutput("level_col")
              )
            )
    )
  ))
)




server <- function(input, output, session) { 
  dat <- reactive({
    inFile <- input$inFile
    if (is.null(inFile)) return(NULL)
    
    d <- read.csv(inFile$datapath)
    d
  })
  
  # output$contents ----
  output$contents <- renderDataTable({
    datatable(dat())
  })
  
  # output$vars ----
  output$vars <- renderDataTable({
    rawdata <- dat()
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
  
  # output$level_col ----
  output$level_col <- renderDataTable({
    theCol <- input$level_col_select
    rawdata <- dat()
    
    unique_vals <- sort(unique(rawdata[,theCol]))
    unique_desc <- unique_vals
    # TODO: get unique_desc from attributes if they exist
    
    level_col_table <- data.frame(
      "values" = unique_vals,
      "description" = unique_desc
    )
    
    datatable(level_col_table, editable = TRUE)
    
    # TODO: save values on change to attributes
  })
}

shinyApp(ui, server)