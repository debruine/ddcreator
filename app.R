## app.R ##
library(shiny)
library(shinydashboard)
library(DT)

rawdata <- NULL
var_data <- NULL 
level_col_data <- NULL
attribute_storage <- list()

ui <- dashboardPage(
  dashboardHeader(title = "DDConvertor"),
  ## Sidebar Menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload_tab"),
      menuItem("Variables", tabName = "variables_tab"),
      menuItem("Define Levels", tabName = "define_levels_tab"),
      menuItem("Output", tabName = "output_selection")
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
    ),
    ## Output options ----
    tabItem(tabName = "output_selection",
            fluidRow(
              box(
                title = "Output",
                width = 12,
                downloadButton("output_csv", "Download CSV"),
                downloadButton("output_attributes", "Download Attributes")
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
    column_names <- names(rawdata)
    
    updateSelectInput(session, "level_col_select", 
                      label = NULL, choices = column_names,
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
    
    var_data <<- data.frame(
      Column.Name = column_names,
      Description = NA,
      Data.Type = types,
      Unique.Values = unique_val_n,
      Levels = unique_vals,
      stringsAsFactors = F
    )
    
    attribute_storage <<- as.list(rep(NA, length(column_names)))
    names(attribute_storage) <<- column_names
    
    datatable(var_data, editable = TRUE)
  })
  
  ## proxy saving variable data ----
  proxy_variable = dataTableProxy('vars_table')
  observeEvent(input$vars_table_cell_edit,  {
    info = input$vars_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    var_data[i,j] <<- coerceValue(v, var_data[i,j])
    replaceData(proxy_variable, var_data, resetPaging = F)
  })
  
  ## output$level_col_table ----
  output$level_col_table <- renderDataTable({
    theCol <- input$level_col_select
    
    unique_vals <- sort(unique(rawdata[,theCol]))
    unique_desc <- unique_vals
    # TODO: get unique_desc from attributes if they exist
    
    level_col_data <<- data.frame(
      "values" = unique_vals,
      "description" = as.character(unique_desc),
      stringsAsFactors = F
    )
    
    datatable(level_col_data, editable = TRUE)
    
    # TODO: save values on change to attributes
  })
  
  ## proxy saving level column data ----
  proxy_level_col = dataTableProxy('level_col_table')
  observeEvent(input$level_col_table_cell_edit,  {
    info = input$level_col_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    level_col_data[i,j] <<- coerceValue(v, level_col_data[i,j])
    replaceData(proxy_level_col, level_col_data, resetPaging = F)
    
    # save level column data to temp storage, eventually to attributes
    attribute_storage[input$level_col_select] <<- level_col_data
  })
  
  ## output$output_csv ----
  output$output_csv <- downloadHandler(
    filename = "SETTHIS.csv",
    content = function(file) {
      write.csv(var_data, file, row.names = F, quote = TRUE)
    }
  )
  
  ## output$output_attributes_csv ----
  output$output_attributes <- downloadHandler(
    filename = "stuff.csv",
    content = function(file) {
      write.csv(attribute_storage, file, row.names = F, quote = TRUE)
    }
  )
}

shinyApp(ui, server)