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
      menuItem("Project Info", tabName = "project_tab"),
      menuItem("Upload Data", tabName = "upload_tab"),
      menuItem("Variables", tabName = "variables_tab"),
      menuItem("Define Levels", tabName = "define_levels_tab"),
      menuItem("Output", tabName = "output_tab"),
      menuItem("Metadata Format", tabName = "format_tab")
    )
  ),
  ## Upload Data ----
  dashboardBody(
    tabItems(
      ## Project Info ----
      tabItem(tabName = "project_tab",
              fluidRow(
                box(
                  title = "Project Info",
                  width = 12,
                  p(
                    "This is a working demo and many functions do not work yet. You can upload csv or excel files to test the process. We do not save any information and delete your data from the temp directory regularly."
                  ),
                  textInput("project_name", "Project Name"),
                  textInput("project_author", "Project Authors"),
                  textAreaInput("project_description", "Project Description")
                )
              )
      ),
      tabItem(tabName = "format_tab",
              fluidRow(
                box(
                  title = "Metadata format",
                  width = 12,
                  p(
                    "If metadata is included, this should be specified as a nested JSON file containing a name, a description, an author, and a variable object. The nested variable object must minimally encode a list of objects, one per variable. Each object has the following keys:"
                  ),
                  tags$ol(
                    tags$li("variable: name of variable"),
                    tags$li("description: description of variable"),
                    tags$li("[type]: “string”, “int”, “float”, “bool”"),
                    tags$li("[min]: minimum valid value (not necessarily observed value)"),
                    
                    tags$li("[max]: maximum valid value (not necessarily observed value)"),
                    tags$li("[levels]: valid levels of a categorical (string) variable"),
                    tags$li("[na] (bool): whether or not missing values are allowed"),
                    tags$li(
                      "[na_values]: values to be treated as missing or invalid observations. Defaults to [“NA”, “N/A”, “n/a”, “NaN”, “nan”]"
                    ),
                    tags$li(
                      "[synonyms]: A comma-separated list of common synonyms for the variable name. These are user-defined and not constrained by a controlled vocabulary."
                    )
                  )
                )
              )),
      tabItem(tabName = "upload_tab",
              fluidRow(
                box(
                  title = "Upload Data",
                  width = 12,
                  p("This is a working demo and many functions do not work yet."),
                  fileInput("inFile", "CSV/XLS(X) Data File", 
                            multiple = FALSE, width = NULL,
                            accept = c(
                              'text/csv',
                              'text/comma-separated-values,text/plain',
                              '.csv',
                              '.xls',
                              '.xlsx'
                            ), 
                            buttonLabel = "Browse...", 
                            placeholder = "No file selected"
                  ),
                  checkboxInput("header", "Data file has a header", TRUE),
                  DTOutput("rawdata_table")
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
      tabItem(tabName = "output_tab",
              fluidRow(
                box(
                  title = "Output",
                  width = 12,
                  downloadButton("output_csv", "Download CSV"),
                  downloadButton("output_attributes", "Download Attributes (test)"),
                  downloadButton("output_rdata", "Download R data file (test)")
                )
              )
      )
    ) # end tabItems()
  )
) # end dashboardPage()

server <- function(input, output, session) { 
  ## Load data ----
  dat <- reactive({
    inFile <- input$inFile
    if (is.null(inFile)) return(NULL)
    
    file_extension <- tools::file_ext(inFile$datapath)
    if (file_extension == "csv") {
      rawdata <<- read.csv(inFile$datapath, header = input$header)
    } else if (file_extension %in% c("xls", "xlsx")) {
      rawdata <<- as.data.frame(readxl::read_excel(inFile$datapath, 
                                                   col_names = input$header))
    }
    
    #save file name as global variable for writing
    file_name <<- gsub(paste0("." , file_extension), "", inFile$name)
    
    # populate level attributes
    column_names <- names(rawdata)
    attribute_storage <<- sapply(column_names, function(x) NULL)
    
    for (theCol in column_names) {
      unique_vals <- sort(unique(rawdata[,theCol]))
      unique_desc <- as.character(unique_vals)
      # TODO: get unique_desc from SPSS attributes if they exist
      
      attribute_storage[[theCol]] <<- data.frame(
        "values" = unique_vals,
        "description" = unique_desc,
        stringsAsFactors = F
      )
    }
  })
  
  ## output$rawdata_table ----
  output$rawdata_table <- renderDataTable({
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
    
    min_vals <- apply(rawdata, 2, min)
    max_vals <- apply(rawdata, 2, max)
    
    var_data <<- data.frame(
      variable = column_names,
      description = NA,
      type = types,
      unique_values = unique_val_n,
      min = min_vals,
      max = max_vals,
      levels = unique_vals,
      na = NA,
      na_values = NA,
      synonyms = NA,
      stringsAsFactors = F
    )
    
    datatable(var_data, editable = TRUE)
  })
  
  ## proxy saving variable data ----
  vars_proxy = dataTableProxy('vars_table')
  observeEvent(input$vars_table_cell_edit,  {
    info = input$vars_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    var_data[i,j] <<- coerceValue(v, var_data[i,j])
    replaceData(vars_proxy, var_data, resetPaging = F)
  })
  
  ## output$level_col_table ---- 
  output$level_col_table <- renderDataTable({
    theCol <- input$level_col_select
    
    level_col_data <<- attribute_storage[[theCol]]
    
    datatable(level_col_data, editable = TRUE)
  })
  
  ## proxy saving level column data ----
  level_col_proxy = dataTableProxy('level_col_table')
  observeEvent(input$level_col_table_cell_edit,  {
    info = input$level_col_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    level_col_data[i,j] <<- coerceValue(v, level_col_data[i,j])
    replaceData(level_col_proxy, level_col_data, resetPaging = F)
    
    # save level column data to temp storage, eventually to attributes
    attribute_storage[[input$level_col_select]] <<- level_col_data
  })
  
  ## output$output_csv ----
  output$output_csv <- downloadHandler(
    filename = paste0(file_name, "_metadata_", gsub("-", "", Sys.Date()), ".csv"),
    content = function(file) {
      write.csv(var_data, file, row.names = F, quote = TRUE)
    }
  )
  
  ## output$output_attributes_csv ----
  output$output_attributes <- downloadHandler(
    filename = paste0(file_name, "_valuelabels_", gsub("-", "", Sys.Date()), ".csv"),
    content = function(file) {
      write.csv(attribute_storage, file, row.names = F, quote = TRUE)
    }
  )
  
  ## output$output_Rdata & set attributes ----
  output$output_rdata <- downloadHandler(
    filename= paste0(file_name, "_metadata_", gsub("-", "", Sys.Date()), ".Rdata"),
    content = function(file) {
      
      #convert missing descriptions to blank
      var_data[is.na(var_data)] <- ""
      
      #variable & value labels
      for (i in 1:ncol(rawdata)){
        attr(rawdata[,i], "label") <- var_data$Description[i]
        
        #set up value labels 
        #TO DO: Get this working; unsure how data is being set
        temp <- as.character(attribute_storage[[i]][,1])
        names(temp) <- attribute_storage[[i]][,2]
        attr(rawdata[,i], "labels") <- temp
      }
      
      
      
      save(rawdata, file=file)
    }
  )
  
  
} # end server()

shinyApp(ui, server)