## app.R ##
library(shiny)
library(shinydashboard)
library(DT)

## interface files
source("project_interface.R")
source("help_interface.R")
source("upload_interface.R")
source("variables_interface.R")
source("labels_interface.R")
source("output_interface.R")

## Global Variables ----
rawdata <- NULL
var_data <- NULL 
level_col_data <- NULL
attribute_storage <- list()

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "DDConvertor"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Project Info", tabName = "project_tab"),
      menuItem("2. Upload Data", tabName = "upload_tab"),
      menuItem("3. Variables", tabName = "variables_tab"),
      menuItem("4. Category Labels", tabName = "labels_tab"),
      menuItem("5. Output", tabName = "output_tab"),
      menuItem("Help", tabName = "help_tab")
    )
  ),
  dashboardBody(
    tabItems(
      project_tab,
      upload_tab,
      variables_tab,
      labels_tab,
      output_tab,
      help_tab
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage

## server ----
server <- function(input, output, session) { 
  ## Load data ----
  dat <- reactive({
    inFile <- input$inFile
    if (is.null(inFile)) return(NULL)
    
    file_extension <- tools::file_ext(inFile$datapath)
    if (file_extension == "csv") {
      rawdata <<- read.csv(inFile$datapath, header = input$header,
                           stringsAsFactors = F)
    } else if (file_extension %in% c("xls", "xlsx")) {
      rawdata <<- as.data.frame(readxl::read_excel(inFile$datapath, 
                                                   col_names = input$header))
    } else if (file_extension %in% c("sav")) {
      rawdata <<- haven::read_sav(inFile$datapath)
    } else if (file_extension %in% c("sas")) {
      rawdata <<- haven::read_sas(inFile$datapath)
    }
    
    #save file name as global variable for writing
    file_name <<- gsub(paste0("." , file_extension), "", inFile$name)
    
    # populate level attributes
    column_names <- names(rawdata)
    attribute_storage <<- sapply(column_names, function(x) NULL)
    
    for (theCol in column_names) {
      vals <- sort(unique(rawdata[,theCol]))
      desc <- as.character(vals)
      # TODO: get value + description labels from SPSS attributes if they exist
      
      attribute_storage[[theCol]] <<- data.frame(
        "values" = vals,
        "description" = desc,
        stringsAsFactors = F
      )
    }
  })
  
  ## output$rawdata_table ----
  output$rawdata_table <- renderDataTable({
    dat()
    datatable(rawdata, rownames = F)
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
    
    missing_val_n <- apply(rawdata, 2, function(x) { 
      length(which(is.na(x))) 
    })
    
    unique_vals <- apply(rawdata, 2, function(x) {
      uv <- unique(x)
      if (length(uv) < 11) {
        paste(sort(uv), collapse = ", ")
      } else {
        ""
      }
    })
    
    min_vals <- apply(rawdata, 2, min, na.rm = T)
    max_vals <- apply(rawdata, 2, max, na.rm = T)
    
    var_data <<- data.frame(
      # not editable
      variable = column_names,
      unique_values = unique_val_n,
      missing_values = missing_val_n,
      levels = unique_vals,
      
      # editable - required
      # TODO: get from SPSS description column
      description = "",
      
      # editable - optional
      type = types,
      min = min_vals,
      max = max_vals,
      na = TRUE,
      na_values = 'NA',
      synonyms = NA,
      
      # arguments
      stringsAsFactors = F
    )
    
    datatable(var_data, editable = TRUE, rownames = F,
              colnames = c(
                'Variable',
                '# Unique Values',
                '# Missing Values',
                'Levels',
                'Description (required)',
                'Type',
                'Minimum Allowable',
                'Maximum\nAllowable',
                'Missing Allowed',
                'Missing Values =',
                'Synonyms'
              ))
  })
  
  vars_proxy = dataTableProxy('vars_table', session)
  
  # ## copy_var_to_desc_button
  # observeEvent(input$copy_var_to_desc_button, {
  #   var_data[, 'description'] <<- var_data[, 'variable']
  #   DT::replaceData(vars_proxy, var_data, resetPaging = F)
  # })
  
  ## proxy saving variable data ----
  observeEvent(input$vars_table_cell_edit,  {
    info = input$vars_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    var_data[i,j] <<- isolate(DT::coerceValue(v, var_data[i,j]))
    #replaceData(vars_proxy, var_data, resetPaging = F)
  })
  
  ## output$level_col_table ---- 
  output$level_col_table <- renderDataTable({
    theCol <- input$level_col_select
    
    level_col_data <<- attribute_storage[[theCol]]
    
    datatable(level_col_data, editable = TRUE, rownames = F)
  })
  
  ## proxy saving level column data ----
  level_col_proxy = dataTableProxy('level_col_table', session)
  observeEvent(input$level_col_table_cell_edit,  {
    info = input$level_col_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    level_col_data[i,j] <<- isolate(DT::coerceValue(v, level_col_data[i,j]))
    #replaceData(level_col_proxy, level_col_data, resetPaging = F)
    
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
      temp <- do.call("rbind", attribute_storage)
      colnames(temp) = c("description", "values") #temporary fix since these are writing out backwards
      write.csv(temp[ , c(2,1)], file, row.names = T, quote = TRUE)
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