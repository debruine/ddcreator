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
  dashboardBody(
    tabItem(tabName = "contents",
      fluidRow(
        box(
          title = "Data contents",
          DTOutput("contents")
        )
      )
    ),
    tabItem(tabName = "variables",
      fluidRow(
        box(
          title = "Variables",
          DTOutput("vars")
        )
      )
    ),
    tabItem(tabName = "define_levels",
            fluidRow(
              box(
                title = "Define Levels"
              )
            )
    )
  )
)

server <- function(input, output, session) { 
  dat <- reactive({
    inFile <- input$inFile
    if (is.null(inFile)) return(NULL)
    
    d <- read.csv(inFile$datapath)
    d
  })
  
  output$contents <- renderDataTable({
    datatable(dat())
  })
  
  output$vars <- renderDataTable({
    rawdata <- dat()
    colnames <- names(rawdata)
    
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
      Column.Names = colnames,
      Description = NA,
      Data.Type = types,
      Unique.Values = unique_val_n,
      Levels = unique_vals
    )
    
    datatable(vartable, editable = TRUE)
  })
}

shinyApp(ui, server)