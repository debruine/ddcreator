upload_tab <- tabItem(tabName = "upload_tab",
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
)