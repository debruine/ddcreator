output_tab <- tabItem(tabName = "output_tab",
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