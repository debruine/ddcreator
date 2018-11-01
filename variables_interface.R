variables_tab <- tabItem(tabName = "variables_tab",
  fluidRow(
    box(
      title = "Variables",
      width = 12,
      DTOutput("vars_table")
      #actionButton("copy_var_to_desc_button", "Copy Variable Names to Description")
    )
  )
)