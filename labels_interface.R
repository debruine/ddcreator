labels_tab <- tabItem(tabName = "labels_tab",
  fluidRow(
    box(
      title = "Category Labels",
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