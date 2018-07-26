project_tab <- tabItem(tabName = "project_tab",
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
)