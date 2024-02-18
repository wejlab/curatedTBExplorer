tabPanel( #summarize tab empty for now
  "Summarize",
  icon = icon("table"),
  fluidRow(
    column(12,
           DTOutput("selected_studies_table")
    )
  )
)
