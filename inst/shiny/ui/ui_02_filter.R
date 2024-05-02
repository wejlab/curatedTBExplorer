# Summarize tab page
tabPanel(
  "Summarize",

  icon = icon("filter", class = "fa-solid fa-filter", lib = "font-awesome"),
  tabsetPanel(
    tabPanel(
      "Filter",
      sidebarPanel(

        # Title and display of selected studies
        h4("Selected Studies:"),
        verbatimTextOutput("selected_studies_text"),

        # Dropdown box for selecting filter
        selectInput("filter_by", "Filter By",
                    choices = NULL
        ),

        # This will outputa dynamic field with a conditional selectInput and filter button created in server file
        uiOutput("dynamic_filter"),
        br(),

        # Add Filter button
        actionButton("add_filter_btn", "Add Filter", class = "btn btn-primary"),

        # Displays selected filters
        uiOutput("selected_filters_ui"),

        # APPLY FILTER BUTTON
        actionButton("filter_apply_btn", "Apply Filter", class = "btn-primary"),

        # Reset filter button
        actionButton("filter_reset_btn", "Reset"),
        br()
      ),
      mainPanel(
        fluidRow(
          column(
            12,
            DTOutput("filter_summary_table")
          ),
        ),
        width = 8
      )
    ),
    tabPanel(
      "Visualize",
      sidebarPanel(

        # Summary Table
        tableOutput("summaryStatsTable"),

        # Dropdown box for selecting variable to filter by
        selectInput("visualize_filter_by", "Filter By",
                    choices = NULL
        ),
        actionButton("visualize_btn", "Visualize")
      ),
      mainPanel(
        fluidRow(
          column(6,
            plotOutput("top_visualization"),
            plotOutput("pieChart")
          ),
          column(6,
            plotOutput("boxPlot")
          )
        )
      )
    )
  ),

)
