# TBSigProfiler tab page
tabPanel(
  "TBSigProfiler",
  tabsetPanel(
    tabPanel(
      "Profiler",
      sidebarPanel(
        "Select Data For the TBSignatureProfiler",

        # Button for displaying selected profiles to display
        actionButton("showProfiles", "Profiler Selection"),

        # Conditional Panel for displaying selected profiles
        conditionalPanel(
          condition = "input.showProfiles % 2 != 0",

          # Dropdowns box for selecting profile information
          checkboxGroupInput("profiles", "Select Profiles:",
            choices = names(TBsignatures),
            selected = names(TBsignatures)
          ),

          # Dropdown box for selecting assay
          selectInput("assay", "Select Assay:",
            choices = c("log_counts", "log_counts_cpm", "counts"),
            selected = "log_counts"
          )
        )
      ),

      # Displays ssgsea table
      DTOutput("ssgsea_table"),

      # Button to begin table generation
      actionButton("begin", "Begin")
    ),
    tabPanel(
      "Heatmap",
      sidebarPanel(
        "Create Heatmap"
      ),

      # Button to begin heatmap generation
      actionButton("genHeatmap", "Create Heatmap"),

      # Displays generated heatmaps
      plotOutput("heatmap_result", height = "750")
    ),
    tabPanel(
      "Boxplots",
      sidebarPanel(
        "Create Boxplots",

        # Button for displaying selected profiles
        actionButton("selectProfiles", "Profiler Selection"),
        conditionalPanel(
          condition = "input.selectProfiles % 2 != 0",
          checkboxGroupInput("box_profiles", "Select Profiles:",
            choices = names(TBsignatures)
          ),
        )
      ),

      # Button to generate boxplots
      actionButton("genBoxplots", "Create Boxplots"),

      # Displays generated boxplots
      plotOutput("boxplot_result", height = "200")
    )
  )
)
