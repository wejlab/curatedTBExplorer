# TBSigProfiler tab page
tabPanel(
  "TBSigProfiler",
  tabsetPanel(
    tabPanel(
      "Profiler",
        sidebarPanel(
        selectInput("assay", "Select Assay for Profiler:",
                    choices = "assay1"
        ),
        selectInput("algorithm", "Select Algorithm for Profiler:",
                    choices = c("GSVA", "ssGSEA")
        ),

        pickerInput(
          "profiles", "Select Profiles", choices = names(TBsignatures),
          options = list("actions-box" = TRUE),
          multiple = TRUE, selected = NULL),
        # Button to begin table generation
        actionButton("begin", "Run TBSignatureProfiler")
      ),

      # Displays ssgsea table
      DTOutput("ssgsea_table"),

    ),
    tabPanel(
      "Heatmap",
      sidebarPanel(
        # "Create Heatmap",
        # Button to begin heatmap generation
        selectInput("heatmapType", "Select Type of Heatmap",
                    choices = c("All Signatures","Single Signature")
        ),
        conditionalPanel(
          condition = "input.heatmapType == 'Single Signature'",
          pickerInput(
            "signatures", "Select Signature(s)", choices = names(TBsignatures),
            options = list("actions-box" = TRUE),
            multiple = TRUE, selected = NULL),
          ),
          selectInput("column", "Select Columns:",
            choices = NULL
          ),
          selectInput("annotations", "Show Annotations?",
                      choices = c("TRUE", "FALSE")
          ),
          actionButton("genHeatmap", "Create Heatmap")
        ),
      # Displays generated heatmaps
      plotOutput("heatmap_result", height = "750")
    ),

    tabPanel(
      "Boxplots",
      sidebarPanel(
        # "Create Boxplots",
        pickerInput(
          "box_profiles", "Select Signature(s)", choices = names(TBsignatures),
          options = list("actions-box" = TRUE),
          multiple = TRUE, selected = NULL),
        selectInput("boxCovariate", "Covariate", choices = NULL),
        actionButton("genBoxplots", "Create Boxplots"),
      ),
      # Displays generated boxplots
      # mainPanel(
        plotOutput("boxplot_result")  # Set height to auto
      # )
    )
  )
)

