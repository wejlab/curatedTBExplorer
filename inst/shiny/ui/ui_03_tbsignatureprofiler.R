tabPanel(
  "TBSigProfiler",
  tabsetPanel(
    tabPanel(
      "Profiler",
      sidebarPanel(
        "Select Data For the TBSignatureProfiler",
        # dropdowns to select the profile information
        checkboxGroupInput("profiles", "Select Profiles:",
                           choices = names(TBsignatures),
                           selected = names(TBsignatures)),

        # dropdown to select assay
        selectInput("assay", "Select Assay:",
                    choices = c("log_counts", "log_counts_cpm", "counts"),
                    selected = "log_counts")
      ),
      # titlePanel(""),
      DTOutput("ssgsea_table"),
      actionButton("begin", "Begin")
    ),

    tabPanel(
      "Heatmap",
      sidebarPanel(
        "Create Heatmap"
      ),
      actionButton("genHeatmap", "Create Heatmap"),
      plotOutput("heatmap_result", height = "750")
    ),

    tabPanel(
      "Boxplots",
      sidebarPanel(
        "Create Boxplots",
        checkboxGroupInput("box_profiles", "Select Profiles:",
                           choices = names(TBsignatures)),
      ),
      actionButton("genBoxplots", "Create Boxplots"),
      plotOutput("boxplot_result", height = "200")
    )
  )
)
