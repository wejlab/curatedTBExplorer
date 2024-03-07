tabPanel(
  "TBSigProfiler",
  tabsetPanel(
    tabPanel(
      "Profiler",
      sidebarPanel(
        "Select Data For the TBSignatureProfiler",
        # dropdowns to select the profile information
        selectInput("profile1", "Select Profile 1:", choices = names(TBsignatures), selected = "Anderson_42"),
        selectInput("profile2", "Select Profile 2:", choices = names(TBsignatures), selected = "Anderson_OD_51"),
        selectInput("profile3", "Select Profile 3:", choices = names(TBsignatures), selected = "Gong_OD_4"),
        # dropdown to select assay
        selectInput("assay", "Select Assay:", choices = c("log_counts", "log_counts_cpm", "counts"), selected = "log_counts")
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
        "Create Boxplots"
      ),
      actionButton("genBoxplots", "Create Boxplots"),
      plotOutput("boxplot_result", height = "200")
    )
  )
)
