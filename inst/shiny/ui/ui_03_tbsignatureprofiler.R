# TBSigProfiler tab page
tabPanel(
  "TBSigProfiler",
  tabsetPanel(
    tabPanel(
      "Profiler",
        sidebarPanel(
        selectInput("selectAssay", "Make Assay",
          choices = c("Log Counts","CPM","Log CPM"),
          selected = "CPM"
        ),
        actionButton("makeAssay", "Create Assays"),

        HTML("<br><br>"),
        # "Select Data For the TBSignatureProfiler",
        # HTML("<br>"),
        # Dropdown box for selecting assay
        # selectInput("profassay", "Select Assay for Profiler",
        #             choices = NULL),
        selectInput("assay", "Select Assay for Profiler:",
                    # choices = c("log_counts", "log_counts_cpm", "counts"),
                    # choices = c("assay_curated", "log_assay_curated", "assay_curated_cpm", "log_assay_curated_cpm"),
                    # selected = "assay_curated"
                    choices = "assay1"
        ),
        selectInput("algorithm", "Select Algorithm for Profiler:",
                    choices = c("GSVA", "ssGSEA")
        ),

        pickerInput(
          "profiles", "Select Profiles", choices = names(TBsignatures),
          options = list("actions-box" = TRUE),
          multiple = TRUE, selected = NULL),
        # # Button for displaying selected profiles to display
        # actionButton("showProfiles", "Profile Selection"),
        # # Conditional Panel for displaying selected profiles
        # conditionalPanel(
        #   condition = "input.showProfiles % 2 != 0",
        #
        #   # Dropdowns box for selecting profile information
        #   checkboxGroupInput("profiles", "Select Profiles:",
        #     choices = names(TBsignatures),
        #     selected = NULL
        #   ),

          #unfunctional atm
        #   actionButton("selectAll", "Select All"),
        #   actionButton("deselectAll", "Deselect All")
        #
        # ),
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
        pickerInput(
          "signatures", "Select Signature(s)", choices = names(TBsignatures),
          options = list("actions-box" = TRUE),
          multiple = TRUE, selected = NULL),
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
      plotOutput("boxplot_result", height = "200")
    )
  )
)

