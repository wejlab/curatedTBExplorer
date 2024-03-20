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
                    choices = "assay_curated"
        ),
        selectInput("algorithm", "Select Algorithm for Profiler:",
                    choices = c("GSVA", "ssGSEA")
        ),

        # Button for displaying selected profiles to display
        actionButton("showProfiles", "Profile Selection"),
        # Conditional Panel for displaying selected profiles
        conditionalPanel(
          condition = "input.showProfiles % 2 != 0",

          # Dropdowns box for selecting profile information
          checkboxGroupInput("profiles", "Select Profiles:",
            choices = names(TBsignatures),
            selected = NULL
          ),

          #unfunctional atm
          actionButton("selectAll", "Select All"),
          actionButton("deselectAll", "Deselect All")

        ),
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

          actionButton("showSigs", "Signature Selection"),
          conditionalPanel(
            condition = "input.showSigs % 2 != 0",
            checkboxGroupInput("signatures", "Select Signatures:",
                               choices = names(TBsignatures),
                               selected = NULL
            ),

            actionButton("selectAll", "Select All"),
            actionButton("deselectAll", "Deselect All")
          )
        ),
        actionButton("genHeatmap", "Create Heatmap")
      ),

      # Displays generated heatmaps
      plotOutput("heatmap_result", height = "750")
    ),



    # shiny::tabPanel(
    #   "Boxplots of Individual Signatures",
    #   shiny::sidebarPanel(
    #     shinyWidgets::pickerInput(
    #       "singbox", "Signature(s)", choices = siglist,
    #       options = list("actions-box" = TRUE),
    #       multiple = TRUE, selected = NULL),
    #     shiny::hr(),
    #     shiny::selectInput("singboxcovar", "Covariate", choices = NULL),
    #     shiny::actionButton("singboxplot", "Plot Boxplot(s)")
    #   ),
    #   shiny::mainPanel(
    #     shiny::plotOutput("boxplotind", height = 500)
    #   )
    # ),


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
        # Button for displaying selected profiles
        # actionButton("selectProfiles", "Profiler Selection"),
        # conditionalPanel(
        #   condition = "input.selectProfiles % 2 != 0",
        #   checkboxGroupInput("box_profiles", "Select Profiles:",
        #     choices = names(TBsignatures)
        #   ),
        # )
      ),

      # Button to generate boxplots


      # Displays generated boxplots
      plotOutput("boxplot_result", height = "200")
    )
  )
)

