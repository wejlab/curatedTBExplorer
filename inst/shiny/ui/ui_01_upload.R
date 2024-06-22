# Select/Download tab page
tabPanel(
  "Select/Download",

  # Upload Icon by tab title
  icon = icon("download", class = "fa-solid fa-download" , lib = "font-awesome"),
  sidebarLayout(
    sidebarPanel(
      div(style = "margin-top: -20px; border-bottom: 1px solid #ccc",
          h3("Table Display Options: ")
      ),

      # Checkbox inputs for choosing displayed columns on the table
      checkboxInput("filterDSPlatform", "Platform", value = FALSE, width = NULL),
      checkboxInput("filterDSGeoRegion", "GeographicalRegion", value = FALSE, width = NULL),
      checkboxInput("filterDSTissue", "Tissue", value = FALSE, width = NULL),
      checkboxInput("filterDSAge", "Age", value = FALSE, width = NULL),
      checkboxInput("filterDSHIV", "HIV Status", value = FALSE, width = NULL),
      checkboxInput("filterDSMethod", "Diagnosis Method", value = FALSE, width = NULL),
      checkboxInput("filterDSControl", "Control", value = FALSE, width = NULL),
      checkboxInput("filterDSLTBI", "LTBI", value = FALSE, width = NULL),
      checkboxInput("filterDSPTB", "PTB", value = FALSE, width = NULL),
      checkboxInput("filterDSOD", "OD", value = FALSE, width = NULL),
      checkboxInput("filterDSTotal", "Total", value = FALSE, width = NULL),
      checkboxInput("filterDSType", "General Type", value = FALSE, width = NULL),

      # Download options button
      actionButton("downloadOptions", "Download Options"),

      # Conditional panel for more options
      conditionalPanel(
        condition = "input.downloadOptions % 2 != 0",

        # Checkbox inputs for selecting download options
        checkboxInput("dLMultiThread", "Multithread Downloading", value = TRUE, width = NULL),
        checkboxInput("dLCurated", "Curated Data Only", value = TRUE, width = NULL),
        checkboxInput("dLLocal", "Local Download", value = FALSE, width = NULL),

        # Button to clear the local download
        actionButton("clearLocalDownload", "Clear Local Download")
      )

    ),

    # sidebarPanel(
    #   conditionalPanel(
    #     condition = "input.confirmStudiesBtn",
    #
    #     checkboxInput("test", "test", value = TRUE),
    #   )
    # ),

    mainPanel(
      # Layout panel to seperate page into columns
      fluidRow(
        column(
          12,
          # Dynamic table for selection of studies
          DTOutput("study_table"),
          HTML("<br>"),

          # Button to continue and download selected studies
          actionButton("downloadStudiesBtn", "Download Studies", style = "width: 100%"),

          HTML("<hr>"),

          # Displays selectize for selecting dataset to use
          selectizeInput("selectedActiveMAEList", "Select Datasets To Use", choices = list(), multiple = TRUE, width = "100%"),
          actionButton("confirmStudiesBtn", "Confirm Selected Studies"),

          #this panel appears after studies are confirmed, allow user to select their batch conditions
          conditionalPanel(
            condition = "input.confirmStudiesBtn > 0",
            selectizeInput("selectedCovars", "Confirm Covariates for Batch Correction", choices = list(), multiple = TRUE, width = "100%"),
            actionButton("confirmCovarsBtn", "Confirm Covariates")
          )
          # textOutput("test")
        )
      )
    )
  )
)
