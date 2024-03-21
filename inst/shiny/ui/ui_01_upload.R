# Select/Download tab page
tabPanel(
  "Select/Download",

  # Upload Icon by tab title
  icon = icon("download", class = "fa-solid fa-download" , lib = "font-awesome"),

  # Layout panel to seperate page into columns
  fluidRow(
    column(
      3,

      # Upload file block
      # fileInput("file1", "Upload file"),

      # Table Options
      h5("Table Options: "),

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
      ),
    ),
    column(
      9,
      # Dynamic table for selection of studies
      DTOutput("study_table"),
      HTML("<br>"),

      # Button to continue and download selected studies
      div(style = "display:inline-block; float: right", actionButton("continue", "Continue"))
    )
  )
)
