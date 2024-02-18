tabPanel(  #this is our upload page
  "Upload",
  icon = icon("cloud-upload"), #this is the icon that shows up on the navbar next to the tab title
  fluidRow(#might be better to make separated rows out of this
    column(3,
   # Upload file block
          fileInput("file1", "Upload file"),
          h5("Table Options: "), # Probably needs a better name at some point

          # Could probably pull from DataSummary itself, but I don't know how to do that - Alex
          checkboxInput("filterDSStudy", "Study", value = TRUE, width = NULL),
          checkboxInput("filterDSPlatform", "Platform", value = FALSE, width = NULL),
          checkboxInput("filterDSGeoRegion", "GeographicalRegion", value = TRUE, width = NULL),
          checkboxInput("filterDSTissue", "Tissue", value = TRUE, width = NULL),
          checkboxInput("filterDSAge", "Age", value = FALSE, width = NULL),
          checkboxInput("filterDSHIV", "HIV Status", value = TRUE, width = NULL),
          checkboxInput("filterDSMethod", "Diagnosis Method", value = FALSE, width = NULL),
          checkboxInput("filterDSControl", "Control", value = FALSE, width = NULL),
          checkboxInput("filterDSLTBI", "LTBI", value = FALSE, width = NULL),
          checkboxInput("filterDSPTB", "PTB", value = FALSE, width = NULL),
          checkboxInput("filterDSOD", "OD", value = FALSE, width = NULL),
          checkboxInput("filterDSTotal", "Total", value = FALSE, width = NULL)
    ),
    column(9,
           # Select studies block
           DTOutput("study_table"),

           # Continue button that should download the dataset
           actionButton("continue", "Continue") # Probably need to align right - Alex

    )
  )
)
