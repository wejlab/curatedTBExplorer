tabPanel(  #this is our upload page
  "Upload",
  icon = icon("cloud-upload"), #this is the icon that shows up on the navbar next to the tab title
  fluidRow(#might be better to make separated rows out of this
    column(6,
           # Upload file block
           fileInput("file1", "Upload file"),
           # Select studies block
           DTOutput("study_table")
    ),
    column(6,
           # Studies description block
           wellPanel(
             hr(),
             h5("Study Description:"),
             verbatimTextOutput("study_description"),
             # Continue button
             actionButton("continue", "Continue")
           )
    )
  )
)
