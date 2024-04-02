#machine learning tab panel
tabPanel(
  "Machine Learing",
  fluidRow(
    column(width = 3,
           "General Settings",
           style = "background-color: #EDF1F1;",
           selectInput("oc1", "Outcome", choices = c("LTBI", "PTB", "Control", "OD")),
           selectInput("oc2", "Compared Outcome", choices = c("LTBI", "PTB", "Control", "OD")),
           numericInput("featureSelectionCount", "Feature Count", value = 500, min = 0, step = 1),
           numericInput("foldCount", "Cross Validation Folds", value = 10, min = 0, step = 1)
           ),
    column(width = 9,
           "Training & Testing Dataset Selection",
           DTOutput("trainingDatasetTable"),
           DTOutput("testingDatasetTable")
           )
  ),
  tabsetPanel(
    # Contains general settings for the machine learning

    tabPanel(
      "Elastic Net Regression",
      sidebarPanel(
        "Elastic Net Regression"
      ),
      actionButton("continueEN", "Continue")

    ),
    tabPanel(
      "Neural Networks",
      sidebarPanel(
        "Neural Networks"
      ),
      actionButton("continueNN", "Continue")
    ),

    tabPanel(
      "Random Forests",
      sidebarPanel(
        "Random Forests"

      ),
      actionButton("continueRF", "Continue")
    ),

    tabPanel(
      "Support Vector Machines",
      sidebarPanel(
        "Support Vector Machines"
      ),
      actionButton("continueSVM", "Continue")
    )
  )
)

