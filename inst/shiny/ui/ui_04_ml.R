# machine learning tab panel
tabPanel(
  "Machine Learning",
  fluidRow(
    column(
      width = 3,
      div(style = "border-bottom: 1px solid #ccc",
          h3("General Training Settings: ")
      ),
      # "General Settings",
      style = "background-color: #EDF1F1;",

      selectInput("covariateCategory", "Category Contrast", choices = list()),
      selectInput("oc1", "Outcome", choices = list()),
      selectInput("oc2", "Compared Outcome", choices = list()),

      selectInput("assaySelection", "Select Assay", choices = list()),  # Add this line for assay selection

      numericInput("featureSelectionCount", "Feature Count", value = 500, min = 0, step = 1),
      numericInput("foldCount", "Cross Validation Folds", value = 10, min = 0, step = 1)
    ),
    column(
      width = 9,
      "Training & Testing Dataset Selection",
      selectizeInput("selectedTrainingData", "Training Datasets", choices = list(), multiple = TRUE),
      selectizeInput("selectedTestingData", "Testing Datasets", choices = list(), multiple = TRUE),
      actionButton("confirmDataset", "Confirm Datasets")
    )
  ),
  tabsetPanel(
    # Contains general settings for the machine learning
    tabPanel(
      "Elastic Net Regression",
      sidebarPanel(
        "Elastic Net Regression",
        actionButton("continueEN", "Continue"),

        sliderInput("enSignatureSize", "Set Signature Size: ", min = 1, max = 100, value = 10),
        actionButton("enTestGeneSig", "Test Gene Signature")
      ),
      mainPanel(
        plotOutput("enImportancePlot", height = "600"),
        plotOutput("enMatrixPlot"),
        tableOutput("enMatrixTable")
      )
    ),
    tabPanel(
      "Neural Networks",
      sidebarPanel(
        "Neural Networks",

        # Input for number of epochs
        numericInput("numEpochs", "Number of Epochs:", value = 100, min = 1, max = 1000),

        # Button to start training
        actionButton("continueNN", "Continue"),

        sliderInput("nnSignatureSize", "Set Signature Size: ", min = 1, max = 100, value = 10),
        actionButton("nnTestGeneSig", "Test Gene Signature")
      ),
      mainPanel(
        plotOutput("nnImportancePlot", height = "600"),
        plotOutput("nnMatrixPlot"),
        tableOutput("nnMatrixTable")
      )
    ),
    tabPanel(
      "Random Forests",
      sidebarPanel(
        "Random Forests",
        numericInput("numTrees", "Number of Trees Generated:", value = 500, min = 1),
        numericInput("nodeSize", "Size of Each Node:", value = 5, min = 1),
        numericInput("mtryInput", "mtry:", value = 2, min = 1),
        actionButton("continueRF", "Continue"),

        sliderInput("rfSignatureSize", "Set Signature Size: ", min = 1, max = 100, value = 10),
        actionButton("rfTestGeneSig", "Test Gene Signature")
      ),
      mainPanel(
        plotOutput("rfImportancePlot", height = "600"),
        plotOutput("rfMatrixPlot"),
        tableOutput("rfMatrixTable")
      )
    ),
    tabPanel(
      "Support Vector Machines",
      sidebarPanel(
        "Support Vector Machines",
        selectInput("kernelType", "Kernel Type", choices = c("Linear", "Radial", "Polynomial")),
        actionButton("continueSVM", "Continue"),

        sliderInput("svmSignatureSize", "Set Signature Size: ", min = 1, max = 100, value = 10),
        actionButton("svmTestGeneSig", "Test Gene Signature")
      ),
      mainPanel(
        plotOutput("svmImportancePlot", height = "600"),
        plotOutput("svmMatrixPlot"),
        tableOutput("svmMatrixTable")
      )
    )
  )
)
