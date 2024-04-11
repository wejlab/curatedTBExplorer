# machine learning tab panel
tabPanel(
  "Machine Learning",
  fluidRow(
    column(
      width = 3,
      "General Settings",
      style = "background-color: #EDF1F1;",

      ##########################################################################
      # NEED TO CHANGE THE selectInput CHOICES SO THEY INCLUDE MORE OPTIONS
      # LIKE HIV vs NO HIV
      ##########################################################################
      selectInput("oc1", "Outcome", choices = c("LTBI", "PTB", "Control", "OD")),
      selectInput("oc2", "Compared Outcome", choices = c("LTBI", "PTB", "Control", "OD")),
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
        actionButton("continueEN", "Continue")
      ),
      mainPanel(
        plotOutput("elasticNetImportancePlot", width = "100%", height = "5000px")
      )
    ),
    tabPanel(
      "Neural Networks",
      sidebarPanel(
        # Input for number of hidden layers
        numericInput("num_layers", "Number of Hidden Layers:", value = 1, min = 1, max = 10),
        # Input for number of neurons in each hidden layer
        numericInput("num_neurons", "Number of Neurons per Hidden Layer:", value = 10, min = 1, max = 100),
        # Input for learning rate
        numericInput("learning_rate", "Learning Rate:", value = 0.01, min = 0, max = 1, step = 0.01),
        # Input for number of epochs
        numericInput("epochs", "Number of Epochs:", value = 100, min = 1, max = 1000),
        # Input for batch size
        numericInput("batch_size", "Batch Size:", value = 32, min = 1, max = 256),
        # Button to start training
        actionButton("continueNN", "Continue")
      )
    ),
    tabPanel(
      "Random Forests",
      sidebarPanel(
        "Random Forests",
        numericInput("numTrees", "Number of Trees Generated:", value = 500, min = 1),
        numericInput("nodeSize", "Size of Each Node:", value = 5, min = 1),
        actionButton("continueRF", "Continue")
      ),
      mainPanel(
        plotOutput("rfNumTreesPlot", width = "100%", height = "500px"),
        plotOutput("rfNodeSizePlot", width = "100%", height = "500px"),
        shinycssloaders::withSpinner(plotOutput("rfImportancePlot", width = "100%", height = "5000px"))
      )
    ),
    tabPanel(
      "Support Vector Machines",
      sidebarPanel(
        "Support Vector Machines",
        actionButton("continueSVM", "Continue")
      ),
      mainPanel(
        plotOutput("svmImportancePlot", width = "100%", height = "5000px")
      )
    )
  )
)
