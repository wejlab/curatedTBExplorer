# Code For General Settings And Selecting Datasets
rv <- reactiveValues(
  # Holds the users' choices for outcomes
  # Still need to figure out the format that this will come in
  outcomeChoice1 = NULL,
  outcomeChoice2 = NULL,

  # Results of user choice
  trainingSE = NULL,
  testingSE = NULL,
)

# Updates outcome choice 1 reactive based on user selection
outcomeChoice1 <- reactive({
  input$oc1
})

# Updates outcome choice 2 reactive based on user selection
outcomeChoice2 <- reactive({
  input$oc2
})

# Splits the SEList based on selected training and testing data
observeEvent(input$confirmDataset, {
  selectedTrainingList <- input$selectedTrainingData
  selectedTestingList <- input$selectedTestingData

  subsetByStudy <- colData(vals$SEList)[colData(vals$SEList)$Study %in% selectedTrainingList, , drop = FALSE]
  rv$trainingSE <- vals$SEList[, colData(vals$SEList)$Study %in% selectedTrainingList]
  # View(rv$trainingSE)

  subsetByStudy <- colData(vals$SEList)[colData(vals$SEList)$Study %in% selectedTestingList, , drop = FALSE]
  rv$testingSE <- vals$SEList[, colData(vals$SEList)$Study %in% selectedTestingList]
  # View(rv$testingSE)
})

# Updates displayed study data to allow user to select
observe({
  if (!is.null(vals$SEList)) {
    # will need to change to use please' code output
    mlList <- vals$SEList
    # grab the unique studies from the mlList
    study_info <- colData(mlList)$Study
    unique_study_values <- unique(study_info)
    updateSelectizeInput(session, "selectedTrainingData", choices = unique_study_values)
    updateSelectizeInput(session, "selectedTestingData", choices = unique_study_values)
    # setdiff
    View(unique_study_values)


    #progression vs non progression
    #Ptb vs ltbi or other conditions
    #treatment response info? -> reoccurance or not
    #HIV vs non HIV?
    #batch needs to be a factor -> study needs to be present
    #conditions should also be a factor, ptb, ltbi -> user will choose this

    #note that "study" and "tbstatus" are going to be available for user input -> need to change here
    vals$DE <- DE_analyze(mlList, 'limma', "Study", "TBStatus", 'log_assay1_cpm')
    View(vals$DE)

  }
})

# Just for checking work
reactive({
  # View(names$SEList)
})

# Code for Random Forests
observeEvent(input$continueRF, {
  # Might need to check the SEList
  # DE_analyze(vals$SEList, 'limma', "logCPM")
  View(vals$SEList)
  View(rv$trainingSE)
})

# test <- DE_analyze(vals$SEList, 'limma', )


# Code for Support Vector Machines










observeEvent(input$continueSVM, {

})
# Code for Elastic Net Regression






observeEvent(input$continueEN, {

})

# Code for Neural Networks
# Define server logic for the "Machine Learning" tab
server <- function(input, output, session) {
  # Define a reactive value to store the trained model
  trained_model <- reactiveVal(NULL)

  # Function to train the neural network
  train_neural_network <- function() {
    # Place for neural network training code here
    # This will involve defining and training a neural network model using the specified parameters

    # For demonstration purposes, let's just print a message indicating training started
    print("Neural network training started...")

    # Simulate training process for demonstration
    Sys.sleep(5)  # Simulate training process taking 5 seconds

    # After training is complete, store the trained model
    trained_model(list(
      num_layers = input$num_layers,
      num_neurons = input$num_neurons,
      learning_rate = input$learning_rate,
      epochs = input$epochs,
      batch_size = input$batch_size
    ))

    # Print a message indicating training completed
    print("Neural network training completed.")
  }

  # Observer to trigger neural network training when the button is clicked
  observeEvent(input$train_nn, {
    train_neural_network()
  })

  # Output to display training progress or results
  output$nn_output <- renderPrint({
    # If the model is trained, display the training configuration
    if (!is.null(trained_model())) {
      cat("Neural Network Configuration:\n")
      cat(paste("Number of Hidden Layers:", trained_model()$num_layers), "\n")
      cat(paste("Number of Neurons per Hidden Layer:", trained_model()$num_neurons), "\n")
      cat(paste("Learning Rate:", trained_model()$learning_rate), "\n")
      cat(paste("Number of Epochs:", trained_model()$epochs), "\n")
      cat(paste("Batch Size:", trained_model()$batch_size), "\n")
    } else {
      # If the model is not trained yet, display a message
      "Neural network not trained yet."
    }
  })
}

observeEvent(input$continueNN, {

})
