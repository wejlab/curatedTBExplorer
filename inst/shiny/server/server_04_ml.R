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

  #############################################################################
  # NEED TO ADD SOMETHING TO FILTER OUT ALL THE COLUMNS/ROWS WITH UNNEEDED GENE DATA
  # SHOULD ALSO HOPEFULLY BE THE SPOT WHERE DIFFERENTIAL ANALYSIS HAPPENS.
  #############################################################################

  # Now that I'm looking back at this line, i don't know what happens with subsetByStudy
  subsetByStudy <- colData(vals$SEList)[colData(vals$SEList)$Study %in% selectedTrainingList, , drop = FALSE]

  tempTrainingSE <- vals$SEList[, colData(vals$SEList)$Study %in% selectedTrainingList]

  rv$trainingSE <- tempTrainingSE
  # View(rv$trainingSE)

  # Now that I'm looking back at this line, i don't know what happens with subsetByStudy
  subsetByStudy <- colData(vals$SEList)[colData(vals$SEList)$Study %in% selectedTestingList, , drop = FALSE]

  rv$testingSE <- vals$SEList[, colData(vals$SEList)$Study %in% selectedTestingList]
  # View(rv$testingSE)




})

observe({
  if (!is.null(vals$SEList)) {
    #Update selectize input
    isolate({
      vals$mlList <- vals$SEList
      study_info <- colData(vals$mlList)$Study
      unique_study_values <- unique(study_info)
      updateSelectizeInput(session, "selectedTrainingData", choices = unique_study_values)
      updateSelectizeInput(session, "selectedTestingData", choices = unique_study_values)
    })

    #code for the differetial expression analysis
    isolate({
      vals$mlList$TBStatus <- factor(ifelse(vals$mlList$TBStatus == "PTB", "TBYes", "TBNo"))
      vals$DE <- DE_analyze(vals$mlList, 'limma', "Study", "TBStatus", 'log_assay1_cpm')

      ##########################################################################
      # WE MIGHT NEED TO BIND THE LIST OF GENE RATINGS FROM DE_ANALYZE TO THE
      # LIST OF GENES IN OUR SUMMARIZED EXPERIMENTS
      ##########################################################################

      ##########################################################################
      # NEED TO ADD ERROR HANDLING HERE SO WE SKIP THIS LAPPLY IF vals$filtered
      # HAS A LIST IN IT LESS THAN 500 (these are just like notes btw)
      ##########################################################################

      vals$filtered <- lapply(vals$DE, function(df) {
        df %>%
          filter(padj <= 0.05)
      })

      vals$filtered <- lapply(vals$filtered, function(df) {
        df %>%
          arrange(abs(log2FoldChange))
      })

      vals$filtered <- lapply(vals$filtered, function(df) {
        df %>%
          slice_head(n = 500)
      })
      View(vals$filtered)












      # filtered_genes <- rownames(vals$filtered)
      # View(filtered_genes)
      # # filtered_SEList <- SummarizedExperiment(assays(filtered_SEList))
      #
      # filteredColdata <- vals$SEList[filtered_genes, ]
      # filteredAssays <- assays(vals$SEList[filtered_genes, ])
      # View(filteredColdata)
      # View(filteredAssays)
      # Subset assays from vals$SEList
      # filtered_assays <- assays(vals$SEList)[filtered_genes, , drop=FALSE]
      # # Subset row metadata from vals$SEList
      # filtered_row_metadata <- rowData(vals$SEList)[filtered_genes, , drop=FALSE]
      # # Create a new SummarizedExperiment with filtered assay data and row metadata
      # vals$SEListFiltered <- SummarizedExperiment(assays = filtered_assays, rowData = filtered_row_metadata)
      #
      # # Check if the assay data is preserved
      # View(assays(vals$SEListFiltered))
      #
      #
      # vals$SEListFiltered <- vals$SEList[filtered_genes, ]
      # View(assays(vals$SEList))
      # View(vals$SEListFiltered)
      #
      # vals$assays <- assays(vals$SEListFiltered)
      # View(assays(vals$SEListFiltered))
      # View(assays(vals$SEListFiltered[[1]]))
      # View(assays(vals$SEListFiltered)[[1]])
      # View(SummarizedExperiment(vals$SEListFiltered))
    })
  }
})









# Just for checking work
reactive({
  # View(names$SEList)
})

# Code for Random Forests

# Training to see which nodesize is best for the random forest
# nodesize <- seq(1, 51, 10)
# acc <- sapply(nodesize, function(ns){
#   train(y ~ ., method = "rf", data = mnist_27$train,
#         tuneGrid = data.frame(mtry = 2),
#         nodesize = ns)$results$Accuracy
# })



observeEvent(input$continueRF, {
  # Might need to check the SEList
  # DE_analyze(vals$SEList, 'limma', "logCPM")
  View(vals$SEList)
  View(rv$trainingSE)
})


# Code for Support Vector Machines
observeEvent(input$continueSVM, {
  # assay_data <- SummarizedExperiment(assays(intersectRows(vals$SEListFiltered)))
  # View(vals$filtered[[2]])
  # assay_data <- vals$filtered[[2]]
  # View(assays(vals$SEListFiltered))
  # assay_data <- assay(vals$SEListFiltered)
  # View(assay_data)
  assay_data <- vals$assays
  # View(assay_data$listData)
  View(as.data.frame(assay_data@listData[[4]]))
  # View(assay_data$log_assay1_cpm)
  col_data <- colData(vals$SEListFiltered)

  # Combine assay and colData
  # data <- data.frame(TBStatus = col_data$TBStatus,
  #                    t(assay_data))

  data <- cbind(TBStatus = col_data$TBStatus, t(assay_data))
  View(assay_data)
  View(assay_data[[4]])
  View(data)

  # Define the classes (hard-coded)
  group1 <- c("PTB", "od", "control")
  group2 <- c("lTBI", "od", "control")

  # Perform SVM model training
  ctrl <- trainControl(method = "cv", number = 10)
  print("works")
  svm_model <- train(TBStatus ~ .,
                     data = data,
                     method = "svmLinear",
                     trControl = ctrl)
  print("Works after svm model")
  # Make predictions on testing set
  predictions <- predict(svm_model, testing_data[, -which(names(testing_data) == "TBStatus")])

  # Create confusion matrix
  confusion_matrix <- table(predictions, testing_data$TBStatus)
  print(confusion_matrix)

  # Calculate accuracy
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  print(paste("Accuracy In Testing:", accuracy))
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
