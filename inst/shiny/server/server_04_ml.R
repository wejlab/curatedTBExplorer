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

  vals$statusList <- vals$mlList$TBStatus

  # Replaces values in TBStatus as TBYes if it matches PTB. Replaces as TBNo if not
  vals$mlList$TBStatus <- factor(ifelse(vals$mlList$TBStatus == "PTB", "TBYes", "TBNo"))

  View(vals$mlList$TBStatus)
  # Running DE_analyze function from BATCHQC
  vals$DE <- DE_analyze(vals$mlList, 'limma', "Study", "TBStatus", 'log_assay1_cpm')

  View(vals$DE)

  ##########################################################################
  # NEED TO ADD ERROR HANDLING HERE SO WE SKIP THIS LAPPLY IF vals$filtered
  # HAS A LIST IN IT LESS THAN 500 (these are just like notes btw)
  ##########################################################################

  # Filters out when padj is less than or equal to 0.05
  vals$filtered <- lapply(vals$DE, function(df) {
    df %>%
      filter(padj <= 0.05)
  })

  # Sorts by log2FoldChange
  vals$filtered <- lapply(vals$filtered, function(df) {
    df %>%
      arrange(abs(log2FoldChange))
  })

  # Only keeps highest 500 values (Is this highest 500 log2FoldChange )
  vals$filtered <- lapply(vals$filtered, function(df) {
    df %>%
      slice_head(n = input$featureSelectionCount)
  })

  # Filters Summarized Experiment so only coinciding genes get kept
  filtered_genes <- rownames(vals$filtered$TBStatusTBYes)
  # vals$mlList <-
  limitedSE <- vals$mlList[filtered_genes, , drop = FALSE]

  # View(limitedSE)
  View(vals$mlList)

  # Now that I'm looking back at this line, i don't know what happens with subsetByStudy
  # subsetByStudy <- colData(limitedSE)[colData(limitedSE)$Study %in% selectedTrainingList, , drop = FALSE]

  rv$trainingSE <- limitedSE[, colData(limitedSE)$Study %in% selectedTrainingList]
  View(rv$trainingSE)

  # Now that I'm looking back at this line, i don't know what happens with subsetByStudy
  # subsetByStudy <- colData(limitedSE)[colData(limitedSE)$Study %in% selectedTestingList, , drop = FALSE]

  rv$testingSE <- limitedSE[, colData(limitedSE)$Study %in% selectedTestingList]
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
  }
})

# Just for checking work
reactive({
  # View(names$SEList)
})

# Code for Random Forests
observeEvent(input$continueRF, {
  rfSE <- rv$trainingSE

  # Setting control settings for random forest model
  control <- trainControl(
    method = "cv",
    number = input$foldCount,
    verboseIter = TRUE,
    classProbs = TRUE
  )

  # Sets the different node sizes so we can tell which node count works best
  nodesize <- seq(1, 51, 10)

  # Random Viewing stuff to check my work
  View(vals$mlList)
  View(rfSE)
  View(rfSE$TBStatus)
  View(factor(rfSE$TBStatus))
  View(rfSE@assays@data@listData$log_assay1_cpm)

  rfData <- cbind(TBStatus = rfSE$TBStatus, t(rfSE@assays@data@listData$log_assay1_cpm))
  View(rfData)

  # Repeats training for each nodesize
  acc <- sapply(nodesize, function(ns) {
    # Trains random forest model
    train(
      TBStatus ~ ., # String that tells which column to look into for outcome
      method = "rf",# selects random forests method
      ######## REALLY MIGHT NEED TO APPEND TBSTATUS ROW TO DATAFRAME
      data = rfData, # Should be a dataframe containing all the data
      trControl = control,
      tuneGrid = data.frame(mtry = 2),
      nodesize = ns)$results$Accuracy
  })

  # Plots how accurate the random forest is depending on how many trees used
  plot(nodesize, acc)

  # confusionMatrix(predict)
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
