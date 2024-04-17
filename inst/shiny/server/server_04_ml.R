# Code For General Settings And Selecting Datasets

#these observe blocks grab the available inputs for the prediction
#they then dynamically change the outcome tabs as necessary, including an "all" option for oc2
observe({
  if (!is.null(vals$mlList)) {
    col_data <- colData(vals$mlList)

    #filter out columns that are ALL NA values (some NA values are allowed)
    rv$non_na_cols <- col_data[, !apply(col_data, 2, function(x) all(is.na(x)))]
    updateSelectInput(session, "mainPredictor", choices = names(rv$non_na_cols)[!(names(rv$non_na_cols) %in% c("Age", "PatientID"))])
  }
})

#if mainPredictor updates, the oc1 and oc2 dropdowns change accordingly
observeEvent(input$mainPredictor, {
  rv$selected_main_predictor <- input$mainPredictor
  selected_column <- rv$non_na_cols[[rv$selected_main_predictor]]
  non_na_values <- na.omit(selected_column)

  updateSelectInput(session, "oc1", choices = unique(non_na_values))
})

#oc2 value shouldn't be oc1, this block ensures this isn't possible
observeEvent(input$oc1, {
  selected_column <- rv$non_na_cols[[rv$selected_main_predictor]]
  non_na_values <- na.omit(selected_column)

  oc1_val <- input$oc1
  if(oc1_val != ""){
    oc2_choices <- setdiff(non_na_values, oc1_val)
  } else {
    oc2_choices <- non_na_values
  }

  if (length(oc2_choices) > 1) {
    oc2_choices <- c(oc2_choices, "All")
  }

  updateSelectInput(session, "oc2", choices = oc2_choices)
})

# observeEvent(input$oc2, {
#   if(input$oc2 == "All") {
#     # If oc2 is "All", compare oc1 to all other values in vals$mlList[[mainPredictor]]
#     # oc1_value <- input$oc1
#     # all_values <- unique(vals$mlList[[input$mainPredictor]])
#     # all_values <- all_values[all_values != oc1_value]  # Exclude oc1 from comparison
#     # vals$mlList[[input$mainPredictor]] <- factor(ifelse(vals$mlList[[input$mainPredictor]] == oc1_value, "Yes", "Ignore"))
#     vals$mlList[[input$mainPredictor]] <- factor(ifelse(vals$mlList[[input$mainPredictor]] == input$oc1, "Yes", "No"))
#   } else {
#     # If oc2 is not "All", compare oc1 to oc2
#     oc1_value <- input$oc1
#     oc2_value <- input$oc2
#     vals$mlList[[input$mainPredictor]] <- factor(ifelse(vals$mlList[[input$mainPredictor]] == oc1_value, "Yes", ifelse(vals$mlList[[input$mainPredictor]] == oc2_value, "No", "Ignore")))
#   }
# })


#
# observeEvent(input$oc2, {
#   # print(input$oc2)
#
#   if(input$oc2 == "All") {
#     print("test")
#     main_val <- input$mainPredictor
#     vals$mlList$selection <- factor(ifelse(vals$mlList[[main_val]] == input$oc1, "Yes", "No"))
#     View(vals$mlList$selection)
#   } else {
#     print("not all")
#   }
#
# })



#do we still need this, as we update outcomeChoice1 and 2 below? - Andrew
rv <- reactiveValues(
  # Holds the users' choices for outcomes
  # Still need to figure out the format that this will come in
  outcomeChoice1 = NULL,
  outcomeChoice2 = NULL,

  # Results of user choice
  trainingSE = NULL,
  testingSE = NULL,
)

observeEvent(vals$SEList, {
  if (!is.null(vals$SEList)) {
    vals$mlList <- vals$SEList
    study_info <- colData(vals$mlList)$Study
    unique_study_values <- unique(study_info)
    updateSelectizeInput(session, "selectedTrainingData", choices = unique_study_values)
    updateSelectizeInput(session, "selectedTestingData", choices = unique_study_values)
  }
})

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

  View(selectedTrainingList)
  View(selectedTestingList)

  vals$statusList <- vals$mlList$TBStatus
  # Replaces values in TBStatus as TBYes if it matches PTB. Replaces as TBNo if not


  # if(outcomeChoice2 == "All"){
  #   # vals$mlList$selection <- factor(ifelse(vals$mlList$input$mainPredictor == input$oc1, "Yes", "No"))
  #   View(vals$mlList[[input$mainPredictor]])
  #   # vals$mlList$selection <- factor(ifelse(vals$mlList[[input$mainPredictor]] == input$oc1, "Yes", "No"))
  #   vals$mlList$selection <- factor(ifelse(as.character(vals$mlList[[input$mainPredictor]]) == input$oc1, "Yes", "No"))
  #   print("test")
  # } else {
  #   print("wrong input")
  # }

  # vals$mlList$TBStatusTrue <- factor(ifelse(vals$mlList$TBStatus == "PTB", "TBYes", "TBNo"))
  # vals$mlList$TBStatusFalse <- factor(ifelse(vals$mlList$TBStatus == "LTBI", "TBYes", "TBNo"))

  # View(vals$mlList$TBStatus)
  # Running DE_analyze function from BATCHQC
  # vals$DE <- DE_analyze(vals$mlList, 'limma', "Study", "TBStatus", 'log_assay1_cpm')
  print(input$oc1)
  print(input$oc2)


  #########################
  #here we need to grab Everything if the value is All, but only compare between two things if else
  # if(input$oc2 == "All"){
  #   vals$mlList[[input$mainPredictor]] <- factor(ifelse(vals$mlList[[input$mainPredictor]] == input$oc1, "Yes", "No"))
  # } else {
  #   print("not All")
  # }

  if(input$oc2 == "All") {
    # If oc2 is "All", compare oc1 to all other values in vals$mlList[[mainPredictor]]
    # oc1_value <- input$oc1
    # all_values <- unique(vals$mlList[[input$mainPredictor]])
    # all_values <- all_values[all_values != oc1_value]  # Exclude oc1 from comparison
    # vals$mlList[[input$mainPredictor]] <- factor(ifelse(vals$mlList[[input$mainPredictor]] == oc1_value, "Yes", "Ignore"))
    vals$mlList[[input$mainPredictor]] <- factor(ifelse(vals$mlList[[input$mainPredictor]] == input$oc1, "Yes", "No"))
  } else {
    # If oc2 is not "All", compare oc1 to oc2
    oc1_value <- input$oc1
    oc2_value <- input$oc2
    vals$mlList[[input$mainPredictor]] <- factor(ifelse(vals$mlList[[input$mainPredictor]] == oc1_value, "Yes", ifelse(vals$mlList[[input$mainPredictor]] == oc2_value, "No", "Ignore")))
  }


  View(vals$mlList[[input$mainPredictor]])
  vals$DE <- DE_analyze(vals$mlList, 'limma', "Study", input$mainPredictor, 'log_assay1_cpm')
  print("test")
  View(vals$DE)

  # Filters out when padj is less than or equal to 0.05
  vals$filtered <- lapply(vals$DE, function(df) {
    df %>%
      filter(padj <= 0.05)
  })

  # Prevents the list of genes from going lower than 500
  if(length(vals$filtered$TBStatusTBYes$padj) < 500) {
    vals$filtered <- vals$DE
  }

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
  # View(vals$mlList)

  # Now that I'm looking back at this line, i don't know what happens with subsetByStudy
  # subsetByStudy <- colData(limitedSE)[colData(limitedSE)$Study %in% selectedTrainingList, , drop = FALSE]

  rv$trainingSE <- limitedSE[, colData(limitedSE)$Study %in% selectedTrainingList]
  # View(rv$trainingSE)

  # Now that I'm looking back at this line, i don't know what happens with subsetByStudy
  # subsetByStudy <- colData(limitedSE)[colData(limitedSE)$Study %in% selectedTestingList, , drop = FALSE]

  rv$testingSE <- limitedSE[, colData(limitedSE)$Study %in% selectedTestingList]
  # View(rv$testingSE)

  #data loaded for training
  training_assay_data <- rv$trainingSE@assays@data@listData$log_assay1_cpm
  col_data <- colData(rv$trainingSE)
  col_data$TBStatus <- factor(col_data$TBStatus, levels = c("TBYes", "TBNo"))
  #data is our training dataframe
  rv$trainingData <- data.frame(TBStatus = col_data$TBStatus, t(training_assay_data))
  rv$trainingData$TBStatus <- factor(rv$trainingData$TBStatus, levels = c("TBYes", "TBNo"))

  #data for testing
  testing_assay_data <- rv$testingSE@assays@data@listData$log_assay1_cpm
  testing_col_data <- colData(rv$testingSE)
  testing_col_data$TBStatus <- factor(testing_col_data$TBStatus, levels = c("TBYes", "TBNo"))
  rv$testData <- data.frame(TBStatus = testing_col_data$TBStatus, t(testing_assay_data))
  rv$testData$TBStatus <- factor(rv$testData$TBStatus, levels = c("TBYes", "TBNo"))

})

mlList <- reactive({
  if (!is.null(vals$SEList)) {
    vals$SEList
  } else {
    NULL
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

  withProgress(message = "Training Model...", value = 0, {
    progress <- 0
    rfModel <- caret::train(
      TBStatus ~ .,
      data = rv$trainingData,
      method = "rf",
      tuneGrid = data.frame(mtry = 2),
      nodesize = input$nodeSize,
      ntree = input$numTrees,
      trControl = control)

    rfImportance <- varImp(rfModel)

    output$rfImportancePlot <- renderPlot({
      plot(rfImportance)
    })
    showNotification("Finished Generating Random Forest Model", type = "message")
  })

################################################################################
# COMMENTED OUT SINCE IT ONLY DETECTS BEST NODESIZE
################################################################################
  # # Sets the different node sizes so we can tell which node count works best
  # nodesize <- seq(1, 51, 1)
  # acc <- NULL
  # # Repeats training for each nodesize
  # withProgress(message = "Training Model...", value = 0, {
  #   progress <- 0
  #   len <- length(nodesize)
  #   acc <- sapply(nodesize, function(ns) {
  #
  #     progress <- progress + 1
  #     print("error in incProgress")
  #     incProgress(progress / len, detail = paste("Training Node-size", ns))
  #
  #     # Trains random forest model
  #     return (caret::train(
  #       TBStatus ~ ., # String that tells which column to look into for outcome
  #       method = "rf",# selects random forests method
  #       data = rv$trainingData, # Should be a dataframe containing all the data
  #       trControl = control,
  #       tuneGrid = data.frame(mtry = 2),
  #       nodesize = ns)$results$Accuracy)
  #   })
  #
  #   incProgress(len / len, message = "Finished Training")
  #
  # })
  #
  # # Plots how accurate the random forest is depending on how many trees used
  # output$rfNodeSizePlot <- renderPlot({
  #   plot(nodesize, acc)
  #   lines(nodesize, acc, col = "blue")
  # })
################################################################################
})


# Code for Support Vector Machines
observeEvent(input$continueSVM, {
    if(input$kernelType == "Linear"){
      kType <- "svmLinear"
    } else if(input$kernelType == "Radial"){
      kType <- "svmRadial"
    } else {
      kType <- "svmPoly"
    }
    #cross validation and SVM training
    ctrl <- trainControl(method = "cv", number = input$foldCount)
    svm_model <- caret::train(TBStatus ~ .,
                       data = rv$trainingData,
                       method = kType,
                       trControl = ctrl)
    importance <- varImp(svm_model)
    # View(importance)
    output$svmImportancePlot <- renderPlot({
      plot(importance, main = "Importance Plot")
    })


    #gene selection
    #included are the top 5, ten, any genes above 90, and any above 80, for comparison purposes
    #grabs the top genes from the importance matrix
    #tbYes will need to change dependent on user selection further up -> will basically be OC1
    sorted_data <- importance$importance[order(importance$importance$TBYes, decreasing = TRUE), ]
    #select the top 5 genes after sorting
    top_five <- sorted_data[1:5, , drop = FALSE]
    # View(top)
    #select the top 10 genes after sorting
    top_genes <- sorted_data[1:10, , drop = FALSE]
    # View(top_genes)
    #select any genes which are greater than 90
    genes_above_90 <- importance$importance[importance$importance$TBYes >= 90, , drop = FALSE]
    # View(genes_above_90)
    #and select any genes which are greater than 80
    genes_above_80 <- importance$importance[importance$importance$TBYes >= 80, , drop = FALSE]
    # View(genes_above_80)


    #this sends the identified genes to the TBsignatureprofiler
    TBsignatures_reactive <- reactive({
      top_five <- as.list(rownames(top_five))
      top_five_list <- CharacterList(top_five)
      top_genes <- as.list(rownames(top_genes))
      top_genes_list <- CharacterList(top_genes)
      genes_above_90 <- as.list(rownames(genes_above_90))
      genes_above_90_list <- CharacterList(genes_above_90)
      genes_above_80 <- as.list(rownames(genes_above_80))
      genes_above_80_list <- CharacterList(genes_above_80)
      TBsignatures <- c(TBsignatures, list(TopFive = top_five_list@unlistData), list(TopGenes = top_genes_list@unlistData), list(GenesAbove90 =genes_above_90_list@unlistData), list(GenesAbove80 = genes_above_80_list@unlistData))
    })
    observe({
      TBsignatures <- TBsignatures_reactive()
      rv$TBsignatures_reactive <- TBsignatures_reactive()
    })


    #create predictions based on the testing data/ svm training
    predictions <- predict(svm_model, rv$testData)
    # View(predictions)
    print(predictions)

    #confusion matrix
    confusion_matrix <- table(predictions, rv$testData$TBStatus)
    output$svmMatrix <- renderPlot({
      plot(confusion_matrix, main = "Confusion Matrix", cex.main = 1.2)
    })
    print(confusion_matrix)

    #accuracy
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    print(paste("Accuracy In Testing:", accuracy))

})
# Code for Elastic Net Regression
observeEvent(input$continueEN, {
  ctrl <- trainControl(method = "cv", number = input$foldCount)
  elastic_net <- caret::train(TBStatus ~ .,
                                    data = rv$trainingData,
                                    method = "glmnet",
                                    trControl = ctrl,
                                    tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.001, 1, length = 100)))

  importance <- varImp(elastic_net)
  output$elasticNetImportancePlot <- renderPlot({
    plot(importance)
  })

  # Create predictions based on the testing data/ elastic net training
  predictions <- predict(elastic_net, rv$testData)
  print(predictions)

})


# Code for Neural Networks
# Define server logic for the "Machine Learning" tab
server <- function(input, output, session) {
  # Define a reactive value to store the trained model
  trained_model <- reactiveVal(NULL)

  # Function to train the neural network
  train_neural_network <- function() {
    # This will involve defining and training a neural network model using the specified parameters
    nn_caret <- caret::train(Species~., data = rv$trainingData,
                             method = "nnet", linout = TRUE,
                             trace = FALSE)
                             ps <- predict(nn_caret, rv$trainingData)
                             confusionMatrix(ps, rv$trainingData$Species)$overall["Accuracy"]
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
