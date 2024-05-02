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
  tryCatch({

    # Gives warning if selectedTrainingData or selectedTesting Data is empty
    if (length(input$selectedTrainingData) <= 0) {
      showNotification("Please select studies for training", type = "warning")
    } else if (length(input$selectedTestingData) <= 0) {
      showNotification("Please select studies for testing", type = "warning")
    } else {
      selectedTrainingList <- input$selectedTrainingData
      selectedTestingList <- input$selectedTestingData

      # View(selectedTrainingList)
      # View(selectedTestingList)

      # Replaces values in TBStatus as TBYes if it matches PTB. Replaces as TBNo if not
      vals$statusList <- vals$mlList$TBStatus

      # Replaces values in TBStatus as TBYes if it matches PTB. Replaces as TBNo if not
      vals$mlList$TBStatus <- factor(ifelse(vals$mlList$TBStatus == "PTB", "TBYes", "TBNo"))

      # Running DE_analyze function from BATCHQC
      vals$DE <- DE_analyze(vals$mlList, 'limma', "Study", "TBStatus", 'log_assay1_cpm')

      # View(vals$DE)

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


      # View(rv$trainingSE)
      # View(rv$testingSE)
      showNotification("Dataset Confirmed", type = "message")
    }
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
})

# Updates mlList and Selectize Inputs every time SEList is updated
observeEvent(vals$SEList, {
  if (!is.null(vals$SEList)) {
    vals$mlList <- vals$SEList
    study_info <- colData(vals$mlList)$Study
    unique_study_values <- unique(study_info)
    updateSelectizeInput(session, "selectedTrainingData", choices = unique_study_values)
    updateSelectizeInput(session, "selectedTestingData", choices = unique_study_values)
  }
})

# Sets mlList to reactive
mlList <- reactive({
  if (!is.null(vals$SEList)) {
    vals$SEList
  } else {
    NULL
  }
})

# Code for Random Forests
observeEvent(input$continueRF, {
  tryCatch({
    withProgress(message = "Training Model...", value = 0, {
      # Setting control settings for random forest model
      control <- trainControl(
        method = "cv",
        number = input$foldCount
        # verboseIter = TRUE,
        # classProbs = TRUE
      )

      rfModel <- caret::train(
        TBStatus ~ .,
        data = rv$trainingData,
        method = "rf",
        tuneGrid = data.frame(mtry = input$mtryInput),
        nodesize = input$nodeSize,
        ntree = input$numTrees,
        trControl = control
      )

      rfImportance <- varImp(rfModel)
      importance <- rfImportance
      View(rfImportance)

      output$rfImportancePlot <- renderPlot({
        plot(rfImportance, main = "Random Forest Importance Plot")
      })

      sorted_data <- importance$importance[order(importance$importance$Overall, decreasing = TRUE), , drop = FALSE]

      # View(sorted_data)
      # View(as.data.frame(sorted_data))
      top_five <- sorted_data[1:5, , drop = FALSE]
      # View(top_five)
      top_genes <- sorted_data[1:10, , drop = FALSE]
      # View(top_genes)
      genes_above_90 <- importance$importance[importance$importance$Overall >= 90, , drop = FALSE]
      genes_above_80 <- importance$importance[importance$importance$Overall >= 80, , drop = FALSE]
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
        TBsignatures <- c(TBsignatures, list(TopFive_RF = top_five_list@unlistData), list(TopGenes_RF = top_genes_list@unlistData), list(GenesAbove90_RF =genes_above_90_list@unlistData), list(GenesAbove80_RF = genes_above_80_list@unlistData))
      })

      observe({
        TBsignatures <- TBsignatures_reactive()
        rv$TBsignatures_reactive <- TBsignatures_reactive()
      })

      showNotification("Finished Generating Random Forest Model", type = "message")
    })
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
})

# Code for Support Vector Machines
observeEvent(input$continueSVM, {
  tryCatch({
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
    View(importance)
    output$svmImportancePlot <- renderPlot({
      plot(importance, main = "Importance Plot")
    })


    #gene selection
    #included are the top 5, ten, any genes above 90, and any above 80, for comparison purposes
    #grabs the top genes from the importance matrix
    sorted_data <- importance$importance[order(importance$importance$TBYes, decreasing = TRUE), ]
    View(as.data.frame(sorted_data))
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
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
})
# Code for Elastic Net Regression
observeEvent(input$continueEN, {
  tryCatch({
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
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
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
  tryCatch({

  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
})
