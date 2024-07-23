# Code For General Settings And Selecting Datasets
rv <- reactiveValues(
  # Holds the users' choices for outcomes
  # Still need to figure out the format that this will come in
  covariateOutcomeChoice = NULL,
  outcomeChoice1 = NULL,
  outcomeChoice2 = NULL,

  # Results of user choice
  trainingSE = NULL,
  testingSE = NULL,

  # Importance DataFrames
  rfImportance = NULL,
  rfGeneSigNames = NULL,
  rfPredictions = NULL,
  rfConfusionMatrix = NULL,

  enImportance = NULL,
  enGeneSigNames = NULL,
  enPredictions = NULL,
  enConfusionMatrix = NULL,

  svmImportance = NULL,
  svmGeneSigNames = NULL,
  svmPredictions = NULL,
  svmConfusionMatrix = NULL,

  nnImportance = NULL,
  nnGeneSigNames = NULL,
  nnPredictions = NULL,
  nnConfusionMatrix = NULL,

  TBsignatures_reactive = NULL

)

################################################################################################
####################################### SELECTING DATASETS #####################################
################################################################################################

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

      # Grabs the columnData for the selected covariate category (TBStatus or Ethnicity)
      vals$statusList <- vals$mlList@colData@listData[[input$covariateCategory]]
      covarColumn <- vals$mlList@colData@listData[[input$covariateCategory]]

      # Changes the covariate data into factors depending on choice of outcomes
      if(input$oc2 == "All Else") {
        covarColumn <- factor(
          ifelse(covarColumn == input$oc1, input$oc1, "All Else"))
      } else {
        covarColumn <- factor(
          ifelse(covarColumn == input$oc1, input$oc1, ifelse(covarColumn == input$oc2, input$oc2, NA))
        )

        # Subset the summarized experiment so we only keep valid samples (samples without NA)
        keptOutcomes <- !is.na(covarColumn)
        vals$mlList <- vals$mlList[, keptOutcomes]

        vals$mlList$colData[[input$covariateCategory]] <- covarColumn[keptOutcomes]
      }

      # Running DE_analyze function from BATCHQC
      vals$DE <- DE_analyze(vals$mlList, 'limma', "Study", input$covariateCategory, 'log_assay1_cpm')

      # Filters out when padj is less than or equal to 0.05
      vals$filtered <- lapply(vals$DE, function(df) {
        df %>%
          filter(padj <= 0.05)
      })

      # We make generated name because it needs to match the one generated through DE_analyze (covarCategory + Outcome)
      generatedName <- paste0(input$covariateCategory, input$oc1)

      # Prevents the list of genes from going lower than 500
      if(length(vals$filtered[[generatedName]]$padj) < 500) {
        vals$filtered <- vals$DE
      }

      # Sorts by log2FoldChange
      vals$filtered <- lapply(vals$filtered, function(df) {
        df %>%
          arrange(abs(log2FoldChange))
      })

      # Lets user select how many genes to keep
      vals$filtered <- lapply(vals$filtered, function(df) {
        df %>%
          slice_head(n = input$featureSelectionCount)
      })

      # Filters Summarized Experiment so only coinciding genes get kept
      filtered_genes <- rownames(vals$filtered[[generatedName]])

      limitedSE <- vals$mlList[filtered_genes, , drop = FALSE]

      rv$trainingSE <- limitedSE[, colData(limitedSE)$Study %in% selectedTrainingList]

      rv$testingSE <- limitedSE[, colData(limitedSE)$Study %in% selectedTestingList]

      # Data loaded for training
      training_assay_data <- rv$trainingSE@assays@data@listData$log_assay1_cpm
      col_data <- colData(rv$trainingSE)
      col_data[[input$covariateCategory]] <- factor(col_data[[input$covariateCategory]], levels = c(input$oc1, input$oc2))

      # Data is our training dataframe
      rv$trainingData <- setNames(data.frame(col_data[[input$covariateCategory]], t(training_assay_data)), c(input$covariateCategory, colnames(t(training_assay_data))))

      rv$trainingData[[input$covariateCategory]] <- factor(rv$trainingData[[input$covariateCategory]], levels = c(input$oc1, input$oc2))

      # Data for testing
      testing_assay_data <- rv$testingSE@assays@data@listData$log_assay1_cpm
      testing_col_data <- colData(rv$testingSE)
      testing_col_data[[input$covariateCategory]] <- factor(testing_col_data[[input$covariateCategory]], levels = c(input$oc1, input$oc2))
      rv$testData <- setNames(data.frame(testing_col_data[[input$covariateCategory]], t(testing_assay_data)), c(input$covariateCategory, colnames(t(testing_assay_data))))
      rv$testData[[input$covariateCategory]] <- factor(rv$testData[[input$covariateCategory]], levels = c(input$oc1, input$oc2))

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

    allCovarChoices <- as.list(names(vals$SEList@colData@listData))
    # print(allCovarChoices)
    # goodCovarChoices <- allCovarChoices[sapply(allCovarChoices, function(name) {
    #   type <- typeof(vals$SEList$colData$listData[[name]])
    #   type == "character"
    # })]
    # print(goodCovarChoices)

    updateSelectInput(session, "covariateCategory", choices = allCovarChoices)
  }
})

# Reactively updates the selectInputs for the outcome choices
observeEvent(input$covariateCategory, {
  if(!is.null(input$covariateCategory)) {
    if(!is.null(vals$SEList)) {
      uniqueCovarChoices <- unique(vals$SEList@colData@listData[[input$covariateCategory]])
      updateSelectInput(session, "oc1", choices = uniqueCovarChoices)
      updateSelectInput(session, "oc2", choices = c(uniqueCovarChoices, "All Else"))
    }
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

######################################################################################################
####################################### MACHINE LEARNING METHODS #####################################
######################################################################################################

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

      print("1")
      # Forming random forest model
      rfModel <- caret::train(
        as.formula(paste(input$covariateCategory, "~ .")),
        data = rv$trainingData,
        method = "rf",
        tuneGrid = data.frame(mtry = input$mtryInput),
        nodesize = input$nodeSize,
        ntree = input$numTrees,
        trControl = control
      )

      print("2")

      # Getting importance plot
      rfImportance <- varImp(rfModel)

      importance <- rfImportance
      rv$rfImportance <- rfImportance

      sorted_data <- importance$importance[order(importance$importance$Overall, decreasing = TRUE), , drop = FALSE]

      top_five <- sorted_data[1:5, , drop = FALSE]
      top_genes <- sorted_data[1:10, , drop = FALSE]
      genes_above_90 <- importance$importance[importance$importance$Overall >= 90, , drop = FALSE]
      genes_above_80 <- importance$importance[importance$importance$Overall >= 80, , drop = FALSE]

      # This sends the identified genes to the TBsignatureprofiler
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

# Here, we output the rf Importance plot.
output$rfImportancePlot <- renderPlot({
  tryCatch({
    if(!is.null(rv$rfImportance)) {
      importance <- rv$rfImportance
      sorted_data <- importance
      sorted_data$importance <- importance$importance[order(importance$importance$Overall, decreasing = TRUE), , drop = FALSE]
      sorted_data$importance <- sorted_data$importance[1:input$rfSignatureSize, , drop = FALSE]
      rv$rfGeneSigNames <- as.list(rownames(sorted_data$importance))
      plot(sorted_data, main = "Random Forest Importance Plot")
    }
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
})

observeEvent(input$rfTestGeneSig, {

  colKeep <- c(input$covariateCategory, rv$rfGeneSigNames)

  # Reduces testing and training data to only include chosen genes
  newTrainingData <- rv$trainingData[, unlist(colKeep)]

  newtestingData <- rv$testData[, unlist(colKeep)]

  control <- trainControl(
    method = "cv",
    number = input$foldCount
  )

  # Forming random forest model
  rfModel <- caret::train(
    as.formula(paste(input$covariateCategory, "~ .")),
    data = newTrainingData,
    method = "rf",
    tuneGrid = data.frame(mtry = input$mtryInput),
    nodesize = input$nodeSize,
    ntree = input$numTrees,
    trControl = control
  )

  rfPredictions <- predict(rfModel, newtestingData)
  rv$rfConfusionMatrix <- confusionMatrix(rfPredictions, newtestingData[[input$covariateCategory]])

  # Renders the matrix table
  output$rfMatrixTable <- renderTable({
    tryCatch({
      as.data.frame(rv$rfConfusionMatrix$table)
    }, error = function(e) {
      # cat("Error:", conditionMessage(e), "\n")
      # showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
  })

  # Renders the matrix plot
  output$rfMatrixPlot <- renderPlot({
    tryCatch({
      plot(table(rfPredictions, rv$testData[[input$covariateCategory]]), main = "Confusion matrix", xlab = "", ylab = "Test Actual:")
      mtext("Model Prediction:", side = 3, line = .5, cex = 1.2)
    })
  })
})

# Code for Support Vector Machines
observeEvent(input$continueSVM, {
  tryCatch({
    withProgress(message = "Training Model...", value = 0, {
      if(input$kernelType == "Linear"){
        kType <- "svmLinear"
      } else if(input$kernelType == "Radial"){
        kType <- "svmRadial"
      } else {
        kType <- "svmPoly"
      }

      #cross validation and SVM training
      control <- trainControl(method = "cv", number = input$foldCount)
      svmModel <- caret::train(as.formula(paste(input$covariateCategory, "~ .")),
                               data = rv$trainingData,
                               method = kType,
                               trControl = control)

      svmImportance <- varImp(svmModel)
      importance <- svmImportance
      rv$svmImportance <- svmImportance

      #gene selection
      #included are the top 5, ten, any genes above 90, and any above 80, for comparison purposes
      #grabs the top genes from the importance matrix
      sorted_data <- importance$importance[order(importance$importance[[input$oc1]], decreasing = TRUE), ]
      # View(as.data.frame(sorted_data))
      #select the top 5 genes after sorting
      top_five <- sorted_data[1:5, , drop = FALSE]
      # View(top)
      #select the top 10 genes after sorting
      top_genes <- sorted_data[1:10, , drop = FALSE]
      # View(top_genes)
      #select any genes which are greater than 90
      genes_above_90 <- importance$importance[importance$importance[[input$oc1]] >= 90, , drop = FALSE]
      # View(genes_above_90)
      #and select any genes which are greater than 80
      genes_above_80 <- importance$importance[importance$importance[input$oc1] >= 80, , drop = FALSE]
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

      showNotification("Finished Generating Support Vector Machine Model", type = "message")
    })
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
})

output$svmImportancePlot <- renderPlot({
  tryCatch({
    if(!is.null(rv$svmImportance)) {
      importance <- rv$svmImportance
      sortedData <- importance
      sortedData$importance <- importance$importance[order(importance$importance[[input$oc1]], decreasing = TRUE), , drop = FALSE]
      sortedData$importance <- sortedData$importance[1:input$svmSignatureSize, , drop = FALSE]

      rv$svmGeneSigNames <- as.list(rownames(sortedData$importance))
      plot(sortedData, main = "Support Vector Machine Importance Plot")
    }
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
})

observeEvent(input$svmTestGeneSig, {
  colKeep <- c(input$covariateCategory, rv$svmGeneSigNames)

  newTrainingData <- rv$trainingData[, unlist(colKeep)]
  newtestingData <- rv$testData[, unlist(colKeep)]

  if(input$kernelType == "Linear"){
    kType <- "svmLinear"
  } else if(input$kernelType == "Radial"){
    kType <- "svmRadial"
  } else {
    kType <- "svmPoly"
  }

  control <- trainControl(method = "cv", number = input$foldCount)

  svmModel <- caret::train(as.formula(paste(input$covariateCategory, "~ .")),
                           data = newTrainingData,
                           method = kType,
                           trControl = control)

  svmPredictions <- predict(svmModel, newtestingData)

  rv$svmConfusionMatrix <- confusionMatrix(svmPredictions, newtestingData[[input$covariateCategory]])
  View(rv$svmConfusionMatrix)
  View(as.data.frame(rv$svmConfusionMatrix$table))
  output$svmMatrixTable <- renderTable({
    tryCatch({
      as.data.frame(rv$svmConfusionMatrix$table)
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
  })

  output$svmMatrixPlot <- renderPlot({
    tryCatch({
      plot(table(svmPredictions, rv$testData[[input$covariateCategory]]), main = "Confusion matrix", xlab = "", ylab = "Test Actual:")
      mtext("Model Prediction:", side = 3, line = .5, cex = 1.2)
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
  })
})


# Code for Elastic Net Regression
observeEvent(input$continueEN, {
  tryCatch({
    withProgress(message = "Training Model...", value = 0, {
      control <- trainControl(method = "cv", number = input$foldCount)
      enModel <- caret::train(as.formula(paste(input$covariateCategory, "~ .")),
                              data = rv$trainingData,
                              method = "glmnet",
                              trControl = control,
                              tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.001, 1, length = 100)))

      enImportance <- varImp(enModel)
      importance <- enImportance
      rv$enImportance <- enImportance

      sortedData <- importance$importance[order(importance$importance$Overall, decreasing = TRUE), , drop = FALSE]

      top_five <- sortedData[1:5, , drop = FALSE]
      top_genes <- sortedData[1:10, , drop = FALSE]
      genes_above_90 <- importance$importance[importance$importance$Overall >= 90, , drop = FALSE]
      genes_above_80 <- importance$importance[importance$importance$Overall >= 80, , drop = FALSE]

      # This sends the identified genes to the TBsignatureprofiler
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

      showNotification("Finished Generating Elastic Net Model", type = "message")
    })

  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
})

output$enImportancePlot <- renderPlot({
  tryCatch({
    if(!is.null(rv$enImportance)) {
      importance <- rv$enImportance
      sortedData <- importance
      sortedData$importance <- importance$importance[order(importance$importance$Overall, decreasing = TRUE), , drop = FALSE]
      sortedData$importance <- sortedData$importance[1:input$enSignatureSize, , drop = FALSE]
      rv$enGeneSigNames <- as.list(rownames(sortedData$importance))
      plot(sortedData, main = "Elastic Net Importance Plot")
    }
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
})

observeEvent(input$enTestGeneSig, {

  colKeep <- c(input$covariateCategory, rv$enGeneSigNames)

  newTrainingData <- rv$trainingData[, unlist(colKeep)]
  newtestingData <- rv$testData[, unlist(colKeep)]

  control <- trainControl(
    method = "cv",
    number = input$foldCount
  )

  enModel <- caret::train(as.formula(paste(input$covariateCategory, "~ .")),
                          data = newTrainingData,
                          method = "glmnet",
                          trControl = control,
                          tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.001, 1, length = 100)))
  enPredictions <- predict(enModel, newtestingData)

  rv$enConfusionMatrix <- confusionMatrix(enPredictions, newtestingData[[input$covariateCategory]])

  output$enMatrixTable <- renderTable({
    tryCatch({
      as.data.frame(rv$enConfusionMatrix$table)
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
  })
  output$enMatrixPlot <- renderPlot({
    tryCatch({
      plot(table(enPredictions, rv$testData[[input$covariateCategory]]), main = "Confusion matrix", xlab = "", ylab = "Test Actual:")
      mtext("Model Prediction:", side = 3, line = .5, cex = 1.2)
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
  })

})


# Code for Neural Networks
# Define a reactive value to store the trained model

observeEvent(input$continueNN, {
  tryCatch({
    withProgress(message = "Training Model...", value = 0, {
      control <- trainControl(method = "cv", number = input$foldCount)

      nnModel <- caret::train(as.formula(paste(input$covariateCategory, "~ .")),
                              data = rv$trainingData,
                              method = "nnet",
                              trControl = control,
                              linout = FALSE,
                              maxit = input$numEpochs,
                              maxNWts = 10000,
      )

      nnImportance <- varImp(nnModel)

      importance <- nnImportance
      rv$nnImportance <- nnImportance

      sortedData <- importance$importance[order(importance$importance$Overall, decreasing = TRUE), , drop = FALSE]

      top_five <- sortedData[1:5, , drop = FALSE]
      top_genes <- sortedData[1:10, , drop = FALSE]
      genes_above_90 <- importance$importance[importance$importance$Overall >= 90, , drop = FALSE]
      genes_above_80 <- importance$importance[importance$importance$Overall >= 80, , drop = FALSE]

      # This sends the identified genes to the TBsignatureprofiler
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

      showNotification("Finished Generating Neural Network Model", type = "message")
    })
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
})

output$nnImportancePlot <- renderPlot({
  tryCatch({
    if(!is.null(rv$nnImportance)) {
      importance <- rv$nnImportance
      sortedData <- importance
      sortedData$importance <- importance$importance[order(importance$importance$Overall, decreasing = TRUE), , drop = FALSE]
      sortedData$importance <- sortedData$importance[1:input$nnSignatureSize, , drop = FALSE]
      rv$nnGeneSigNames <- as.list(rownames(sortedData$importance))
      plot(sortedData, main = "Neural Network Importance Plot")
    }
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
})

observeEvent(input$nnTestGeneSig, {
  colKeep <- c(input$covariateCategory, rv$nnGeneSigNames)

  newTrainingData <- rv$trainingData[, unlist(colKeep)]
  newtestingData <- rv$testData[, unlist(colKeep)]

  control <- trainControl(method = "cv", number = input$foldCount)

  nnModel <- caret::train(as.formula(paste(input$covariateCategory, "~ .")),
                          data = newTrainingData,
                          method = "nnet",
                          trControl = control,
                          linout = FALSE,
                          maxit = input$numEpochs,
                          maxNWts = 10000,
  )

  nnPredictions <- predict(nnModel, newtestingData)

  rv$nnConfusionMatrix <- confusionMatrix(nnPredictions, newtestingData[[input$covariateCategory]])

  output$nnMatrixTable <- renderTable({
    tryCatch({
      as.data.frame(rv$nnConfusionMatrix$table)
    }, error = function(e) {

    })
  })

  output$nnMatrixPlot <- renderPlot({
    tryCatch({
      plot(table(nnPredictions, rv$testData[[input$covariateCategory]]), main = "Confusion matrix", xlab = "", ylab = "Test Actual:")
      mtext("Model Prediction:", side = 3, line = .5, cex = 1.2)
    })
  })
})
