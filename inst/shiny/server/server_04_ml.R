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

  TBsignatures_reactive = NULL,
  datasetConfirm = NULL

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
    rv$datasetConfirm = TRUE
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

      vals$DE <- DE_analyze(vals$mlList, 'limma', "Study", input$covariateCategory, input$assaySelection)

      # Filters out when padj is less than or equal to 0.05
      vals$filtered <- lapply(vals$DE, function(df) {
        df %>%
          filter(padj <= 0.05)
      })

      # We make generated name because it needs to match the one generated through DE_analyze (covarCategory + Outcome)
      generatedName <- paste0(input$covariateCategory, input$oc1)
      # View(generatedName)
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

      #training assay data depends on the assay selection by user
      training_assay_data <- rv$trainingSE@assays@data@listData[[input$assaySelection]]

      col_data <- colData(rv$trainingSE)
      col_data[[input$covariateCategory]] <- factor(col_data[[input$covariateCategory]], levels = c(input$oc1, input$oc2))

      # Data is our training dataframe
      rv$trainingData <- setNames(data.frame(col_data[[input$covariateCategory]], t(training_assay_data)), c(input$covariateCategory, colnames(t(training_assay_data))))

      rv$trainingData[[input$covariateCategory]] <- factor(rv$trainingData[[input$covariateCategory]], levels = c(input$oc1, input$oc2))

      #same as training assay data
      testing_assay_data <- rv$testingSE@assays@data@listData[[input$assaySelection]]

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
    if("TBStatus" %in% allCovarChoices) {
      allCovarChoices <- c("TBStatus", allCovarChoices[allCovarChoices != "TBStatus"])
    }

    # print(allCovarChoices)
    # goodCovarChoices <- allCovarChoices[sapply(allCovarChoices, function(name) {
    #   type <- typeof(vals$SEList$colData$listData[[name]])
    #   type == "character"
    # })]
    # print(goodCovarChoices)

    updateSelectInput(session, "covariateCategory", choices = setdiff(allCovarChoices, c("Age", "TST")))
    updateSelectInput(session, "assaySelection", choices = vals$datassays)
  }
})

# Reactively updates the selectInputs for the outcome choices
observeEvent(input$covariateCategory, {
  if(!is.null(input$covariateCategory)) {
    if(!is.null(vals$SEList)) {
      uniqueCovarChoices <- unique(vals$SEList@colData@listData[[input$covariateCategory]])
      updateSelectInput(session, "oc1", choices = na.omit(uniqueCovarChoices))
      updateSelectInput(session, "oc2", choices = na.omit(c(uniqueCovarChoices, "All Else")))
      observeEvent(input$oc1, {
        if(!is.null(input$oc1)) {
          if(!is.null(vals$SEList)) {
            updateSelectInput(session, "oc2", choices = na.omit(c(setdiff(uniqueCovarChoices, input$oc1), "All Else")))
          }
        }
      })
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

###################################################################
# Code for Random Forests
# Random Forest Model Creation and Variable Importance Handling
observeEvent(input$continueRF, {
  if (is.null(rv$datasetConfirm)) {
    showModal(modalDialog(
      title = "Error",
      "Please confirm the datasets before continuing.",
      easyClose = TRUE,
      footer = NULL
    ))
  } else {
    tryCatch({
      withProgress(message = "Training Model...", value = 0, {
        # Setting control settings for random forest model
        control <- trainControl(
          method = "cv",
          number = input$foldCount
        )

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

        # Getting variable importance
        rfImportance <- varImp(rfModel)
        rv$rfImportance <- rfImportance

        # Select genes based on importance
        sortedData <- rfImportance$importance[order(rfImportance$importance$Overall, decreasing = TRUE), , drop = FALSE]
        rv$rfGeneSigNames <- as.list(rownames(sortedData)[1:input$rfSignatureSize])

        showNotification("Finished Generating Random Forest Model", type = "message")
      })
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
  }
})

# Plot for Random Forest Importance
output$rfImportancePlot <- renderPlot({
  tryCatch({
    if (!is.null(rv$rfImportance)) {
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

# Update and Test Random Forest Model upon Button Click
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
      cat("Error:", conditionMessage(e), "\n")
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
  })

  # Renders the matrix plot
  output$rfMatrixPlot <- renderPlot({
    tryCatch({
      table <- rv$rfConfusionMatrix$table

      df <- data.frame(
        Prediction = c(input$oc1, input$oc2, input$oc1, input$oc2),
        Reference = c(input$oc1, input$oc1, input$oc2, input$oc2),
        Freq = c(table[1, 1], table[2, 1], table[1, 2], table[2, 2])
      )

      cm <- matrix(as.character(unlist(df[3])), nrow=2, byrow=TRUE)

      rownames(cm) <- c(input$oc1, input$oc2)
      colnames(cm) <- c(input$oc1, input$oc2)

      # Convert the matrix to a data frame suitable for ggplot
      cmDf <- as.data.frame(cm)
      cmDf$Reference <- rownames(cmDf)
      cmMelt <- melt(cmDf, id.vars = "Reference")

      colnames(cmMelt) <- c("Actual", "Predicted", "Freq")

      # Define colors for the cells
      cmMelt$Color <- ifelse(cmMelt$Actual == cmMelt$Predicted, "lightgreen", "lightcoral")

      # Confusion matrix plot
      ggplot(data = cmMelt, aes(x = Predicted, y = Actual)) +
        geom_tile(aes(fill = Color), color = "white") +
        scale_fill_identity() +
        geom_text(aes(label = Freq), vjust = 1) +
        labs(title = "Confusion Matrix",
             x = "Predicted",
             y = "Actual") +
        theme_minimal()
    })
  })

  # Create and update TBSignatures after the Random Forest testing is completed
  TBsignatures <- c(TBsignatures, list(RFGeneSignature = rv$rfGeneSigNames))
  rv$TBsignatures_reactive <- TBsignatures

  # Display notification when TBSignatures are updated
  showNotification("TBSignatures have been updated.", type = "message")
})

###################################################################



###################################################################
# Code for Support Vector Machines
# SVM Model Creation and Variable Importance Handling
observeEvent(input$continueSVM, {
  if (is.null(rv$datasetConfirm)) {
    showModal(modalDialog(
      title = "Error",
      "Please confirm the datasets before continuing.",
      easyClose = TRUE,
      footer = NULL
    ))
  } else {
    tryCatch({
      withProgress(message = "Training Model...", value = 0, {
        if(input$kernelType == "Linear"){
          kType <- "svmLinear"
        } else if(input$kernelType == "Radial"){
          kType <- "svmRadial"
        } else {
          kType <- "svmPoly"
        }

        # Cross validation and SVM training
        control <- trainControl(method = "cv", number = input$foldCount)
        svmModel <- caret::train(as.formula(paste(input$covariateCategory, "~ .")),
                                 data = rv$trainingData,
                                 method = kType,
                                 trControl = control)

        # Get variable importance
        svmImportance <- varImp(svmModel)
        rv$svmImportance <- svmImportance

        # Select genes based on importance
        sortedData <- svmImportance$importance[order(svmImportance$importance[[input$oc1]], decreasing = TRUE), ]
        rv$svmGeneSigNames <- as.list(rownames(sortedData)[1:input$svmSignatureSize])

        showNotification("Finished Generating Support Vector Machine Model", type = "message")
      })
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
  }
})

# Plot for SVM Importance
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

# Update and Test SVM Model upon Button Click
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

  # Cross-validation and SVM training with new selected features
  control <- trainControl(method = "cv", number = input$foldCount)
  svmModel <- caret::train(as.formula(paste(input$covariateCategory, "~ .")),
                           data = newTrainingData,
                           method = kType,
                           trControl = control)

  # Predict on the test data
  svmPredictions <- predict(svmModel, newtestingData)

  # Confusion Matrix
  rv$svmConfusionMatrix <- confusionMatrix(svmPredictions, newtestingData[[input$covariateCategory]])
  # View(rv$svmConfusionMatrix)
  # View(as.data.frame(rv$svmConfusionMatrix$table))

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
      table <- rv$svmConfusionMatrix$table

      df <- data.frame(
        Prediction = c(input$oc1, input$oc2, input$oc1, input$oc2),
        Reference = c(input$oc1, input$oc1, input$oc2, input$oc2),
        Freq = c(table[1, 1], table[2, 1], table[1, 2], table[2, 2])
      )

      cm <- matrix(as.character(unlist(df[3])), nrow=2, byrow=TRUE)

      rownames(cm) <- c(input$oc1, input$oc2)
      colnames(cm) <- c(input$oc1, input$oc2)

      # Convert the matrix to a data frame suitable for ggplot
      cmDf <- as.data.frame(cm)
      cmDf$Reference <- rownames(cmDf)
      cmMelt <- melt(cmDf, id.vars = "Reference")

      colnames(cmMelt) <- c("Actual", "Predicted", "Freq")

      # Define colors for the cells
      cmMelt$Color <- ifelse(cmMelt$Actual == cmMelt$Predicted, "lightgreen", "lightcoral")

      # Confusion matrix plot
      ggplot(data = cmMelt, aes(x = Predicted, y = Actual)) +
        geom_tile(aes(fill = Color), color = "white") +
        scale_fill_identity() +
        geom_text(aes(label = Freq), vjust = 1) +
        labs(title = "Confusion Matrix",
             x = "Predicted",
             y = "Actual") +
        theme_minimal()
    })
  })

  # Create and update TBSignatures after the SVM testing is completed
  TBsignatures <- c(TBsignatures, list(SVMGeneSignature = rv$svmGeneSigNames))
  rv$TBsignatures_reactive <- TBsignatures

  # Display notification when TBSignatures are updated
  showNotification("TBSignatures have been updated.", type = "message")
})

###################################################################



###################################################################
# Code for Elastic Net Regression
# Elastic Net Model Creation and Variable Importance Handling
observeEvent(input$continueEN, {
  if (is.null(rv$datasetConfirm)) {
    showModal(modalDialog(
      title = "Error",
      "Please confirm the datasets before continuing.",
      easyClose = TRUE,
      footer = NULL
    ))
  } else {
    tryCatch({
      withProgress(message = "Training Model...", value = 0, {
        # Setting control settings for Elastic Net model
        control <- trainControl(method = "cv", number = input$foldCount)

        # Forming Elastic Net model
        enModel <- caret::train(
          as.formula(paste(input$covariateCategory, "~ .")),
          data = rv$trainingData,
          method = "glmnet",
          trControl = control,
          tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.001, 1, length = 100))
        )

        # Getting variable importance
        enImportance <- varImp(enModel)
        rv$enImportance <- enImportance

        # Select genes based on importance
        sortedData <- enImportance$importance[order(enImportance$importance$Overall, decreasing = TRUE), , drop = FALSE]
        rv$enGeneSigNames <- as.list(rownames(sortedData)[1:input$enSignatureSize])

        showNotification("Finished Generating Elastic Net Model", type = "message")
      })
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
  }
})

# Plot for Elastic Net Importance
output$enImportancePlot <- renderPlot({
  tryCatch({
    if (!is.null(rv$enImportance)) {
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

# Update and Test Elastic Net Model upon Button Click
observeEvent(input$enTestGeneSig, {
  tryCatch({
    colKeep <- c(input$covariateCategory, rv$enGeneSigNames)

    # Reduces testing and training data to only include chosen genes
    newTrainingData <- rv$trainingData[, unlist(colKeep)]
    newtestingData <- rv$testData[, unlist(colKeep)]

    control <- trainControl(method = "cv", number = input$foldCount)

    # Forming Elastic Net model with selected gene signature
    enModel <- caret::train(
      as.formula(paste(input$covariateCategory, "~ .")),
      data = newTrainingData,
      method = "glmnet",
      trControl = control,
      tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.001, 1, length = 100))
    )

    # Making predictions on test data
    enPredictions <- predict(enModel, newtestingData)
    rv$enConfusionMatrix <- confusionMatrix(enPredictions, newtestingData[[input$covariateCategory]])

    # Render confusion matrix table
    output$enMatrixTable <- renderTable({
      tryCatch({
        as.data.frame(rv$enConfusionMatrix$table)
      }, error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
        showNotification(paste("Error:", conditionMessage(e)), type = "error")
      })
    })

    # Render confusion matrix plot
    output$enMatrixPlot <- renderPlot({
      tryCatch({
        table <- rv$enConfusionMatrix$table

        df <- data.frame(
          Prediction = c(input$oc1, input$oc2, input$oc1, input$oc2),
          Reference = c(input$oc1, input$oc1, input$oc2, input$oc2),
          Freq = c(table[1, 1], table[2, 1], table[1, 2], table[2, 2])
        )

        cm <- matrix(as.character(unlist(df[3])), nrow=2, byrow=TRUE)

        rownames(cm) <- c(input$oc1, input$oc2)
        colnames(cm) <- c(input$oc1, input$oc2)

        # Convert the matrix to a data frame suitable for ggplot
        cmDf <- as.data.frame(cm)
        cmDf$Reference <- rownames(cmDf)
        cmMelt <- melt(cmDf, id.vars = "Reference")

        colnames(cmMelt) <- c("Actual", "Predicted", "Freq")

        # Define colors for the cells
        cmMelt$Color <- ifelse(cmMelt$Actual == cmMelt$Predicted, "lightgreen", "lightcoral")

        # Confusion matrix plot
        ggplot(data = cmMelt, aes(x = Predicted, y = Actual)) +
          geom_tile(aes(fill = Color), color = "white") +
          scale_fill_identity() +
          geom_text(aes(label = Freq), vjust = 1) +
          labs(title = "Confusion Matrix",
               x = "Predicted",
               y = "Actual") +
          theme_minimal()
      })
    })

    # Create and update TBSignatures after Elastic Net testing is completed
    TBsignatures <- c(TBsignatures, list(ENGeneSignature = rv$enGeneSigNames))
    rv$TBsignatures_reactive <- TBsignatures

    # Display notification when TBSignatures are updated
    showNotification("TBSignatures have been updated.", type = "message")
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
})

###################################################################



###################################################################
#Neural Networks
# Neural Network Model Creation and Variable Importance Handling
observeEvent(input$continueNN, {
  if (is.null(rv$datasetConfirm)) {
    showModal(modalDialog(
      title = "Error",
      "Please confirm the datasets before continuing.",
      easyClose = TRUE,
      footer = NULL
    ))
  } else {
    tryCatch({
      withProgress(message = "Training Model...", value = 0, {
        # Setting control settings for Neural Network model
        control <- trainControl(method = "cv", number = input$foldCount)

        # Forming Neural Network model
        nnModel <- caret::train(
          as.formula(paste(input$covariateCategory, "~ .")),
          data = rv$trainingData,
          method = "nnet",
          trControl = control,
          linout = FALSE,
          maxit = input$numEpochs,
          maxNWts = 10000
        )

        # Getting variable importance
        nnImportance <- varImp(nnModel)
        rv$nnImportance <- nnImportance

        # Select genes based on importance
        sortedData <- nnImportance$importance[order(nnImportance$importance$Overall, decreasing = TRUE), , drop = FALSE]
        rv$nnGeneSigNames <- as.list(rownames(sortedData)[1:input$nnSignatureSize])

        showNotification("Finished Generating Neural Network Model", type = "message")
      })
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
  }
})

# Plot for Neural Network Importance
output$nnImportancePlot <- renderPlot({
  tryCatch({
    if (!is.null(rv$nnImportance)) {
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

# Update and Test Neural Network Model upon Button Click
observeEvent(input$nnTestGeneSig, {
  tryCatch({
    colKeep <- c(input$covariateCategory, rv$nnGeneSigNames)

    # Reduces testing and training data to only include chosen genes
    newTrainingData <- rv$trainingData[, unlist(colKeep)]
    newtestingData <- rv$testData[, unlist(colKeep)]

    control <- trainControl(method = "cv", number = input$foldCount)

    # Forming Neural Network model with selected gene signature
    nnModel <- caret::train(
      as.formula(paste(input$covariateCategory, "~ .")),
      data = newTrainingData,
      method = "nnet",
      trControl = control,
      linout = FALSE,
      maxit = input$numEpochs,
      maxNWts = 10000
    )

    # Making predictions on test data
    nnPredictions <- predict(nnModel, newtestingData)
    rv$nnConfusionMatrix <- confusionMatrix(nnPredictions, newtestingData[[input$covariateCategory]])

    # Render confusion matrix table
    output$nnMatrixTable <- renderTable({
      tryCatch({
        as.data.frame(rv$nnConfusionMatrix$table)
      }, error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
        showNotification(paste("Error:", conditionMessage(e)), type = "error")
      })
    })

    # Render confusion matrix plot
    output$nnMatrixPlot <- renderPlot({
      tryCatch({
        table <- rv$nnConfusionMatrix$table

        df <- data.frame(
          Prediction = c(input$oc1, input$oc2, input$oc1, input$oc2),
          Reference = c(input$oc1, input$oc1, input$oc2, input$oc2),
          Freq = c(table[1, 1], table[2, 1], table[1, 2], table[2, 2])
        )

        cm <- matrix(as.character(unlist(df[3])), nrow=2, byrow=TRUE)

        rownames(cm) <- c(input$oc1, input$oc2)
        colnames(cm) <- c(input$oc1, input$oc2)

        # Convert the matrix to a data frame suitable for ggplot
        cmDf <- as.data.frame(cm)
        cmDf$Reference <- rownames(cmDf)
        cmMelt <- melt(cmDf, id.vars = "Reference")

        colnames(cmMelt) <- c("Actual", "Predicted", "Freq")

        # Define colors for the cells
        cmMelt$Color <- ifelse(cmMelt$Actual == cmMelt$Predicted, "lightgreen", "lightcoral")

        # Confusion matrix plot
        ggplot(data = cmMelt, aes(x = Predicted, y = Actual)) +
          geom_tile(aes(fill = Color), color = "white") +
          scale_fill_identity() +
          geom_text(aes(label = Freq), vjust = 1) +
          labs(title = "Confusion Matrix",
               x = "Predicted",
               y = "Actual") +
          theme_minimal()
      })
    })

    # Create and update TBSignatures after Neural Network testing is completed
    TBsignatures <- c(TBsignatures, list(NNGeneSignature = rv$nnGeneSigNames))
    rv$TBsignatures_reactive <- TBsignatures

    # Display notification when TBSignatures are updated
    showNotification("TBSignatures have been updated.", type = "message")
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  })
})


###################################################################