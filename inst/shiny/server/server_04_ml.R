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

outcomeChoice1 <- reactive({
  input$oc1
})

outcomeChoice2 <- reactive({
  input$oc2
})

observeEvent(input$confirmDataset, {
  selectedTrainingList <- input$selectedTrainingData
  selectedTestingList <- input$selectedTestingData

  subsetByStudy <- colData(vals$SEList)[colData(vals$SEList)$Study %in% selectedTrainingList, , drop = FALSE]
  rv$trainingSE <- vals$SEList[, colData(vals$SEList)$Study %in% selectedTrainingList ]
  # View(rv$trainingSE)

  subsetByStudy <- colData(vals$SEList)[colData(vals$SEList)$Study %in% selectedTestingList, , drop = FALSE]
  rv$testingSE <- vals$SEList[, colData(vals$SEList)$Study %in% selectedTestingList ]
  # View(rv$testingSE)

})

# Code for Random Forests
observeEvent(input$continueRF, {

  # Might need to check the SEList
  # DE_analyze(vals$SEList, 'limma', "logCPM")
  View(vals$SEList)
  View(rv$trainingSE)
})

observe ({
  if(!is.null(vals$SEList)){
    #will need to change to use please' code output
    mlList <- vals$SEList
    #grab the unique studies from the mlList
    study_info <- colData(mlList)$Study
    unique_study_values <- unique(study_info)
    updateSelectizeInput(session, "selectedTrainingData", choices = unique_study_values)
    updateSelectizeInput(session, "selectedTestingData", choices = unique_study_values)
    setdiff
    View(unique_study_values)
  }
})

reactive ({
  View(names$SEList)
})

# test <- DE_analyze(vals$SEList, 'limma', )





# Code for Support Vector Machines










observeEvent(input$continueSVM, {

})
# Code for Elastic Net Regression






observeEvent(input$continueEN, {

})

# Code for Neural Networks


observeEvent(input$continueNN, {

})







