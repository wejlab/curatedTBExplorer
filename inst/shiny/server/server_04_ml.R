# Code For General Settings And Selecting Datasets
rv <- reactiveValues(
  # Holds the users' choices for outcomes
  # Still need to figure out the format that this will come in
  outcomeChoice1 = NULL,
  outcomeChoice2 = NULL,

  # Results of user choice
  trainingSE = NULL,
  testingSE = NULL
)

outcomeChoice1 <- reactive({
  input$oc1
})
outcomeChoice2 <- reactive({
  input$oc2
})

# Code for Random Forests
observeEvent(input$continueRF, {
  #will need to change to use please' code output
  mlList <- vals$SEList
  # Might need to check the SEList
  # DE_analyze(vals$SEList, 'limma', "logCPM")
  View(vals$SEList)

  #grab the unique studies from the mlList
  study_info <- colData(mlList)$Study
  unique_study_values <- unique(study_info)
  View(unique_study_values)
})




################# Couple of Questions #############################

# In what format are we feeding in the conditions variable since it's said to be like PTB vs LTBI
# is it just c("PTB", "LTBI") ?

# Are we making each ML model have their own continue button or run them all at once?
# WE ARE MAKING THEM SEPERATELY
# This is especially important since each model has their own specific settings that need to be filled out seperately

# Wait, what is batch? Prof Johnson says it's the study, but what if there are multiple?
reactive ({
  View(names$SEList)
})

# test <- DE_analyze(vals$SEList, 'limma', )





# Code for Support Vector Machines











# Code for Elastic Net Regression








# Code for Neural Networks










