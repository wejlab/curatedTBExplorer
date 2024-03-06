
#adding functionality to select the profiles
#also, to select which assays to run the profiler on
#also, to select which algorithms to use (ssgsea, etc)
# if(input$profile1) {
#   input1 <- choices
# }
#
# View(input1)

observeEvent(input$begin, {
  # View(the$downloaded_datasets)
  #converts the datasets into se
  #currently only uses the first downloaded dataset, will change once we have a single se with all downloads
  temp_se <- toSE(vals$MAEList[1])
  # View(temp_se)
  # se <- toSE(vals$MAEList[[1]])
  output$ssgsea_table <- renderDT({
    selected_dataset <- temp_se
    selected_profiles <- c(input$profile1, input$profile2, input$profile3)
    selected_assay <- input$assay
    # runTBsigProfilerFunction(temp_se, selected_profiles, selected_assay)
    runTBsigProfilerFunction(temp_se, selected_profiles)
  })
})
