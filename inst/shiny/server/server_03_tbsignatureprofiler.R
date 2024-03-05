# source(file.path("../../R", "runTBsigProfilerFunction.R"), local = TRUE)$value
# source(file.path("../../R", "toSE.R"), local=TRUE)$value
# View(the$downloaded_datasets)

observeEvent(input$begin, {
  if(continue_clicked()) {
    temp_se <- toSE(the$downloaded_datasets[[1]])
  }

  output$ssgsea_table <- renderDT({
    selected_dataset <- temp_se
    selected_profiles <- c(input$profile1, input$profile2, input$profile3)
    selected_assay <- input$assay
    # curatedTBExplorer::runTBsigProfilerFunction(selected_dataset, selected_profiles, selected_assay)
  })
})
