
observeEvent(input$begin, {
  # View(the$downloaded_datasets)
  #converts the datasets into se
  temp_se <- toSE(the$downloaded_datasets[[1]])
  # View(temp_se)

  output$ssgsea_table <- renderDT({
    selected_dataset <- temp_se
    selected_profiles <- c(input$profile1, input$profile2, input$profile3)
    selected_assay <- input$assay
    # runTBsigProfilerFunction(temp_se, selected_profiles, selected_assay)
    #note that this isn't currently working
    curatedTBExplorer::runTBsigProfilerFunction(temp_se, selected_profiles)
  })
})
