source(file.path("../../R", "runTBsigProfilerFunction.R"), local = TRUE)$value

output$ssgsea_table <- renderDT({
  selected_dataset <- input$dataset
  selected_profiles <- c(input$profile1, input$profile2, input$profile3)
  selected_assay <- input$assay
  runTBsigProfilerFunction(selected_dataset, selected_profiles, selected_assay)
})