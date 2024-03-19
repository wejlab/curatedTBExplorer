# note: the 5th dataset is a great dataset for the heatmap visualization

# to do:
# generate box plots correctly
# allow for the plots in the heatmap to change based on filters from the filter page
#update assay selection based on what has been created

tb_profiler_result <- reactiveVal(NULL)

#need to get select All / deselect All working properly
#need to get assay selection working properly
  #works, however it doesn't run properly
observeEvent(input$selectAll, {
  selected_profiles <- names(TBsignatures)
})

#Select which Assays (if any) to create
observeEvent(input$makeAssay, {
  if(input$selectAssay == "Log Counts"){
    vals$SEList <- mkAssay(vals$SEList, input_name = "assay_curated",
                           log = TRUE, counts_to_CPM = FALSE)
    vals$datassays <- names(SummarizedExperiment::assays(vals$SEList))
  } else if(input$selectAssay == "CPM"){
    vals$SEList <- mkAssay(vals$SEList, input_name = "assay_curated",)
    vals$datassays <- names(SummarizedExperiment::assays(vals$SEList))
  } else if(input$selectAssay == "Log CPM"){
    vals$SEList <- mkAssay(vals$SEList, input_name = "assay_curated",
                           log = TRUE)
    vals$datassays <- names(SummarizedExperiment::assays(vals$SEList))

  }
})

observeEvent(input$begin, {
  # vals$SEList <- mkAssay(vals$SEList, input_name = "assay_curated",
  #                       log = TRUE, counts_to_CPM = FALSE)
  # vals$SEList <- mkAssay(vals$SEList, input_name = "assay_curated",)
  # vals$SEList <- mkAssay(vals$SEList, input_name = "assay_curated",
  #                        log = TRUE)
  # vals$datassays <- names(SummarizedExperiment::assays(vals$SEList))
  selected_dataset <- vals$SEList
  selected_profiles <- input$profiles
  selected_assay <- input$assay
  selected_algorithm <- input$algorithm
  # stores both the direct ssgsea_results and the info for the dt output
  tb_profiler_result(runTBsigProfilerFunction(vals$SEList, selected_profiles, selected_assay, selected_algorithm))
  # renders the dt
  output$ssgsea_table <- renderDT({
    tb_profiler_result()[[1]]
  })
})

# observer for the heatmap button
observeEvent(input$genHeatmap, {
  # checks to ensure that the tb sig profiler has already been ran
  if (is.null(tb_profiler_result())) {
    # note: want to output to user in the future
    print("You must run the TB Signature Profiler First!")
  } else {
    # derived from the TBSignatureProfiler Vignette
    colors <- RColorBrewer::brewer.pal(6, "Spectral")
    col.me <- circlize::colorRamp2(seq(from = -2, to = 2, length.out = 6), colors)
    output$heatmap_result <- renderPlot({
      signatureHeatmap(tb_profiler_result()[[2]],
        name = "Heatmap of Signatures, ssGSEA Algorithm",
        signatureColNames = names(TBsignatures),
        annotationColNames = "PatientID",
        scale = TRUE,
        showColumnNames = TRUE,
        choose_color = col.me
      )
    })
  }
})

# observer for the boxplots button
# note: not working yet
observeEvent(input$genBoxplots, {
  # checks to ensure that the tb sig profiler has already been ran
  if (is.null(tb_profiler_result())) {
    print("You must run the TB Signature Profiler First!")
  } else {
    # dervied from the TBSignatureProfiler Vignette
    output$boxplot_result <- renderPlot({
      signatureBoxplot(
        inputData = tb_profiler_result()[[2]],
        name = "Boxplots of Signatures, ssGSEA",
        # signatureColNames = names(TBsignatures),
        signatureColNames = input$box_profiles,
        annotationColName = "PatientID", rotateLabels = FALSE
      )
    })
  }
})
