# note: the 5th dataset is a great dataset for the heatmap visualization

# to do:
# generate box plots correctly
# fix the output of the heatmap (its just squished and the button overlaps)
# allow for the plots in the heatmap to change based on filters from the filter page

tb_profiler_result <- reactiveVal(NULL)

#need to get select All / deselect All working properly
#need to get assay selection working properly
  #works, however it doesn't run properly
observeEvent(input$selectAll, {
  selected_profiles <- names(TBsignatures)
})

observeEvent(input$begin, {
  vals$SEList <- mkAssay(vals$SEList, input_name = "assay_curated",
                        log = TRUE, counts_to_CPM = FALSE)
  vals$SEList <- mkAssay(vals$SEList, input_name = "assay_curated",)
  vals$SEList <- mkAssay(vals$SEList, input_name = "assay_curated",
                         log = TRUE)
  vals$datassays <- names(SummarizedExperiment::assays(vals$SEList))
  # View(vals$datassays)
  # View(input$assay)


  # View(assays(vals$SEList)$assay_curated_cpm)
  # View(assays(vals$SEList)$assay_curated)
  # View(assays(vals$SEList)$log_assay_curated)
  # View(assays(vals$SEList)$log_assay_curated_cpm)


  selected_dataset <- vals$SEList
  # selected_profiles <- c(input$profile1, input$profile2, input$profile3)
  selected_profiles <- input$profiles
  selected_assay <- input$assay
  # stores both the direct ssgsea_results and the info for the dt output
  tb_profiler_result(runTBsigProfilerFunction(vals$SEList, selected_profiles, selected_assay))
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
