# to do:
# generate box plots correctly
# allow for the plots in the heatmap to change based on filters from the filter page

tb_profiler_result <- reactiveVal(NULL)
# reactive ({
#   vals$datassays <- names(assays(vals$SEList))
#   View(vals$datassays)
# })

#need to get select All / deselect All working properly
# observeEvent(input$selectAll, {
#   selected_profiles <- names(TBsignatures)
# })

#Select which Assays (if any) to create

#want to change this creation to be present on the filter page
observeEvent(input$makeAssay, {
  if(input$selectAssay == "Log Counts"){
    vals$SEList <- mkAssay(vals$SEList, input_name = "assay1",
                           log = TRUE, counts_to_CPM = FALSE)
    vals$datassays <- names(SummarizedExperiment::assays(vals$SEList))
  } else if(input$selectAssay == "CPM"){
    vals$SEList <- mkAssay(vals$SEList, input_name = "assay1",)
    vals$datassays <- names(SummarizedExperiment::assays(vals$SEList))
  } else if(input$selectAssay == "Log CPM"){
    vals$SEList <- mkAssay(vals$SEList, input_name = "assay1",
                           log = TRUE)
    vals$datassays <- names(SummarizedExperiment::assays(vals$SEList))

  }
})

shiny::observe({
  updateSelectInput(session, "boxCovariate", choices = vals$covars)
})

shiny::observe({
  print("entered observe within server 3")
  TBsignatures <- rv$TBsignatures_reactive
  # View(TBsignatures)
  updatePickerInput(session, "profiles", choices = names(TBsignatures))
  updatePickerInput(session, "signatures", choices = names(TBsignatures))
  updatePickerInput(session, "box_profiles", choices = names(TBsignatures))
})

shiny::observe({
  updateSelectInput(session, "assay", choices = vals$datassays)
  # View(vals$datassays)
})

observeEvent(input$begin, {
  selected_dataset <- vals$SEList
  selected_profiles <- input$profiles
  selected_assay <- input$assay
  selected_algorithm <- input$algorithm

  print("entered")
  tb_profiler_result(runTBsigProfilerFunction(vals$SEList, selected_profiles, selected_assay, selected_algorithm, rv$TBsignatures_reactive))
  print("exit")
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
    # View(names(TBsignatures))
    # View(vals$colData)
    # View(vals$SEList)
    colors <- RColorBrewer::brewer.pal(6, "Spectral")
    col.me <- circlize::colorRamp2(seq(from = -2, to = 2, length.out = 6), colors)
    #outputs heatmap using all signatures
    if(input$heatmapType == "All Signatures") {
      # derived from the TBSignatureProfiler Vignette
      output$heatmap_result <- renderPlot({
        signatureHeatmap(tb_profiler_result()[[2]],
                         name = "Heatmap of Signatures",
                         signatureColNames = names(rv$TBsignatures_reactive),
                         annotationColNames = "TBStatus",
                         scale = TRUE,
                         showColumnNames = TRUE,
                         choose_color = col.me
        )
      })
    } else {
      #outputs heatmap using only selected signatures
        #note: need to change from patient id to genes
      output$heatmap_result <- renderPlot({
        signatureHeatmap(tb_profiler_result()[[2]],
                         name = "Heatmap of Signatures",
                         signatureColNames = input$signatures,
                         annotationColNames = "PatientID",
                         scale = TRUE,
                         showColumnNames = TRUE,
                         choose_color = col.me
        )
      })
    }
  }
})

shiny::observe({
  updateSelectInput(session, "boxCovariate" ,choices = vals$covars)
})
# observer for the boxplots button
observeEvent(input$genBoxplots, {
  # checks to ensure that the tb sig profiler has already been ran
  if (is.null(tb_profiler_result())) {
    print("You must run the TB Signature Profiler First!")
  } else {
    output$boxplot_result <- renderPlot({
      isolate({
        print(signatureBoxplot(tb_profiler_result()[[2]],
                         signatureColNames = input$box_profiles,
                         annotationColName = input$boxCovariate))
      })
    })
  }
})
