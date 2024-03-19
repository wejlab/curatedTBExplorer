# Reactive values for various tasks
selected_studies <- reactiveVal(NULL) # this is used to update the selected studies accordingly
# continue_clicked <- reactiveVal(FALSE)  # stores if Continue button is clicked
my_data <- reactiveVal(NULL)



# Render the selected studies text
output$selected_studies_text <- renderText({
  paste("Selected Studies: ", paste(selected_studies(), collapse = ", "))
})

reactive({ # apparently need to be wrapped in reactive to work
  
  # move the selected studies in a single object list
  object_list <- curatedTBData(selected_studies(), dry.run = FALSE, curated.only = TRUE)
  # Combine the studies together in a single SE object
  combined_studies <- combine_objects(object_list, experiment_name = "assay_curated", update_genes = FALSE)
})

observe({
  filter_by <- input$filter_by
  
  if (!is.null(filter_by)) {
    column_index <- which(colnames(colData(combined_studies)) == filter_by)
    column_values <- colData(combined_studies)[, column_index]
    unique_column_values <- unique(column_values)
    
    # Render the dynamic selectInput based on the selected filter_by choice
    output$dynamic_filter <- renderUI({
      tagList(
      selectInput("sub_filter", filter_by, choices = unique_column_values),
      actionButton("filter_apply_btn", "Filter", class = "btn-primary")
      )
    })
  }
})
observeEvent(input$filter_apply_btn, {
  print("Filter button clicked")
  filter_by <- input$filter_by
  sub_filter <- input$sub_filter # Retrieve value of subfilter from input
  # Subset our SE in specific category
  subset_SE <- combined_studies[combined_studies[[filter_by]] == sub_filter, ] # subset the filter value in SE obj
  print("the code ran this far")
  my_data(as.data.frame(colData(subset_SE)))
})


output$filter_summary_table <- renderDT(
  {
    datatable(my_data(), options = list(
      ordering = TRUE,
      pageLength = 10,
      scrollX = TRUE,
      scrollY = TRUE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#4C516D', 'color': '#fff'});",
        "}"
      ),
      rowCallback = JS(
        
      )
    ))
    
  }
)
