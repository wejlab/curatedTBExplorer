# Reactive values for various tasks
# selected_studies <- reactiveVal(NULL) # this is used to update the selected studies accordingly
# continue_clicked <- reactiveVal(FALSE)  # stores if Continue button is clicked
my_data <- reactiveVal(NULL)

# reactive Value to store selected filters
selected_filters <- reactiveValues(filters = NULL)

# Render the selected studies text
output$selected_studies_text <- renderText({

  # paste("Selected Studies: ", paste(vals$selected_studies, collapse = ", "))

  # Displays downloaded study names or default study if none downloaded
  if (!is.null(names(vals$MAEList))) {
    paste("Selected Studies: ", paste(names(vals$MAEList), collapse = ", "))
  }
  else {
    paste("Selected Studies: ", paste(names(vals$defaultStudy), collapse = ", "))
  }
})

reactive({ # apparently need to be wrapped in reactive to work

  # move the selected studies in a single object list
  object_list <- curatedTBData(vals$selected_studies, dry.run = FALSE, curated.only = TRUE)
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
      )
    })
  }
})

##FOLLOWING CODE IS FOR ADDING FILTERS TO LIST

# UI for displaying selected filters as bubbles
output$selected_filters_ui <- renderUI({
  filters <- selected_filters$filters

  # List to store UI elements for each filter
  filter_bubbles <- lapply(seq_along(filters), function(i) {
    tagList(
      div(
        paste(filters[[i]]$filter_by, ":", filters[[i]]$sub_filter),
        class = "filter-bubble",
        actionButton(paste0("remove_filter_btn_", i), "x", class = "btn btn-danger btn-sm remove-filter-btn")
      ),
      br()
    )
  })

  # Wrap filter bubbles in a div
  div(filter_bubbles)
})


# Add a new filter when the user clicks on "Add Filter" button
observeEvent(input$add_filter_btn, {
  print(input$filter_by)
  print(input$sub_filter)
  filters <- selected_filters$filters
  new_filter_index <- length(filters) + 1

  # Create a new filter object and add it to selected_filters
  selected_filters$filters[[new_filter_index]] <- list(
    filter_by = input$filter_by,
    sub_filter = input$sub_filter
  )
})


observeEvent(input$filter_apply_btn, {
  print("Filter button clicked")
  #filtered_data <- combined_studies
  filters <- selected_filters$filters
  if(is.null(filters)){
    print("selected filters is null")
    filter_by <- input$filter_by
    sub_filter <- input$sub_filter # Retrieve value of subfilter from input
    subset_SE <- combined_studies[, eval(parse(text = paste0("combined_studies$", filter_by))) == sub_filter]# subset the filter value in SE obj
    #Explanation : the values of filter_by and sub_filter have quote around it, we need those quotes removed for only filter_by in order for this command to execute properly
  }
  else{
    print("selected filters is not null")
    print(filters)
    subset_SE <- combined_studies
    for (filter in filters) {
      print(filter$filter_by)
      filter_by <- filter$filter_by
      print(filter$sub_filter)
      sub_filter <- filter$sub_filter
      subset_SE <- subset_SE[, eval(parse(text = paste0("subset_SE$", filter_by))) == sub_filter]# subset the filter value in SE obj
      #subset_SE <- combined_studies[combined_studies[[filter_by]] == sub_filter, ]
    }

  }

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
