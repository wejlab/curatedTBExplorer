# Reactive values for various tasks
my_data <- reactiveVal(NULL)

# reactive Value to store selected filters
selected_filters <- reactiveValues(filters = NULL)
# Holds the subsetted SE
subset_SE <- reactiveVal(NULL)
# Reactive value to trigger UI reset
reset_trigger <- reactiveVal(FALSE)

# Render the selected studies text
output$selected_studies_text <- renderText({
  if (!is.null(names(vals$MAEList))) {
    paste("Selected Studies: ", paste(names(vals$MAEList), collapse = ", "))
  }
})

# Updates the Input choices depending on the SE created in first page
observeEvent(vals$SEList, {
  if (!is.null(vals$SEList)) {
    updateSelectInput(session, "filter_by", choices = vals$covars)
    updateSelectInput(session, "visualize_filter_by", choices = vals$covars)
  }
})

############# Dropdowns #######################
observe({
  filter_by <- input$filter_by
  if (!is.null(filter_by)) {
    column_index <- which(vals$covars == filter_by)
    column_values <- vals$colData[, column_index]
    unique_column_values <- unique(column_values)

    # Render the dynamic selectInput based on the selected filter_by choice
    output$dynamic_filter <- renderUI({
      tagList(
        selectInput("sub_filter", filter_by, choices = unique_column_values),
      )
    })
  }
})

############### Add Filter Button ####################
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
  ############# Add Filter to List ####################
  # UI for displaying selected filters as bubbles
  output$selected_filters_ui <- renderUI({
    filters <- selected_filters$filters

    if (reset_trigger()) {
      filters <- NULL
      reset_trigger(FALSE)  # Reset the trigger after resetting the UI
    }
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
})

############### Apply Filter Button ####################
observeEvent(input$filter_apply_btn, {

  print("Filter button clicked")
  filters <- selected_filters$filters

  View(filters)


  if(is.null(filters)){
    print("selected filters is null")
    filter_by <- input$filter_by
    sub_filter <- input$sub_filter # Retrieve value of subfilter from input
    subset_SE <- vals$SEList[, eval(parse(text = paste0("vals$SEList$", filter_by))) == sub_filter]# subset the filter value in SE obj
    #Explanation : the values of filter_by and sub_filter have quote around it, we need those quotes removed for only filter_by in order for this command to execute properly
  }
  else{
    cat("Selected filters not null")
    # cat(filters)

    # Creates a temporary storage for SE
    subset_SE <- vals$SEList

    # Subsets the SE to only include what's listed in the filter
    for (filter in filters) {

      # Cleans filter and subfilter content
      filter_by <- gsub("'", "", filter$filter_by)
      sub_filter <- gsub("'", "", filter$sub_filter)

      # Subsets the SE so it only contains the
      subset_SE <- subset_SE[, !is.na(colData(subset_SE)[[filter_by]]) & colData(subset_SE)[[filter_by]] == sub_filter]
    }
    # View(subset_SE)
    my_data(as.data.frame(colData(subset_SE)))

    # Applies filtered changes to SEList
    vals$SEList <- subset_SE
  }
})

########### Reset Button ###################
observeEvent(input$filter_reset_btn, {
  # Reset selected filters to NULL
  selected_filters$filters <- NULL

  # Clear the UI element where the bubbles appeared
  output$selected_filters_ui <- renderUI({})
  reset_trigger(TRUE)
})

############# Summary Table ####################
output$filter_summary_table <- renderDT({
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
})
########### Visualize Tab ################

# Generates visualizations
observeEvent(input$visualize_btn, {
  filter_by <- input$visualize_filter_by

  data <- as.data.frame(vals$SEList@colData)
  View(data)

  if (!is.null(filter_by)) {
    # Generate the top graph
    output$top_visualization <- renderPlot({
      ggplot(data = data, aes_string(x = filter_by)) +
        geom_bar() +
        labs(x = filter_by, y = "Frequency", title = paste("Histogram of", filter_by, sep = " "))
    })

    # For Categorical Data
    # output$pieChart <- renderPlotly({
    #   # # Getting the dataset
    #   # values_list <- as.character(vals$SEList$filter_by)
    #   #
    #   # # Convert list to dataframe
    #   # df <- data.frame(Category = values_list)
    #   #
    #   # # Filter out NA or non-finite values
    #   # df <- df[!is.na(df$Category) & df$Category != "NA" & df$Category != "NaN", ]
    #   #
    #   # # Convert Category to factor
    #   # df$Category <- as.factor(df$Category)
    #   #
    #   # # Summarize counts
    #   # df_counts <- df %>%
    #   #   group_by(Category) %>%
    #   #   summarise(Count = n())
    #   #
    #   # plot_ly(
    #   #   data = df_counts,
    #   #   labels = ~Category,
    #   #   values = ~Count,
    #   #   type = "pie",
    #   #   textinfo = "percent",
    #   #   textposition = "inside",
    #   #   hoverinfo = "label+percent"
    #   # ) %>%
    #   #   layout(title = "Pie Chart Example")
    # })




    # For Numerical Data
    output$boxPlot <- renderPlot({

    })

  }
})

# Summary Stats Table
output$summaryStatsTable <- renderTable({
  dat <- list()
  dat['Number of Samples'] <- round(length(vals$SEList@colData@rownames))
  dat['Number of Covariates'] <- round(length(names(vals$SEList@colData)))
  df <- as.data.frame(unlist(dat))

  # Formatting
  df$temp <- rownames(df)
  colnames(df) <- c("", "Summary Statistics")
  df <- df[,c(2,1)]
  df[,2] <- as.character(round(df[,2]))
  return(df)
})

