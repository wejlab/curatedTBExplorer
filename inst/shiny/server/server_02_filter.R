###########Reactive values for various tasks###############
my_data <- reactiveVal(NULL)

# reactive Value to store selected filters
selected_filters <- reactiveValues(filters = NULL)
# Holds the subsetted SE
subset_SE <- reactiveVal(NULL)
# Reactive value to trigger UI reset
reset_trigger <- reactiveVal(FALSE)

#################Filter Side Panel######################
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
  print(paste("Filter by:", filter_by))
  if (!is.null(filter_by)) {
    column_index <- which(vals$covars == filter_by)
    print(paste("Column index:", column_index))

    if (length(column_index) == 0) {
      print("Column index is out of bounds or filter_by does not exist in vals$covars")
      return(NULL)
    }

    column_values <- vals$colData[, column_index]
    print(paste("Column values:", column_values))

    # Filter out NA values
    column_values <- column_values[!is.na(column_values)]
    #print(paste("Filtered column values:", column_values)) #Use this console output to debug code, keep commented, delete when done

    # Check if the column has any non-NA values
    if (length(column_values) == 0) {
      unique_column_values <- NULL
    } else {
      unique_column_values <- unique(column_values)
    }

    # Render the dynamic selectInput based on the selected filter_by choice
    output$dynamic_filter <- renderUI({
      if (is.null(unique_column_values)) {
        shinyjs::disable("add_filter_btn") #Don't allow use to click Add Filter if the filter is empty or app will crash
        tags$div(paste("No", filter_by, "Data in Selected Studies"))
      } else {
        shinyjs::enable("add_filter_btn")
        if (filter_by == "Age") {
          sliderInput("sub_filter", "Select Age Range:",
                      min = min(unique_column_values, na.rm = TRUE),
                      max = max(unique_column_values, na.rm = TRUE),
                      value = c(min(unique_column_values, na.rm = TRUE), max(unique_column_values, na.rm = TRUE))
          )
        } else {
          tagList(
            selectInput("sub_filter", filter_by, choices = unique_column_values),
          )
        }
      }
    })
  }
})


############### Add Filter Button ####################
# Add a new filter when the user clicks on "Add Filter" button
observeEvent(input$add_filter_btn, {
  print(input$filter_by)
  print(input$sub_filter)
  print(selected_filters$filters)

  if(input$filter_by == "" || (input$filter_by!="Age" && input$sub_filter == "")) {
    showNotification("Please confirm a selected study first", type = "warning")
  } else {
    filters <- selected_filters$filters
    new_filter_index <- length(filters) + 1

    # Create a new filter object and add it to selected_filters
    #But adapt to range for "age" filter
    if (input$filter_by == "Age") {
      selected_filters$filters[[new_filter_index]] <- list(
        filter_by = input$filter_by,
        sub_filter = list(min = input$sub_filter[1], max = input$sub_filter[2])
      )
    } else {
      selected_filters$filters[[new_filter_index]] <- list(
        filter_by = input$filter_by,
        sub_filter = input$sub_filter
      )
    }
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
          if (filters[[i]]$filter_by == "Age") {
            div(
              paste(filters[[i]]$filter_by, ":",
                    filters[[i]]$sub_filter$min, "to", filters[[i]]$sub_filter$max),
              class = "filter-bubble",
            )
          } else {
            div(
              paste(filters[[i]]$filter_by, ":", filters[[i]]$sub_filter),
              class = "filter-bubble",
              # actionButton(paste0("remove_filter_btn_", i), "x", class = "btn btn-danger btn-sm remove-filter-btn")
            )
          }
          ,
          br()
        )
      })

      # Wrap filter bubbles in a div
      div(filter_bubbles)
    })
  }
})

############### Apply Filter Button ####################
observeEvent(input$filter_apply_btn, {

  print("Filter button clicked")
  filters <- selected_filters$filters

  if(is.null(filters)){
    showNotification("Please add a filter first", type = "warning")
  }
  else{

    # Creates a temporary storage for SE
    subset_SE <- vals$SEList

    # Subsets the SE to only include what's listed in the filter
    for (filter in filters) {

      # Cleans filter and subfilter content
      filter_by <- gsub("'", "", filter$filter_by)
      if (filter_by == "Age") {
        min_age <- filter$sub_filter$min
        max_age <- filter$sub_filter$max
        subset_SE <- subset_SE[, !is.na(colData(subset_SE)[[filter_by]]) &
                                 colData(subset_SE)[[filter_by]] >= min_age &
                                 colData(subset_SE)[[filter_by]] <= max_age]
      } else {
        sub_filter <- gsub("'", "", filter$sub_filter)

        # Subsets the SE so it only contains the
        subset_SE <- subset_SE[, !is.na(colData(subset_SE)[[filter_by]]) & colData(subset_SE)[[filter_by]] == sub_filter]
      }
    }
    # View(subset_SE)
    my_data(as.data.frame(colData(subset_SE)))

    # Applies filtered changes to SEList
    vals$SEList <- subset_SE
  }
})

########### Reset Button ###################
observeEvent(input$filter_reset_btn, {
  tryCatch({
    # Reset selected filters to NULL
    selected_filters$filters <- NULL

    # Clear the UI element where the bubbles appeared
    output$selected_filters_ui <- renderUI({})
    reset_trigger(TRUE)

    # Resets all changes made
    vals$SEList <- vals$backupSE

    my_data(as.data.frame(colData(vals$SEList)))
  }, error = function(e) {
    # cat("Error:", conditionMessage(e), "\n")
  })
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
    # Generate histogram
    output$histogram <- renderPlot({
      ggplot(data = data, aes_string(x = filter_by)) +
        geom_histogram(binwidth = 1, fill = "#4C516D", color = "black") +
        labs(x = filter_by, y = "Frequency", title = paste("Histogram of", filter_by, sep = " ")) +
        theme_minimal()
    })

    # Generate a boxplot
    output$boxPlot <- renderPlot({
      ggplot(data = data, aes_string(x = filter_by, y = filter_by)) +
        geom_boxplot(fill = "orange", color = "black") +
        labs(x = filter_by, y = filter_by, title = paste("Box Plot of", filter_by, sep = " ")) +
        theme_minimal()
    })

    # Generate a scatter plot (if applicable)
    output$scatterPlot <- renderPlot({
      ggplot(data = data, aes_string(x = filter_by, y = filter_by)) +
        geom_point(color = "green") +
        labs(x = filter_by, y = filter_by, title = paste("Scatter Plot of", filter_by, sep = " ")) +
        theme_minimal()
    })

    # Generate a pie chart for categorical data
    output$pieChart <- renderPlotly({
      cat_data <- data[[filter_by]]
      cat_data <- as.factor(cat_data)
      cat_data <- cat_data[!is.na(cat_data)]

      cat_data_summary <- as.data.frame(table(cat_data))
      colnames(cat_data_summary) <- c("Category", "Count")

      plot_ly(cat_data_summary, labels = ~Category, values = ~Count, type = 'pie') %>%
        layout(title = paste("Pie Chart of", filter_by))
    })
  }
})

# Summary Stats Table
output$summaryStatsTable <- renderTable({
  if(!is.null(vals$SEList)) {
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
  }
})

