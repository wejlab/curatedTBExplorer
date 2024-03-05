# Render the selected studies text
  output$selected_studies_text <- renderText({
    paste("Selected Studies: ", paste(selected_studies(), collapse = ", "))
  })

  # Filter data based on user input
  observeEvent(input$filter_button, {

    #temprary, allow you to see the downloaded datasets and how to access

    View(the$downloaded_datasets)

    #filterData gives me errors - andrew
    filtered_data <- filterData(selected_studies(),
                                filter_by = input$filter_by,
                                tb_status = input$tb_status,
                                hiv_status = input$hiv_status,
                                diabetes_status = input$diabetes_status)

    # Update summary plots
    output$summary_plots <- renderPlot({
      # ... (code for generating summary plots)
    })
  })

  output$summary_table <- renderDT({
    datatable(the$downloaded_datasets)
  })

  # Reset the form
  observeEvent(input$reset_button, {
    # ... (code to reset input fields)
  })

    # Reactive values for various tasks
  selected_studies <- reactiveVal(NULL)  # this is used to update the selected studies accordingly
  continue_clicked <- reactiveVal(FALSE)  # stores if Continue button is clicked

  # Taking all the column names from the experiment list













