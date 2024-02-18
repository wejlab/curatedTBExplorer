# Reactive values to store selected studies
selected_studies <- reactiveVal(NULL) #might remove this block, it helped in the beginning but since changed the checkboxes to select-click it looks dumb

# Display study information table
output$study_table <- renderDT({
  datatable(study_data, options = list(
    pageLength = nrow(study_data),
    dom = 't',
    lengthMenu = c(5, 10, 15, 20),
    scrollX = TRUE,
    scrollY = "50vh",  # Set the height to 50% of the viewport height
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#4C516D', 'color': '#fff'});",
      "}"),
    rowCallback = JS(
      "function(row, data, index) {",
      "$(row).addClass('study-row');",
      "$(row).on('click', function() {",
      "Shiny.setInputValue('selected_study', data[1]);",
      "});",
      "}")),
    selection = "multiple"
  )
})

# Display study description based on selected studies

output$study_description <- renderPrint({
  if (!is.null(input$selected_study)) {
    capture.output({ #captures download status info
      selected_study_info <- curatedTBData(input$selected_study, dry.run = FALSE, curated.only = FALSE)
    })
    print(selected_study_info)
  }
})

# Reactive value to store whether Continue button is clicked
continue_clicked <- reactiveVal(FALSE)

# Observer to update reactive value when Continue button is clicked
observeEvent(input$continue, {
  continue_clicked(TRUE)
})

# Display the Summarize tab only if Continue button is clicked
observe({
  if (continue_clicked()) {
    updateTabsetPanel(session, "main", selected = "Summarize")
  }
})

# Render selected studies tables on the Summarize tab
output$selected_studies_table <- renderDT({
  if (!is.null(input$selected_studies)) {
    # You can add your logic here to fetch and display the data tables for selected studies
    # For demonstration purposes, a sample table is created
    data.frame(
      Study = input$selected_studies,
      Value = rnorm(length(input$selected_studies))
    )
  }
})

# Observer to capture selected row(s) and update the description
observe({
  if (!is.null(input$selected_study)) {
    selected_studies(input$selected_study)
  }
})
