# Reactive values to store selected studies
selected_studies <- reactiveVal(NULL) #might remove this block, it helped in the beginning but since changed the checkboxes to select-click it looks dumb

#Grab the selected checkboxes from the ui section, and only output these within the datatable
#super ugly block of code, but it works. Will make prettier later - Andrew
#probably in a function in its own file? maybe
selected_columns <- reactive({
  selected_columns <- c("Study", "Notes")  #study id and notes are always visible

  #these correspond to the checkboxes within the ui_upload code
  #if they are selected, they are added to selected_columns, which is used within the datatable output
  if(input$filterDSPlatform) {
    selected_columns <- c(selected_columns, "Platform")
  }
  if(input$filterDSGeoRegion) {
    selected_columns <- c(selected_columns, "GeographicalRegion")
  }
  if(input$filterDSTissue) {
    selected_columns <- c(selected_columns, "Tissue")
  }
  if(input$filterDSAge) {
    selected_columns <- c(selected_columns, "Age")
  }
  if(input$filterDSHIV) {
    selected_columns <- c(selected_columns, "HIVStatus")
  }
  if(input$filterDSMethod) {
    selected_columns <- c(selected_columns, "DiagnosisMethod")
  }
  if(input$filterDSControl) {
    selected_columns <- c(selected_columns, "Control")
  }
  if(input$filterDSLTBI) {
    selected_columns <- c(selected_columns, "LTBI")
  }
  if(input$filterDSPTB) {
    selected_columns <- c(selected_columns, "PTB")
  }
  if(input$filterDSOD) {
    selected_columns <- c(selected_columns, "OD")
  }
  if(input$filterDSTotal) {
    selected_columns <- c(selected_columns, "Total")
  }
  if(input$filterDSType) {
    selected_columns <- c(selected_columns, "GeneralType")
  }

  return(selected_columns)
})

# Display study information table
output$study_table <- renderDT({

  #these were added to grab the selected information from the checkboxes within the ui
  #they then create a new set of data to be rendered in the datatable
  current_columns <- selected_columns()
  selected_study_data <- study_data[, current_columns, drop = FALSE]

  datatable(selected_study_data, options = list(
    pageLength = nrow(selected_study_data),
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
