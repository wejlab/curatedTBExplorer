#To Do:
  #add multithreading for download
  #output the downloads to the summary tab (don't know how yet - Andrew) - I think we need to save to rda file too
  #add "default" dataset so summary can be viewed without downloading anything
    #this may allow us to ignore the summary tab only appearing after continue button
  #fix issue where summary tab should only appear once continue button is clicked
  #fix issue regarding deselecting datasets - works sometimes
  #Review and delete unneccessary code
  #change the selected columns if statements to a function so it's prettier
  #maybe add progress bar for downloads for user? or some pop up to let them know something is happening

#reactive values for various tasks
selected_studies <- reactiveVal(NULL) #this is used to update the selected studies accordingly
continue_clicked <- reactiveVal(FALSE) #stores if Continue button is clicked


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


#Display study information table
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
    scrollY = "75vh",  # Set the height to 50% of the viewport height
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


#Display study description based on selected studies
#I don't think we need this anymore - Andrew
#download is achieved in the following observeEvent block
output$study_description <- renderPrint({
  if (!is.null(input$selected_study)) {
    capture.output({ #captures download status info
      selected_study_info <- curatedTBData(input$selected_study, dry.run = FALSE, curated.only = FALSE)
    })
    print(selected_study_info)
  }
})


#updates if continue button clicked, also begins the download process for all selected studies
observeEvent(input$continue, {
  continue_clicked(TRUE)
  #if there are studies selected, this block executes
  if (!is.null(selected_studies())) {
    #lapply is used here
    #NOTE: still need to apply multithreaded download, however the base download functionality is present
    selected_studies_info <- lapply(selected_studies(), function(study_id) {
      curatedTBData(study_id, dry.run = FALSE, curated.only = FALSE)
    })
    #not sure if we actually are going to need this, keeping for now
    for (study_info in selected_studies_info) {
      print(study_info)
      View(study_info)
    }
  }
})


#Display the Summarize tab only if Continue button is clicked
#this isn't working - Andrew
#i dont think we need this, as if we have default data then the summarize tab should always be available
observe({
  if (continue_clicked()) {
    updateTabsetPanel(session, "main", selected = "Summarize")
  }
})


#render selected studies tables on the Summarize tab
#not currently working - Andrew
#believe we need to change the selected_studies to work with the selected_studies() as I did for download
output$selected_studies_table <- renderDT({
  if (!is.null(input$selected_studies)) {
    data.frame(
      Study = input$selected_studies,
      Value = rnorm(length(input$selected_studies))
    )
  }
})


#added to ensure that every study that is clicked is added
#deselection currently kind of works, except when you deselect the previously selected study
#it stays in the selected_studies(), leading to unexpected behavior
observeEvent(input$selected_study, {
  current_selection <- isolate(input$selected_study)
  current_studies <- selected_studies()
  #uses the setdiff function to set the selected studies correctly
  if (current_selection %in% current_studies) {
    selected_studies(setdiff(current_studies, current_selection))
  } else {
    selected_studies(c(current_studies, current_selection))
  }
  #allows you to see the selected studies in r studio
  View(selected_studies())
})


#I don't think we need this anymore - Andrew
# Observer to capture selected row(s) and update the description
# observe({
#   if (!is.null(input$selected_study)) {
#     selected_studies <- c(selected_studies, input$selected_study)
#   }
# })
