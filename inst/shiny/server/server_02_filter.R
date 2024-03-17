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

observeEvent(input$filter_tb_btn, {
  print("Filter TB button clicked")
  tb_status <- input$tb_status # Retrieve value of tb_status from input
  # Subset our SE in specific category
  PTB <- combined_studies[, combined_studies$TBStatus == "PTB"] # subset the active group in SE obj
  LTBI <- combined_studies[, combined_studies$TBStatus == "LTBI"] # subset the control group in SE obj
  if (tb_status == "PTB") {
    my_data(as.data.frame(colData(PTB)))
  } else {
    my_data(as.data.frame(colData(LTBI)))
  }
})
observeEvent(input$filter_hiv_btn, {
  print("Filter HIV button clicked")
  hiv_status <- input$hiv_status # Retrieve value of hiv_status from input
  HIV_Positive <- combined_studies[, combined_studies$HIVStatus == "Positive"]
  HIV_Negative <- combined_studies[, combined_studies$HIVStatus == "Negative"]
  if (hiv_status == "Positive") {
    my_data(as.data.frame(colData(HIV_Positive)))
  } else {
    my_data(as.data.frame(colData(HIV_Negative)))
  }
})
observeEvent(input$filter_diabetes_btn, {
  print("Filter Diabetes button clicked")
  diabetes_status <- input$diabetes_status # Retrieve value of diabetes_status from input
  Diabetes_Positive <- combined_studies[, combined_studies$DiabetesStatus == "Positive"]
  Diabetes_Negative <- combined_studies[, combined_studies$DiabetesStatus == "Negative"]
  if (diabetes_status == "Positive") {
    my_data(as.data.frame(colData(Diabetes_Positive)))
  } else {
    my_data(as.data.frame(colData(Diabetes_Negative)))
  }
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
