# Reactive values for various tasks
selected_studies <- reactiveVal(NULL)  # this is used to update the selected studies accordingly
continue_clicked <- reactiveVal(FALSE)  # stores if Continue button is clicked
object_list <-reactiveVal(NULL)
my_data <-reactiveVal(NULL)
combined_studies <-reactiveVal(NULL)
PTB <-reactiveVal(NULL)
LTBI <-reactiveVal(NULL)


# Render the selected studies text
output$selected_studies_text <- renderText({
  paste("Selected Studies: ", paste(selected_studies(), collapse = ", "))
})

reactive({
  #move the selected studies in a single object list
  object_list <- curatedTBData(selected_studies(), dry.run = FALSE, curated.only = TRUE)
  #Combine the studies together in a single SE object
  combined_studies <- combine_objects(object_list, experiment_name = "assay_curated", update_genes = FALSE)
  
  
  observeEvent(input$filter_tb_btn, {
    #sum_table <- as.data.frame(colData(combined_studies))
    #Subset our SE in specific category
    PTB <- combined_studies[, combined_studies$TBStatus == "PTB"] #subset the active group in SE obj
    LTBI <- combined_studies[, combined_studies$TBStatus == "LTBI"] #subset the control group in SE obj
    if(tb_status == "PTB"){ my_data <- as.data.frame(PTB)
    }else { my_data <- as.data.frame(LTBI)}
    
    
  })
  observeEvent(input$filter_hiv_btn, {
    HIV_Positive <- combined_studies[, combined_studies$HIVStatus == "Positive"]
    HIV_Negative <- combined_studies[, combined_studies$HIVStatus == "Negative"]
    if(hiv_status == "Positive"){ my_data <- as.data.frame(HIV_Positive)
    }else { my_data <- as.data.frame(HIV_Negative)}
  })
  observeEvent(input$filter_diabetes_btn, {
    Diabetes_Positive <- combined_studies[, combined_studies$DiabetesStatus == "Positive"]
    Diabetes_Negative <- combined_studies[, combined_studies$DiabetesStatus == "Negative"]
    if(diabetes_status == "Positive"){ my_data <- as.data.frame( Diabetes_Positive)
    }else { my_data <- as.data.frame(Diabetes_Negative)}
    
  })
  
})
#NOT WORKING
#  output$filter_summary_table <-  renderUI({
#    #DTOutput(my_data)
# 
#  })
# 
# output$filter_summary_top_plot <- renderPlot({
# })
# 
# output$filter_summary_bottom_plot <- renderPlot({
# })
