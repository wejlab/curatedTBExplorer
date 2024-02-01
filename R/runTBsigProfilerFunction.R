#' Runs the TBSignatureProfiler using selected datasets, profiles, and assays
#' 
#' The runTBsigProfilerFunction allows for different datasets, profiles, and assays
#' to be pulled into a datatable, allowing for easy searching, sorting, and visualization
#' 
#' @param selected_dataset The selected dataset to be viewed. Required.
#' @param selected_profiles The group of selected profiles to be viewed. Required.
#' 
#' @return A datatable with results
#' 
#' @examples 
#' R code here showing how your function works 

runTBsigProfilerFunction <- function(selected_dataset, selected_profiles) {
  #chooses the dataset
  if (selected_dataset == "TB_hiv") {
    dataset_data <- mkAssay(TB_hiv, log = TRUE, counts_to_CPM = TRUE)
  } else if (selected_dataset == "TB_indian") {
    dataset_data <- mkAssay(TB_indian, log = TRUE, counts_to_CPM = FALSE)
  } else {
    stop("Invalid dataset selected.")
  }
  
  #this runs the profiler: following is modified straight from TBSignatureProfiler vignette
  out <- capture.output({
    ssgsea_result <- runTBsigProfiler(
      input = dataset_data,
      useAssay = "log_counts",
      signatures = TBsignatures,
      algorithm = "ssGSEA",
      combineSigAndAlgorithm = TRUE,
      parallel.sz = 1,
      update_genes = FALSE
    )
  })
  
  #removes unscored signatures
  TBsignatures <- subset(TBsignatures, !(names(TBsignatures) %in% c("Chendi_HIV_2")))
  
  #info for the datatable
  selected_sigs <- unlist(selected_profiles)
  
  #this changes the columns dependent on the selected dataset
  if(selected_dataset == "TB_hiv"){
    column <- colData(ssgsea_result)[, c("Disease", selected_sigs)]
  } else if(selected_dataset == "TB_indian"){
    column <- colData(ssgsea_result)[, c("sample", selected_sigs)]
  }
  
  ssgsea_print_results <- as.data.frame( column )
  ssgsea_print_results[, 2:length(selected_sigs) + 1] <- round(ssgsea_print_results[, 2:length(selected_sigs) + 1], 4)
  #create and outputs the datatable
  datatable(ssgsea_print_results)
}