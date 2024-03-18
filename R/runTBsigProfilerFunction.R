#' Runs the TBSignatureProfiler using selected datasets, profiles, and assays
#'
#' The runTBsigProfilerFunction allows for different datasets, profiles, and assays
#' to be pulled into a datatable, allowing for easy searching, sorting, and visualization
#'
#' @return A datatable with results
#'
#' @param selected_dataset The selected dataset (summarized experiment) to be viewed. Required.
#' @param selected_profiles The group of selected profiles to be viewed. Required.
#'
#' @examples
#' \dontrun{
#' runTBsigProfilerFunction(SE, profiles)
#' }
#'
#' @export

#notes:
  #need to check if the assay_curated corresponds to log_counts like I think it does

#derived from the TBSignatureProfiler Vignette
runTBsigProfilerFunction <- function(selected_dataset, selected_profiles, selected_assay) {
  appDir <- system.file("shiny", package = "curatedTBExplorer")
  if (appDir == "") {
    stop("Could not find my function. Try re-installing 'curatedTBExplorer'.", call. = FALSE)
  }
  View(selected_assay)
  #creates the assays for processing
  #likely want to have additional parameter to select which to run
  #also need to create all 4, currently only have 2, easy fix
  selected_dataset <- mkAssay(selected_dataset, "assay_curated")
  # View(selected_dataset)

  #random views for ease of troubleshooting
  # View(selected_dataset)
  # View(names(selected_dataset))
  # View(assay(selected_dataset))

  #runs the tbsigprofiler using the parameter info
  #still need to add info to capture the desired algorithm, assays, etc
  out <- capture.output({
    ssgsea_result <- runTBsigProfiler(
      input = selected_dataset,         #input is the selected_dataset parameter
      useAssay = selected_assay,   #will need to change based on user input
      signatures = TBsignatures,        #may potentially need to change? though I don't think so
      algorithm = "ssGSEA",             #need to add user input to select algorithms
      combineSigAndAlgorithm = TRUE,
      parallel.sz = 4,
      update_genes = FALSE
    )
  })

  #Removes unscored signatures
    #this came from the vignette, not sure we actually need
    #or may need to alter based on our specific needs
  # TBsignatures <- subset(TBsignatures, !(names(TBsignatures) %in% c("Chendi_HIV_2")))

  #Info for the datatable
  selected_sigs <- unlist(selected_profiles)

  #this currently uses PatientID as the info in the datatable, however this will need to change based on filter
  ssgsea_print_results <- as.data.frame(colData(ssgsea_result)[ , c("PatientID", selected_sigs)])
  ssgsea_print_results[, 2:4] <- round(ssgsea_print_results[, 2:4], 4)

  #datatable is returned
  # DT::datatable(ssgsea_print_results)
  #datatable info is returned, as well as the ssgsea_result itself
  return(list(
    datatable = DT::datatable(ssgsea_print_results),
    ssgsea_result = ssgsea_result
  ))
}
