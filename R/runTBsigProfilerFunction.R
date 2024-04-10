#' Runs the TBSignatureProfiler using selected datasets, profiles, and assays
#'
#' The runTBsigProfilerFunction allows for different datasets, profiles, and assays
#' to be pulled into a datatable, allowing for easy searching, sorting, and visualization
#'
#' @return A datatable with results
#'
#' @param selected_dataset The selected dataset (summarized experiment object) to be viewed. Required.
#' @param selected_profiles The group of selected profiles (Anderson_42/Berry_393/etc) to be viewed. Allows for only select profiles to appear within dt output. Required.
#' @param selected_assay The assay used in the profiler (counts, log counts, etc). Required.
#' @param selected_algorithm The algorithm (GSVA/ssGSEA) to be used in the tbSigProfiler. Required
#' @param selected_signatures The signatures that are available for the profiler to run on. Necessary, as updates to TBsignatures occur within this app. Required.
#'
#' @examples
#' \dontrun{
#' runTBsigProfilerFunction(selected_dataset, selected_profiles, selected_assay, selected_algorithm)
#' }
#'
#' @export

#notes:
  #need to check if the assay_curated corresponds to log_counts like I think it does

#derived from the TBSignatureProfiler Vignette
runTBsigProfilerFunction <- function(selected_dataset, selected_profiles, selected_assay, selected_algorithm, selected_signatures) {
  appDir <- system.file("shiny", package = "curatedTBExplorer")
  if (appDir == "") {
    stop("Could not find my function. Try re-installing 'curatedTBExplorer'.", call. = FALSE)
  }
  #runs the tbsigprofiler using the parameter info
  #still need to add info to capture the desired algorithm, assays, etc

  out <- capture.output({
    profiler_result <- runTBsigProfiler(
      input = selected_dataset,         #input is the selected_dataset parameter
      useAssay = selected_assay,
      signatures = selected_signatures,
      algorithm = selected_algorithm,
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
  profiler_print_results <- as.data.frame(colData(profiler_result)[ , c("PatientID", selected_sigs)])
  profiler_print_results[, 2:4] <- round(profiler_print_results[, 2:4], 4)
  return(list(
    datatable = DT::datatable(profiler_print_results),
    profiler_result = profiler_result
  ))
}
