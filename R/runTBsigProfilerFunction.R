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

#derived from the TBSignatureProfiler Vignette
runTBsigProfilerFunction <- function(selected_dataset, selected_profiles) {
  appDir <- system.file("shiny", package = "curatedTBExplorer")
  if (appDir == "") {
    stop("Could not find my function. Try re-installing 'curatedTBExplorer'.", call. = FALSE)
  }

  #assays to be created
  # selected_dataset <- mkAssay(selected_dataset, counts, log = TRUE, counts_to_CPM = TRUE)

  #attempts to create the necessary assays, as mkAssay was having issues
  #also not working currently
  # log_counts_assay <- log(assay(selected_dataset))
  # counts_CPM_assay <- assay(selected_dataset) / colSums(assay(selected_dataset)) * 1e6
  # log_counts_CPM_assay <- log(counts_CPM_assay + 1)
  #
  # n_rows <- nrow(log_counts_assay)
  # n_cols <- ncol(log_counts_assay)
  #
  # selected_dataset$assays <- SimpleList(
  #   log_counts = matrix(log_counts_assay, nrow = n_rows, ncol = n_cols),
  #   counts_CPM = matrix(counts_CPM_assay, nrow = n_rows, ncol = n_cols),
  #   log_counts_CPM = matrix(log_counts_CPM_assay, nrow = n_rows, ncol = n_cols)
  # )
  # assays(selected_dataset) <- selected_dataset$assays

  View(selected_dataset$assays)

  #runs the tbsigprofiler
  out <- capture.output({
    ssgsea_result <- runTBsigProfiler(
      input = selected_dataset,
      useAssay = NULL, #will need to change based on user input
      signatures = TBsignatures, #may potentially need to change? though I don't think so
      algorithm = "ssGSEA", #need to add user input to select algorithms
      combineSigAndAlgorithm = TRUE,
      parallel.sz = 1,
      update_genes = FALSE
    )
  })

  #Removes unscored signatures
  TBsignatures <- subset(TBsignatures, !(names(TBsignatures) %in% c("Chendi_HIV_2")))

  #Info for the datatable
  selected_sigs <- unlist(selected_profiles)

  #This changes the columns dependent on the selected dataset
  ssgsea_print_results <- as.data.frame(colData(ssgsea_result)[, c("Disease", selected_sigs)])
  ssgsea_print_results[, 2:length(selected_sigs) + 1] <- round(ssgsea_print_results[, 2:length(selected_sigs) + 1], 4)

  #Create and output the datatable
  datatable(ssgsea_print_results)
}
