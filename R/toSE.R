#' Converts MultiAssayExperiment to SummarizedExperiment Object
#'
#' @return MAE will be converted to SE
#'
#' @param MAE MultiAssayExperiment
#'
#' @examples
#' \dontrun{
#'   SE <- toSE(MAE)
#' }
#'
#' @export

toSE <- function(MAE) {
  appDir <- system.file("shiny", package = "curatedTBExplorer")
  if (appDir == "") {
    stop("Could not find my function. Try re-installing 'curatedTBExplorer'.",
         call. = FALSE
    )
  }
  MAECleaned <- intersectRows(MAE[[1]])
  assay_data <- assays(MAECleaned)
  col_data <- colData(MAECleaned)
  row_data <- rowRanges(assay_data$assay_curated)
  se <- SummarizedExperiment(assays=assay_data, colData=col_data, rowData=row_data)
  return (se)
}
