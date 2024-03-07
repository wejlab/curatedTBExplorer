#' Combines all of the downloaded datasets into a single summarized experiment object
#'
#' This is useful for applications such as the TBSignatureProfiler, where a summarized experiment is necessary
#'
#' @return A summarized experiment object
#'
#' @param
#'
#' @examples
#'
#' @export


combineExperiments <- function(datasets) {
  appDir <- system.file("shiny", package = "curatedTBExplorer")
  if (appDir == "") {
    stop("Could not find my function. Try re-installing 'curatedTBExplorer'.", call. = FALSE)
  }

  #inital test to combine the first two from datasets
  #will scale up to include all eventually, but easier to test on two
  first_se <- toSE(datasets[1])
  second_se <- toSE(datasets[2])

  #combined_se is the output se containing all dataset info
  combined_se <- first_se

  View(combined_se)
  View(second_se)

}
