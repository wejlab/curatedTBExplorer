#' Combines all of the downloaded datasets into a single summarized experiment object
#'
#' This is useful for applications such as the TBSignatureProfiler, where a summarized experiment is necessary
#'
#' @return A summarized experiment object
#'
#' @param datasets
#'
#' @examples 
#' \dontrun{
#'   combinedExperiments <- combineExperiments(datasets)
#' }
#'
#' @export


#NOTE: I believe this is now redundant, as the combined experiment is now carried out directly with the download
#step 1 - create generalized list data to be applied to the final se
#step 2 - combine the list data from each
#step 3 - combine colData from each - should be fine once list data is consistent
#step 4 - combine metadata so the original study id is known

combineExperiments <- function(datasets) {
  appDir <- system.file("shiny", package = "curatedTBExplorer")
  if (appDir == "") {
    stop("Could not find my function. Try re-installing 'curatedTBExplorer'.", call. = FALSE)
  }

  #inital test to combine the first two from datasets
  #will scale up to include all eventually, but easier to test on two
  first_se <- toSE(datasets[1])
  second_se <- toSE(datasets[2])
  View(first_se)
  View(second_se)
  test <- mergeSEs( list(se1=first_se, se2=second_se) )
  View(test)
  #different information that needs to be access for list Data
  # print(names(first_se@colData@listData[1]))
  # print(second_se@colData@listData)
  # View(first_se@colData@listData)
  # View(second_se@colData@listData)

  # View(colData(first_se))
  # View(colData(second_se))

  # merged_colData <- c(colData(first_se), colData(second_se))
  # View(merged_colData)

  #combined_se is the output se containing all dataset info
  # combined_se <- SummarizedExperiment

  # View(combined_se)
  # View(second_se)

}
