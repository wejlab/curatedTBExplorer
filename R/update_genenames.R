#' Renames Genes Names In a List
#'
#' @return A character list of adjusted gene names
#'
#' @param siglist A character List
#'
#' @examples
#' \dontrun{
#'   tt <- c("SEPT4", "FCGR1A", "7-Sep")
#'   updatedtt <- update_genenames(tt)
#' }
#'
#' @export

update_genenames <- function(siglist) {
    newgenes <- suppressMessages(suppressWarnings(
        HGNChelper::checkGeneSymbols(siglist,
                                     unmapped.as.na = FALSE)))$Suggested.Symbol
    ind <- grep("//", newgenes)
    if (length(ind) != 0) newgenes[ind] <- strsplit(newgenes[ind],
                                                    " /// ")[[1]][1]
    return(newgenes)
}
