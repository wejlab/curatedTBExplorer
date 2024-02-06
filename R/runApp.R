#' Run curatedTBExplorer app
#'
#' @return The shiny app will open
#'
#' @param dev Run the application in developer mode
#'
#' @examples
#' \dontrun{
#'   run_curatedTBExplorer()
#' }
#' @export
run_curatedTBExplorer <- function(dev = FALSE) {
  appDir <- system.file("shiny", package = "curatedTBExplorer")
  if (appDir == "") {
    stop("Could not find my app. Try re-installing 'mypackage'.", 
      call. = FALSE
    )
  }
  if (dev) {
    options(shiny.autoreload = TRUE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
