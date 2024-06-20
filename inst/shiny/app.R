library(BiocManager)
library(caret)
library(curatedTBData)
library(curatedTBExplorer)
library(dplyr)
library(DT)
library(ggplot2)
library(HGNChelper)
library(MultiAssayExperiment)
library(plotly)
library(SEtools)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(snow)
library(SummarizedExperiment)
library(sva)
library(TBSignatureProfiler)

# Get the list of available studies
data("DataSummary", package = "curatedTBData")
study_data <- DataSummary

# Adjusting column names due to changes in curatedTBData DataSummary
if ("GEOAccession" %in% colnames(study_data)) {
  colnames(study_data)[colnames(study_data) == "GEOAccession"] <- "Study"
}
if ("Country/Region" %in% colnames(study_data)) {
  colnames(study_data)[colnames(study_data) == "Country/Region"] <- "GeographicalRegion"
}

ui <- fluidPage(
    theme = shinytheme("flatly"),
    navbarPage(
        "curatedTBExplorer",
        source(file.path("ui", "ui_01_upload.R"), local = TRUE)$value,
        source(file.path("ui", "ui_02_filter.R"), local = TRUE)$value,
        source(file.path("ui", "ui_03_tbsignatureprofiler.R"), local = TRUE)$value,
        source(file.path("ui", "ui_04_ml.R"), local = TRUE)$value
    )
)

server <- function(input, output, session) {
    tryCatch({
      source(file.path("server", "server_01_upload.R"), local = TRUE)$value
      source(file.path("server", "server_02_filter.R"), local = TRUE)$value
      source(file.path("server", "server_03_tbsignatureprofiler.R"), local = TRUE)$value
      source(file.path("server", "server_04_ml.R"), local = TRUE)$value
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
}

shinyApp(ui = ui, server = server)
