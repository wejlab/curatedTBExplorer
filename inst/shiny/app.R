library(BiocManager)
library(curatedTBData)
library(dplyr)
library(DT)
library(MultiAssayExperiment)
library(plotly)
library(SEtools)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(snow)
library(SummarizedExperiment)
library(TBSignatureProfiler)


# Get the list of available studies
data("DataSummary", package = "curatedTBData")
study_data <- DataSummary

ui <- fluidPage(
    theme = shinytheme("flatly"),
    navbarPage(
        "curatedTBExplorer",
        source(file.path("ui", "ui_01_upload.R"), local = TRUE)$value,
        source(file.path("ui", "ui_02_filter.R"), local = TRUE)$value,
        source(file.path("ui", "ui_03_tbsignatureprofiler.R"), local = TRUE)$value
    )
)

server <- function(input, output, session) {
    source(file.path("server", "server_01_upload.R"), local = TRUE)$value
    source(file.path("server", "server_02_filter.R"), local = TRUE)$value
    source(file.path("server", "server_03_tbsignatureprofiler.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
