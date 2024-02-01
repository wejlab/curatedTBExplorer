library(shiny)
library(shinythemes)
library(BiocManager)
library(TBSignatureProfiler)
library(DT)
library(SummarizedExperiment)

ui <- fluidPage(
    theme = shinytheme("united"),
    navbarPage("curatedTBExplorer",
        source(file.path("ui", "ui_01_visualization.R"), local = TRUE)$value
    )
)

server <- function(input, output, session) {
    source(file.path("server", "server_01_visualization.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)