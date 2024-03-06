#add additional inputs for algorithm

tabPanel(
  "TBSigProfiler",
  sidebarPanel(
    # dropdown to select the dataset - not sure we actually need this implemented, as i believe we will just use the downloaded datasets?
    # selectInput("dataset", "Select Dataset:", choices = temp$downloaded_datasets),
    # dropdowns to select the profile information
    selectInput("profile1", "Select Profile 1:", choices = names(TBsignatures), selected = "Anderson_42"),
    selectInput("profile2", "Select Profile 2:", choices = names(TBsignatures), selected = "Anderson_OD_51"),
    selectInput("profile3", "Select Profile 3:", choices = names(TBsignatures), selected = "Gong_OD_4"),
    # dropdown to select assay
    # not currently working
    selectInput("assay", "Select Assay:", choices = c("log_counts", "log_counts_cpm", "counts"), selected = "log_counts")
  ),
  # titlePanel(""),
  DTOutput("ssgsea_table"),
  actionButton("begin", "Begin")
)
