# To Do:
# Finish local download functionality
# Perhaps allow specification of exact core (multithreading) usage
# Add "default" dataset so summary can be viewed without downloading anything
# Fix issue regarding deselecting datasets - works sometimes
# Fix issue where if selections are made, then user changes table options, deselection occurs on ui side

# Reactive values for various tasks
selected_studies <- reactiveVal(NULL) # this is used to update the selected studies accordingly
continue_clicked <- reactiveVal(FALSE) # stores if Continue button is clicked
multithread_value <- reactiveVal(TRUE)
curated_only <- reactiveVal(TRUE)
local_download <- reactiveVal(FALSE)


# emptyList <- list()

# Creates reactive values for later use
vals <- reactiveValues(
  # Holds list of MAEs that are locally downloaded by user
  localMAEList = list(),

  # Holds active list of MAEs in use
  MAEList = list(),

  # Holds list of MAEs to download
  selectedDownloadList = list()
)

# Error handling for downloading issues
tryCatch(
  {
    # Sets pathing and directories for reading and saving .rds files
    localMAEListDir <- system.file("extdata/localMAEList.rds", package = "curatedTBExplorer")
    extdataDir <- system.file("extdata", package = "curatedTBExplorer")
    localMAEListPath <- file.path(extdataDir, "localMAEList.rds")
    
    # If 
    # fileExists <- file.exists(localMAEListDir)
    if (!file.exists(localMAEListDir)) {
      saveRDS(list(), file = localMAEListPath)
    }
    vals$localMAEList <- readRDS(localMAEListDir)
    cat("Added local download to localMAEList rective value")
  },
  error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    cat("Might be due to not installing via devtools::install_github(\"wejlab/curatedTBExplorer\")")
  }
)


# Grab the selected checkboxes from the ui section, and only output these within the datatable
# super ugly block of code, but it works. Will make prettier later - Andrew
# probably in a function in its own file? maybe
selected_columns <- reactive({
  selected_columns <- c("Study", "Notes") # study id and notes are always visible

  # these correspond to the checkboxes within the ui_upload code
  # if they are selected, they are added to selected_columns, which is used within the datatable output
  if (input$filterDSPlatform) {
    selected_columns <- c(selected_columns, "Platform")
  }
  if (input$filterDSGeoRegion) {
    selected_columns <- c(selected_columns, "GeographicalRegion")
  }
  if (input$filterDSTissue) {
    selected_columns <- c(selected_columns, "Tissue")
  }
  if (input$filterDSAge) {
    selected_columns <- c(selected_columns, "Age")
  }
  if (input$filterDSHIV) {
    selected_columns <- c(selected_columns, "HIVStatus")
  }
  if (input$filterDSMethod) {
    selected_columns <- c(selected_columns, "DiagnosisMethod")
  }
  if (input$filterDSControl) {
    selected_columns <- c(selected_columns, "Control")
  }
  if (input$filterDSLTBI) {
    selected_columns <- c(selected_columns, "LTBI")
  }
  if (input$filterDSPTB) {
    selected_columns <- c(selected_columns, "PTB")
  }
  if (input$filterDSOD) {
    selected_columns <- c(selected_columns, "OD")
  }
  if (input$filterDSTotal) {
    selected_columns <- c(selected_columns, "Total")
  }
  if (input$filterDSType) {
    selected_columns <- c(selected_columns, "GeneralType")
  }

  return(selected_columns)
})


# Display study information table
output$study_table <- renderDT({
  # these were added to grab the selected information from the checkboxes within the ui
  # they then create a new set of data to be rendered in the datatable
  current_columns <- selected_columns()
  selected_study_data <- study_data[, current_columns, drop = FALSE]

  datatable(selected_study_data,
    options = list(
      pageLength = nrow(selected_study_data),
      dom = "t",
      lengthMenu = c(5, 10, 15, 20),
      scrollX = TRUE,
      scrollY = "65vh", # Set the height to 50% of the viewport height
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#4C516D', 'color': '#fff'});",
        "}"
      ),
      rowCallback = JS(
        "function(row, data, index) {",
        "$(row).addClass('study-row');",
        "$(row).on('click', function() {",
        "Shiny.setInputValue('selected_study', data[1]);",
        "});",
        "}"
      )
    ),
    selection = "multiple"
  )
})

# observes the checkbox for the multithread
observeEvent(input$dLMultiThread, {
  multithread_value(input$dLMultiThread)
})

# observes the checkbox for curated or not
observeEvent(input$dLCurated, {
  curated_only(input$dLCurated)
  # View(curated_only())
})

# observes the checkbox for local download or not
observeEvent(input$dLLocal, {
  local_download(input$dLLocal)
  # View(local_download())
})

observeEvent(input$clearLocalDownload, {
  vals$localMAEList <- list()
  extdataDir <- system.file("extdata", package = "curatedTBExplorer")
  localMAEListPath <- file.path(extdataDir, "localMAEList.rds")
  saveRDS(list(), file = localMAEListPath)
  cat("Cleared Local Download")
})


# updates if continue button clicked, also begins the download process for all selected studies
observeEvent(input$continue, {
  continue_clicked(TRUE)

  # if there are studies selected, this block executes
  if (!is.null(selected_studies())) {
    # If local download coincides with studies in the selected_studies() download list,
    # it removes them from selected_studies() and adds the local download to the MAEList
    # for (studyName in selected_studies()) {
    #   if (studyName %in% names(vals$localMAEList)) {
    #     selected_studies() <- selected_studies()[selected_studies() != studyName]
    #     cat("Removed '", studyName, "' from the download list due to previous download.", sep = "")
    #   }
    # }

    # Adds progress message
    withProgress(message = "Downloading Datasets...", value = 0, {
      n <- length(selected_studies())
      curated_only_value <- curated_only()
      dLLocal_value <- local_download()
      # if (!is.null(multithread_value()) && multithread_value()) {
      # View(multithread_value())
      if (multithread_value() && n >= 4) {
        print("Parallel download")

        # clusters from snow created, they must then load the curatedTBData library to avoid errors
        cl <- makeCluster(4)
        clusterEvalQ(cl, library(curatedTBData))
        # parApply from snow used here. CL created before is a paramater
        selected_studies_info <- parLapply(cl, selected_studies(), function(study_id) {
          vals$MAEList <- c(vals$MAEList, curatedTBData(study_id, dry.run = FALSE, curated.only = curated_only_value))
        })

        stopCluster(cl)
        # multithread_value(TRUE)
        # print(multithread_value())
      } else {
        print("Non-Parallel Download")
        # unparallelized version
        selected_studies_info <- lapply(selected_studies(), function(study_id) {
          setProgress(message = paste("Downloading...", study_id))
          result <- curatedTBData(study_id, dry.run = FALSE, curated.only = curated_only_value)
          # storage for all of the mae's
          vals$MAEList <- c(vals$MAEList, result)

          # stores all mae's into a SINGLE se
          result_se <- toSE(result)
          # View(result_se)
          if (!is.null(vals$SEList)) {
            temp <- mergeSEs(list(se1 = vals$SEList, se2 = result_se))
            vals$SEList <- temp
          } else {
            vals$SEList <- result_se
          }

          # View(vals$MAEList)
          incProgress(1 / n)
          return(result)
        })
      }

      if (dLLocal_value) {
        print("Downloaded/ing")
        extdataDir <- system.file("extdata", package = "curatedTBExplorer")
        localMAEListPath <- file.path(extdataDir, "localMAEList.rds")
        saveRDS(vals$MAEList, file = localMAEListPath)
        print("Downloaded")
      }
      View(vals$localMAEList)

      # Completes Progress Message
      incProgress(n / n, message = "Finished Downloading")
    })

    # Obsolete test code: 
    # the$downloaded_datasets <<- selected_studies_info
    # combineExperiments(vals$MAEList)
    # View(the$downloaded_datasets)
    # View(vals$SEList)
    # View(vals$MAEList)
  } else {
    # Should be code here to add default download to the
  }
})


# render selected studies tables on the Summarize tab
# not currently working - Andrew
# believe we need to change the selected_studies to work with the selected_studies() as I did for download
output$selected_studies_table <- renderDT({
  if (!is.null(input$selected_studies)) {
    data.frame(
      Study = input$selected_studies,
      Value = rnorm(length(input$selected_studies))
    )
  }
})


# added to ensure that every study that is clicked is added
# deselection currently kind of works, except when you deselect the previously selected study
# it stays in the selected_studies(), leading to unexpected behavior
observeEvent(input$selected_study, {
  current_selection <- isolate(input$selected_study)
  current_studies <- selected_studies()
  # uses the setdiff function to set the selected studies correctly
  if (current_selection %in% current_studies) {
    selected_studies(setdiff(current_studies, current_selection))
  } else {
    selected_studies(c(current_studies, current_selection))
  }
  # allows you to see the selected studies
  # View(selected_studies())
})
