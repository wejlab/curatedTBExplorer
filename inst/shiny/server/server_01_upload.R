# To Do:
# Finish local download functionality
# Perhaps allow specification of exact core (multithreading) usage
# Add "default" dataset so summary can be viewed without downloading anything
# Fix issue regarding deselecting datasets - works sometimes - solved
# Fix issue where if selections are made, then user changes table options, deselection occurs on ui side

# Reactive values for various tasks
vals <- reactiveValues(
  # Holds list of MAEs that are locally downloaded by user
  localMAEList = list(),

  # Holds active list of MAEs in use
  MAEList = list(),

  # Holds list of MAEs to download
  selectedDownloadList = list(),

  # this is used to update the selected studies accordingly
  selected_studies = NULL,

  # stores if Continue button is clicked
  continue_clicked = FALSE,

  # holds multithreading value
  multithread_value = TRUE,

  # holds curated only value
  curated_only = TRUE,

  # holds local download value
  local_download = FALSE,

  # Holds default study
  defaultStudy = NULL,

  continueClicks = 0
)

# Try catch error handling for local download
tryCatch(
  {
    # Sets pathing and directories for reading and saving .rds files
    localMAEListDir <- system.file("extdata/localMAEList.rds", package = "curatedTBExplorer")
    extdataDir <- system.file("extdata", package = "curatedTBExplorer")
    localMAEListPath <- file.path(extdataDir, "localMAEList.rds")

    # If file localMAEList.rds doesn't exist, makes one
    if (!file.exists(localMAEListDir)) {
      saveRDS(list(), file = localMAEListPath)
    }

    # Reads .rds file into localMAEList reactive value
    vals$localMAEList <- readRDS(localMAEListDir)
    cat("Added local download to localMAEList reactive value\n")
  },
  error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    cat("Might be due to not installing via devtools::install_github(\"wejlab/curatedTBExplorer\")\n")
  }
)

# Try catch error handling for downloading default study
tryCatch(
  {
    # Sets pathing and directories for reading and saving .rds files
    defaultStudyDir <- system.file("extdata/GSE31348.rds", package = "curatedTBExplorer")
    extdataDir <- system.file("extdata", package = "curatedTBExplorer")
    defaultStudyPath <- file.path(extdataDir, "GSE31348.rds")

    # If the default study file doesn't exist, makes one
    if (!file.exists(defaultStudyDir)) {
      saveRDS(list(), file = defaultStudyPath)
    }
    defaultStudy <- readRDS(defaultStudyDir)
    vals$MAEList <- defaultStudy

    # Reads .rds file into defaultStudy reactive value
    vals$defaultStudy <- defaultStudy
    cat("Added GSE31348 study to defualtStudy reactive value\n")
  },
  error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    cat("Might be due to not installing via devtools::install_github(\"wejlab/curatedTBExplorer\")\n")
  }
)


# Grab the selected checkboxes from the ui section, and only output these within the datatable
selected_columns <- reactive({

  # Sets study id and notes column as always visible
  selected_columns <- c("Study", "Notes")

  # Detects checkbox input and adds correspondingly to the selected columns input
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


# Displays study information table
output$study_table <- renderDT({

  # Checks selected columns and creates a new set of data to be rendered in the datatable
  current_columns <- selected_columns()
  selected_study_data <- study_data[, current_columns, drop = FALSE]

  # Datatable setup
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
                "var selected = $(this).hasClass('selected');",
                "var study = data[1];",
                "if (selected) {",
                "Shiny.setInputValue('selected_study', study);",
                "} else {",
                "Shiny.setInputValue('deselected_study', study);",
                "}",
                "});",
                "}"
              )
            ),
            selection = "multiple"
  )
})

# Observes the checkbox for the multithread or not
observeEvent(input$dLMultiThread, {
  vals$multithread_value <- input$dLMultiThread
})

# Observes the checkbox for curated or not
observeEvent(input$dLCurated, {
  vals$curated_only <- input$dLCurated
  # View(curated_only())
})

# Observes the checkbox for local download or not
observeEvent(input$dLLocal, {
  vals$local_download <- input$dLLocal
  # View(local_download())
})

# If clear local download button pressed, replaces data in .rds file with empty list
observeEvent(input$clearLocalDownload, {
  vals$localMAEList <- list()
  extdataDir <- system.file("extdata", package = "curatedTBExplorer")
  localMAEListPath <- file.path(extdataDir, "localMAEList.rds")
  saveRDS(list(), file = localMAEListPath)
  cat("Cleared Local Download\n")
  # View(vals$localMAEList)
  View(vals$MAEList)
})

# If continue button pressed, downloads study data for selected studies accordingly
observeEvent(input$continue, {
  vals$continue_clicked <- TRUE

  # Executes only if there are studies selected
  if (!is.null(vals$selected_studies)) {

    # Updates the continueClick number by 1 if continue is clicked and has selected studies
    vals$continueClicks <- vals$continueClicks + 1

    # Removes default study when downloading for the first time
    if(vals$continueClicks == 1) {
      vals$MAEList <- list()
    }

    cat("Selected Studies: ", names(vals$selected_studies), "\n")

    # Holds studies that need to be downloaded
    studies_to_download <- vals$selected_studies

    # Holds studies that are already locally available
    studies_from_local <- list()

    # Extract study names from localMAEList
    local_studies <- names(vals$localMAEList)
    current_studies <- names(vals$MAEList)
    # View(local_studies)
    # View(vals$selected_studies)

    studies_to_download <- vals$selected_studies[!(vals$selected_studies %in% local_studies)]
    studies_to_download <- studies_to_download[!(studies_to_download %in% current_studies)]
    # View(studies_to_download)

    print(vals$continueClicks)



    # Adds studies from local to MAEList
    # vals$MAEList <- c(vals$MAEList, studies_from_local)

    # Adds studies that are both in selected_studies and localMAEList to MAEList
    for (studyName in as.vector(vals$selected_studies)) {
      if (studyName %in% names(vals$localMAEList)) {
        vals$MAEList[[studyName]] <- vals$localMAEList[[studyName]]
      }
    }



    # If there are studies left to download
    if (length(studies_to_download) > 0) {

      # Adds progress message
      withProgress(message = "Downloading Datasets...", value = 0, {
        n <- length(studies_to_download)
        curated_only_value <- vals$curated_only
        dLLocal_value <- vals$local_download

        if (vals$multithread_value && n >= 4) {
          cat("Multi-thread download starting...\n")

          # Clusters from snow created, loaded the curatedTBData since clusters need new libraries
          cl <- makeCluster(4)
          clusterEvalQ(cl, library(curatedTBData))

          # 4 clusters download and insert study data into the MAEList reactive value with parLapply
          selected_studies_info <- parLapply(cl, studies_to_download, function(study_id) {
            tempMAEList <- list()
            tempMAEList <- c(tempMAEList, curatedTBData(study_id, dry.run = FALSE, curated.only = curated_only_value))
            return(tempMAEList)
          })


          # Ends the clusters when done
          stopCluster(cl)

          vals$MAEList <- unlist(selected_studies_info, recursive = FALSE)

          cat("Multi-thread download finished\n")
        } else {
          cat("Single-thread download starting...\n")

          # Downloads and inserts study data into the MAEList reactive value with lapply
          selected_studies_info <- lapply(studies_to_download, function(study_id) {
            # Stores downloaded study data into result
            study_data <- curatedTBData(study_id, dry.run = FALSE, curated.only = curated_only_value)

            # Adds downloaded study data to MAEList
            vals$MAEList <- c(vals$MAEList, study_data)

            # Converts all downloaded MAEs into a list of SEs
            result_se <- toSE(study_data)

            # Executes block if SEList is null
            if (!is.null(vals$SEList)) {
              temp <- mergeSEs(list(se1 = vals$SEList, se2 = result_se))
              vals$SEList <- temp
            } else {
              vals$SEList <- result_se
            }

            return(study_data)
          })
        }

        if (dLLocal_value) {
          cat("Local download starting...\n")

          # Compare vals$MAEList to vals$localMAEList and add any missing studies to localMAEList
          studies_to_add_locally <- vals$MAEList[!names(vals$MAEList) %in% names(vals$localMAEList)]
          vals$localMAEList <- c(vals$localMAEList, studies_to_add_locally)

          # Adding studies from studies_to_download to localMAEList.rds file
          extdataDir <- system.file("extdata", package = "curatedTBExplorer")
          localMAEListPath <- file.path(extdataDir, "localMAEList.rds")
          saveRDS(vals$localMAEList, file = localMAEListPath)
          cat("Local download finished\n")
        }
        # object_list <- curatedTBData(vals$selected_studies, dry.run = FALSE, curated.only = TRUE)
        # # Combine the studies together in a single SE object
        # combined_studies <- combine_objects(object_list, experiment_name = "assay_curated", update_genes = FALSE)
        # View(combined_studies)
        # View(vals$SEList)
        if (length(vals$MAEList) > 1) {
          vals$SEList <- combine_objects(vals$MAEList, experiment_name = "assay_curated", update_genes = FALSE)
          vals$SEList <- mkAssay(vals$SEList, input_name = "assay1", log = TRUE)
        }
        else {
          vals$SEList <- mkAssay(vals$SEList, input_name = "assay_curated",
                                 log = TRUE)
        }
        vals$colData <- colData(vals$SEList)
        vals$covars <- colnames(colData(vals$SEList))

        vals$datassays <- names(assays(vals$SEList))
        # Completes Progress Message
        incProgress(n / n, message = "Finished Downloading")
      })
    } else {
      cat("All selected studies are already available locally. Skipping download step.\n")
    }
    showNotification("Finished Downloading Studies", type = "message")
  } else {
    cat("Please select a study first\n")
  }
  # View(vals$MAEList)
  # View(vals$localMAEList)
  # View(vals$defaultStudy)
})


# output$selected_studies_table <- renderDT({
#   if (!is.null(vals$selected_studies)) {
#     data.frame(
#       Study = vals$selected_studies,
#       Value = rnorm(length(vals$selected_studies))
#     )
#   }
# })


# added to ensure that every study that is clicked is added
# deselection currently kind of works, except when you deselect the previously selected study
# it stays in the selected_studies(), leading to unexpected behavior
observeEvent(input$selected_study, {
  current_selection <- isolate(input$selected_study)
  current_studies <- vals$selected_studies

  # Check if the current selection is in the list of selected studies
  if (current_selection %in% current_studies) {
    # If it is, remove it from the list
    vals$selected_studies <- current_studies[current_studies != current_selection]
  } else {
    # If it's not, add it to the list
    vals$selected_studies <- c(current_studies, current_selection)
  }
})

# Handles removal of studies from selected_studies list when studies are deselected
observeEvent(input$deselected_study, {
  current_deselection <- isolate(input$deselected_study)
  current_studies <- vals$selected_studies

  # Check if the deselected study is in the list of selected studies
  if (current_deselection %in% current_studies) {
    # If it is, remove it from the list
    vals$selected_studies <- current_studies[current_studies != current_deselection]
  }
})

