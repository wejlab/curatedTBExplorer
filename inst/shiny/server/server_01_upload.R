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
  # Counts clicks on continue
  continueClicks = 0,
  # Holds downloaded MAELists in session
  sessionMAEList = list(),
  # Holds the combined SummarizedExperiment.
  SEList = NULL,
  # Holds the colData from the studies, used in batch qc
  batchList = NULL
)

# Variables to hold local downloaded and default studies
locallyDownloadedStudies <- list()
tempdefaultStudy <- list()

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
    locallyDownloadedStudies <- readRDS(localMAEListDir)
    vals$localMAEList <- locallyDownloadedStudies
    cat("Added local download to localMAEList reactive value\n")

    # Adds locally downloaded studies to the selectizeInput
    updateSelectizeInput(session, "selectedActiveMAEList", choices = names(locallyDownloadedStudies))

  },
  error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
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

    tempdefaultStudy <- readRDS(defaultStudyDir)


    vals$defaultStudy <- tempdefaultStudy


    updateSelectizeInput(session, "selectedActiveMAEList", choices = union(names(tempdefaultStudy), names(locallyDownloadedStudies)))

    cat("Added GSE31348 study to defaultStudy reactive value\n")
  },
  error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    cat("Might be due to not installing via devtools::install_github(\"wejlab/curatedTBExplorer\")\n")
    showNotification(paste("Error:", conditionMessage(e)), type = "error")
  }
)

# Sets up starting sessionMAEList with local and default studies
if(length(intersect(names(tempdefaultStudy), names(locallyDownloadedStudies))) > 0) {
  vals$sessionMAEList <- c(tempdefaultStudy, locallyDownloadedStudies[!(names(locallyDownloadedStudies) %in% names(tempdefaultStudy))])
} else {
  vals$sessionMAEList <- c(tempdefaultStudy, locallyDownloadedStudies)
}

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

  ###########################################################################
  vals$selected_studies = NULL

  return(selected_columns)
})

# Observes the checkbox for the multithread or not
observeEvent(input$dLMultiThread, {
  vals$multithread_value <- input$dLMultiThread
})

# Observes the checkbox for curated or not
observeEvent(input$dLCurated, {
  vals$curated_only <- input$dLCurated
})

# Observes the checkbox for local download or not
observeEvent(input$dLLocal, {
  vals$local_download <- input$dLLocal
})

# If clear local download button pressed, replaces data in .rds file with empty list
observeEvent(input$clearLocalDownload, {
  vals$localMAEList <- list()
  extdataDir <- system.file("extdata", package = "curatedTBExplorer")
  localMAEListPath <- file.path(extdataDir, "localMAEList.rds")
  saveRDS(list(), file = localMAEListPath)
  showNotification("Cleared Local Download", type = "message")
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
              scrollY = "40vh", # Set the height to 50% of the viewport height
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
  ) %>%
    formatStyle(
      columns = names(selected_study_data),
      fontSize = '90%',
      lineHeight = '0.95'
    )
})

# If continue button pressed, downloads study data for selected studies accordingly
observeEvent(input$downloadStudiesBtn, {
  vals$continue_clicked <- TRUE

  # Executes only if there are studies selected
  if (!is.null(vals$selected_studies)) {
    cat("Selected Studies: ", names(vals$selected_studies), "\n")

    # Holds studies that need to be downloaded
    studies_to_download <- vals$selected_studies

    # Holds studies that are already locally available
    local_studies <- names(vals$localMAEList)
    current_studies <- names(vals$sessionMAEList)

    # Narrows studies_to_download to studies not in local or current yet
    studies_to_download <- vals$selected_studies[!(vals$selected_studies %in% local_studies)]
    studies_to_download <- studies_to_download[!(studies_to_download %in% current_studies)]

    # Adds all unique studies from localMAEList to sessionMAEList
    vals$sessionMAEList <- c(vals$sessionMAEList, vals$localMAEList[!(names(vals$localMAEList) %in% names(vals$sessionMAEList))])

    # If there are studies left to download
    if (length(studies_to_download) > 0) {

      # Adds progress message
      withProgress(message = "Downloading Datasets...", value = 0, {

        n <- length(studies_to_download)
        curated_only_value <- vals$curated_only
        dLLocal_value <- vals$local_download

        # Does Multi-thread download if 4 or more studies chosen. Single threaded otherwise.
        if (vals$multithread_value && n >= 4) {
          cat("Multi-thread download starting...\n")

          # Clusters from snow created, loaded the curatedTBData since clusters need new libraries
          cl <- makeCluster(4)
          clusterEvalQ(cl, library(curatedTBData))

          # 4 clusters download and insert study data into the MAEList reactive value with parLapply
          selected_studies_info <- parLapply(cl, studies_to_download, function(study_id) {
            tempsessionMAEList <- list()
            tempsessionMAEList <- c(tempsessionMAEList, curatedTBData(study_id, dry.run = FALSE, curated.only = curated_only_value))
            return(tempsessionMAEList)
          })

          # Ends the clusters when done
          stopCluster(cl)

          # Adds the newly downloaded to the sessionMAEList
          vals$sessionMAEList <- c(vals$sessionMAEList, unlist(selected_studies_info, recursive = FALSE))

          cat("Multi-thread download finished\n")
        } else {
          cat("Single-thread download starting...\n")

          # Downloads and inserts study data into the MAEList reactive value with lapply
          selected_studies_info <- lapply(studies_to_download, function(study_id) {
            # Stores downloaded study data
            study_data <- curatedTBData(study_id, dry.run = FALSE, curated.only = curated_only_value)

            # Adds downloaded study data to MAEList
            vals$sessionMAEList <- c(vals$sessionMAEList, study_data)

            # Updates the progress bar
            incProgress(1 / n)

            return(study_data)
          })
        }

        if (dLLocal_value) {

          # Updates progress bar
          cat("Local download starting...\n")
          incProgress(1/2, message = "Starting Local Download")

          # Compare vals$sessionMAEList to vals$localMAEList and add any missing studies to localMAEList
          studies_to_add_locally <- vals$sessionMAEList[!names(vals$sessionMAEList) %in% names(vals$localMAEList)]
          vals$localMAEList <- c(vals$localMAEList, studies_to_add_locally)

          # Adding studies from studies_to_download to localMAEList.rds file
          extdataDir <- system.file("extdata", package = "curatedTBExplorer")
          localMAEListPath <- file.path(extdataDir, "localMAEList.rds")
          saveRDS(vals$localMAEList, file = localMAEListPath)
          cat("Local download finished\n")
        }

        # Completes Progress Message
        incProgress(n / n, message = "Finished Downloading")
      })
    } else {
      cat("All selected studies are already available locally. Skipping download step.\n")
      showNotification("Selected studies already downloaded", type = "message")
    }

    #
    updateSelectizeInput(session, "selectedActiveMAEList", choices = names(vals$sessionMAEList))

    # Shows notification after finishing
    showNotification("Finished Downloading Studies", type = "message")
  } else {
    cat("Please select a study first\n")
    showNotification("Please select a study first", type = "warning")
  }
})

# Sets studies to use for rest of package
observeEvent(input$confirmStudiesBtn, {
  withProgress(message = "Confirming Studies...", value = 0, {

    # Confirms that something is selected in the selectize
    if(length(input$selectedActiveMAEList) > 0) {

      # Compare vals$sessionMAEList to vals$localMAEList and add any missing studies to localMAEList
      vals$MAEList <- vals$sessionMAEList[input$selectedActiveMAEList]

      # Resets the SEList so we don't get compounding errors
      vals$SEList <- NULL

      # Error handling for if making assay doesn't work
      tryCatch({

        # Converts MAEList to SEList differently depending on how it's done
        if (length(vals$MAEList) > 1) {
          incProgress(1 / 2, message = "Converting Studies")
          vals$SEList <- combine_objects(vals$MAEList, experiment_name = "assay_curated", update_genes = FALSE)
          vals$SEList <- mkAssay(vals$SEList, input_name = "assay1", log = TRUE)
        } else {
          incProgress(1 / 2, message = "Converting Studies")
          vals$SEList <- toSE(vals$MAEList)
          vals$SEList <- mkAssay(vals$SEList, input_name = "assay_curated", log = TRUE)
        }
        incProgress(2 / 2, message = "Studies Confirmed")

        #Grabs the columns that dont have NA values -> necessary for batch correction
        #We also need to exclude columns where one value only comes from one study, and another only comes from the other study.
        # vals$batchList <- lapply(vals$MAEList, function(colData) ))
        #im thinking we can grab the values from the MAEList instead of SEList?

        df <- as.data.frame(colData(vals$SEList)@listData)

        tempA <- colData(vals$MAEList[[1]])@listData
        tempB <- colData(vals$MAEList[[2]])@listData
        tempA <- as.data.frame(tempA)
        tempB <- as.data.frame(tempB)
        tempNaCols <- sapply(tempA, function(col) any(is.na(col)))
        tempNbCols <- sapply(tempB, function(col) any(is.na(col)))
        tempAUnique <- sapply(tempA, function(col) length(unique(na.omit(col))) < 2)
        tempBUnique <- sapply(tempB, function(col) length(unique(na.omit(col))) < 2)
        View(tempAUnique)
        tempColExclude <- tempNaCols | tempNbCols | tempAUnique | tempBUnique
        tempFiltered <- df[ ,!tempColExclude]
        View(tempFiltered)
        updateSelectizeInput(session, "selectedCovars", choices = colnames(tempFiltered), selected = "TBStatus", server = TRUE)



        View(vals$batchList)
        # df <- as.data.frame(colData(vals$SEList)@listData)
        # naCols <- sapply(df, function(col) any(is.na(col)))
        # uniqueValueCols <- sapply(df, function(col) length(unique(col)) < 2)
        # # Combine the conditions to filter out columns
        # colsToExclude <- naCols | uniqueValueCols
        # dfFiltered <- df[, !colsToExclude]
        # View(dfFiltered)
        # colNamesFiltered <- colnames(dfFiltered)
        # View(colNamesFiltered)
        # #we may want to not allow TB status to even be an option, but i've included it here as a default
        # updateSelectizeInput(session, "selectedCovars", choices = colNamesFiltered, selected = "TBStatus", server = TRUE)



        # Sets values for filter tab
        vals$colData <- colData(vals$SEList)
        vals$covars <- colnames(colData(vals$SEList))
        vals$datassays <- names(assays(vals$SEList))
        vals$backupSE <- vals$SEList

        # Sets up the dataTable in filter page:
        my_data(as.data.frame(colData(vals$SEList)))

      }, error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
        showNotification(paste("Error:", conditionMessage(e)), type = "error")
      })
      showNotification("Studies Confirmed", type = "message")
      # View(vals$SEList)
    } else {
      showNotification("Please select at study first", type = "warning")
    }
  })
})

# Handles the user selections for Batch Correction
observeEvent(input$confirmCovarsBtn, {
  selCov <- input$selectedCovars
  if (length(selCov) > 0) {
    my_formula <- paste("~", paste(selCov, collapse = " + "))
  } else {
    my_formula <- "~ TBStatus" # Default formula if nothing selected -> may be unnecessary?
  }

  print(paste("Formula:", my_formula))

  # Create the model matrix
  mod <- model.matrix(as.formula(my_formula), colData(vals$SEList))

  # Perform batch correction using ComBat
  assay(vals$SEList, "corrected_assay") <- ComBat(assay(vals$SEList, "assay1"),
                                               batch = colData(vals$SEList)$Study,
                                               mod = mod)
})

# Handles addition of studies to selected_studies list when studies are selected
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

# Code for testing
# output$test <- renderText({
#   paste(
#     "sessionMAEList: ", paste(names(vals$sessionMAEList), collapse = ", "), "\n",
#     "selectedActiveMAEList: ", paste(names(vals$selectedActiveMAEList), collapse = ", "), "\n",
#     "MAEList: ", paste(names(vals$MAEList), collapse = ", "), "\n",
#     # "SEList: ", paste(names(vals$SEList), collapse = ", "), "\n",
#     "mlList: ", paste(names(vals$mlList), collapse = ", "), "\n"
#   )
# })
