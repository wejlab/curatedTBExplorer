# Summarize tab page
tabPanel(
  "Summarize",
  
  icon = icon("filter", class = "fa-solid fa-filter", lib = "font-awesome"),
  tabsetPanel(
    tabPanel(
      "Filter",
      sidebarPanel(
        
        # Title and display of selected studies
        h4("Selected Studies:"),
        verbatimTextOutput("selected_studies_text"),
        
        # Dropdown box for selecting filter
        selectInput("filter_by", "Filter By",
                    choices = c(colnames(colData(combined_studies)))
        ),
        #Those values are :
        # print(c(colnames(colData(combined_studies))))
        # [1] "Age"                     "Gender"                  "Ethnicity"              
        # [4] "TBStatus"                "GeographicalRegion"      "BcgVaccinated"          
        # [7] "BirthRegion"             "TST"                     "Tissue"                 
        # [10] "HIVStatus"               "MeasurementTime"         "PatientID"              
        # [13] "PneumoniaStatus"         "exposure_latent"         "index_case_disease_site"
        # [16] "smear_of_index_case"     "modal_x_ray_grade"       "SputumSmearStatus"      
        # [19] "sputum_culture"          "bal_smear"               "bal_culture"            
        # [22] "isolate_sensitivity"     "DiabetesStatus"          "Treatment"              
        # [25] "Study"                   "QFT_GIT"                 "HealthControl"          
        # [28] "StillStatus"             "AdultSLE_Status"         "PediatricSLE_Status"    
        # [31] "StaphStatus"             "StrepStatus"            

        
        # This will outputa dynamic field with a conditional selectInput and filter button created in server file
        uiOutput("dynamic_filter"),
        br(),
        
        # Reset filter button
        actionButton("filter_reset_btn", "Reset"),
        br()
      ),
      mainPanel(
        fluidRow(
          column(
            12,
            DTOutput("filter_summary_table")
          ),
        ),
        width = 8
      )
    )
  ),
  tabPanel(
    "Categorize",
    # code for the "Categorize" sub-tab
  )
)
