tabPanel("Summarize",
             tabsetPanel(
               tabPanel("Filter",
                        sidebarPanel(
                          h4("Selected Studies:"),
                          verbatimTextOutput("selected_studies_text"),
                          
                          selectInput("filter_by", "Filter By",
                                      choices = c("TB Status", "HIV Status", "Diabetes Status")),
                          
                          conditionalPanel(
                            condition = "input.filter_by == 'TB Status'",
                            selectInput("tb_status", "TB Status", choices = c("PTB", "LTBI"))
                          ),
                          conditionalPanel(
                            condition = "input.filter_by == 'HIV Status'",
                            selectInput("hiv_status", "HIV Status", choices = c("Positive", "Negative"))
                          ),
                          conditionalPanel(
                            condition = "input.filter_by == 'Diabetes Status'",
                            selectInput("diabetes_status", "Diabetes Status", choices = c("Positive", "Negative"))
                          ),
                          
                          actionButton("filter_button", "Filter"),
                          actionButton("reset_button", "Reset")
                        ),
                        mainPanel(
                          DTOutput("summary_table"),
                          plotOutput("summary_plots")
                        )
               ),
               
               tabPanel("Categorize",
                        # code for the "Categorize" sub-tab
               )
             )
    )
