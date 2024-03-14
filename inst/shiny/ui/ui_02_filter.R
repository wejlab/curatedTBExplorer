tabPanel("Summarize",
         tabsetPanel(
           tabPanel("Filter",
                    sidebarPanel(
                      h4("Selected Studies:"),
                      verbatimTextOutput("selected_studies_text"),
                      
                      #Filter By field
                      selectInput("filter_by", "Filter By",
                                  choices = c("TB Status", "HIV Status", "Diabetes Status")),
                      
                      #for TB
                      conditionalPanel(
                        condition = "input.filter_by == 'TB Status'",
                        selectInput("tb_status", "TB Status", choices = c("PTB", "LTBI")),
                        
                        
                        actionButton("filter_tb_btn", "Filter", class = "btn-primary")
                        
                      ),
                      conditionalPanel(
                        condition = "input.filter_by == 'HIV Status'",
                        selectInput("hiv_status", "HIV Status", choices = c("Positive", "Negative")),
                        
                        actionButton("filter_hiv_btn", "Filter", class = "btn-primary")
                        
                      ),
                      conditionalPanel(
                        condition = "input.filter_by == 'Diabetes Status'",
                        selectInput("diabetes_status", "Diabetes Status", choices = c("Positive", "Negative")),
                        
                        actionButton("filter_diabetes_btn", "Filter", class = "btn-primary")
                        
                      ),
                      conditionalPanel(
                        condition = "input.filter_by == 'Region'",
                        selectInput("geo_region", "Region", choices = c("Brazil", "China", "Germany", "India", "Indonesia", "Kenya", "Malawi", "Mongolian", "South Africa", "South India", "Taiwan", "The Gambia", "UK", "US", "Venezuela"))
                      ),
                      conditionalPanel(
                        condition = "input.filter_by == 'Tissue'",
                        selectInput("tissue", "Tissue", choices = c("Whole Blood", "PBMCs", "CD", "Monocytes", "Neutrophils"))
                      ),
                      
                      br(),
                      
                      
                      # Reset
                      
                      actionButton("filter_reset_btn", "Reset")
                      ,
                      
                      br()
                    ),
                    mainPanel(
                      fluidRow(
                        column(5,
                               uiOutput("filter_summary_table")
                        ),
                        column(7,
                               plotlyOutput("filter_summary_top_plot", height="350px"),
                               plotlyOutput("filter_summary_bottom_plot", height="350px")
                        )
                      ),
                      width=7
                    )
           )
         ),
         
         tabPanel("Categorize",
                  # code for the "Categorize" sub-tab
         )
)
