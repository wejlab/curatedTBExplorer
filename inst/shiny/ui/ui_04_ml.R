#machine learning tab panel
tabPanel(
  "Machine Learing",
  fluidRow(
    column(width = 3,
           "General Settings",
           selectInput("outputClass", "outputClass", c("LTBI", "PTB", "Control", "OD"))
           ),
    column(width = 9,
           "Training & Testing")
  ),
  tabsetPanel(
    # Contains general settings for the machine learning

    tabPanel(
      "Elastic Net Regression",
      sidebarPanel(
        "Elastic Net Regression"
      )

    ),
    tabPanel(
      "Neural Networks",
      sidebarPanel(
        "Neural Networks"
      )
    ),

    tabPanel(
      "Random Forests",
      sidebarPanel(
        "Random Forests"

      )
    ),

    tabPanel(
      "Support Vector Machines",
      sidebarPanel(
        "Support Vector Machines"
      )
    )
  )
)

