shinyThemeToUse <- shinytheme("cerulean") # alternatives: flatly, cerulean

shinyUI(
  
  navbarPage(
    
    theme = shinyThemeToUse,
    title = "polmineR",
    id = "polmineR",
    
    tabPanel(
      "partition",
      sidebarLayout(
        sidebarPanel = sidebarPanel(polmineR:::.partitionUiInput()),
        mainPanel = mainPanel(polmineR:::.partitionUiOutput())
      )),
    
    tabPanel(
      "kwic",
      sidebarLayout(
        sidebarPanel = sidebarPanel(polmineR:::.kwicUiInput()),
        mainPanel = mainPanel(polmineR:::.kwicUiOutput())
      )
    ),
    
    tabPanel(
      "context",
      sidebarLayout(
        sidebarPanel = sidebarPanel(polmineR:::.contextUiInput()),
        mainPanel = mainPanel(polmineR:::.contextUiOutput())
      )
    ),
    
    tabPanel(
      "dispersion",
      sidebarLayout(
        sidebarPanel = sidebarPanel(polmineR:::.dispersionUiInput()),
        mainPanel = mainPanel(
          tabsetPanel(
            tabPanel("Table", DT::dataTableOutput('dispersion_table')),
            tabPanel("Plot", plotOutput('dispersion_plot'))
          )
        )
      )
    )
    
   )
)