shinyThemeToUse <- shinytheme("cerulean") # alternatives: flatly, cerulean

shinyUI(
  
  navbarPage(
    
    theme = shinyThemeToUse,
    title = "polmineR",
    id = "polmineR",
    
    tabPanel(
  "corpus",
      sidebarLayout(
        sidebarPanel = sidebarPanel(corpusUiInput()),
        mainPanel = mainPanel(
          tabsetPanel(
            tabPanel("info", uiOutput('corpus_info')),
            tabPanel("summary", DT::dataTableOutput('info_table'))
          )
        )
      )
    ),
    
    
    tabPanel(
      "partition",
      sidebarLayout(
        sidebarPanel = sidebarPanel(partitionUiInput()),
        mainPanel = mainPanel(partitionUiOutput())
      )),
    
    tabPanel(
      "kwic",
      sidebarLayout(
        sidebarPanel = sidebarPanel(kwicUiInput()),
        mainPanel = mainPanel(kwicUiOutput())
      )
    ),
    
    tabPanel(
      "cooccurrences",
      sidebarLayout(
        sidebarPanel = sidebarPanel(cooccurrencesUiInput()),
        mainPanel = mainPanel(cooccurrencesUiOutput())
      )
    ),
    
    tabPanel(
      "keywords",
      sidebarLayout(
        sidebarPanel = sidebarPanel(featuresUiInput()),
        mainPanel = mainPanel(featuresUiOutput())
      )
    ),
    
    tabPanel(
      "count",
      sidebarLayout(
        sidebarPanel = sidebarPanel(countUiInput()),
        mainPanel = mainPanel(countUiOutput())
      )
    ),
    
    
    tabPanel(
      "read",
      fluidPage(
        fluidRow(
          column(2),
          column(8, readUiOutput()),
          column(2)
        )
      )
    ),
    
    tabPanel(
      "dispersion",
      sidebarLayout(
        sidebarPanel = sidebarPanel(dispersionUiInput()),
        mainPanel = mainPanel(
          tabsetPanel(
            tabPanel("Table", DT::dataTableOutput('dispersion_table')),
            tabPanel("Plot", plotOutput('dispersion_plot'))
          )
        )
      )
    ),
    
    tabPanel(
      "settings",
      sidebarLayout(
        sidebarPanel = sidebarPanel(settingsUiInput()),
        mainPanel = mainPanel(settingsUiOutput())
      )
    )
  )
)