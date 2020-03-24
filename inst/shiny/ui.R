shinyThemeToUse <- shinytheme("cerulean") # alternative: flatly

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
        sidebarPanel = sidebarPanel(kwicUiInput(drop = "p_attribute")),
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
      "features",
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
      "read",
      fluidPage(
        fluidRow(
          column(2),
          column(8, readUiOutput()),
          column(2)
        )
      )
    )
    
  )
)