library(shiny)
library(driller)

partitionObjects <- driller:::.getClassObjects('.GlobalEnv', 'partition')
drillingControls <- getFromNamespace('drillingControls', 'driller')

shinyUI(pageWithSidebar(
  
  headerPanel("Explore Volatility"),
  
  sidebarPanel(
    # selectInput("foo", "Foo", choices=""),
    selectInput("partitionA", "Partition 1:", names(partitionObjects), selected=names(partitionObjects)[1]),
    textInput("node.a", "Node:", value=""),
    br(),
    actionButton("aButton", "Go!"),
    selectInput("partitionB", "Partition 2:", choices=names(partitionObjects), selected=names(partitionObjects)[1]),
    textInput("node.b", "Node:", value=""),
    br(),
    actionButton("bButton", "Go!"),
    h3('Further settings'),
    selectInput("pattribute", "P-Attribute:", choices=c("word", "pos", "lemma")),
    numericInput("context", "Left/right context:", value=5),
    numericInput("min.frequency", "Minimum frequency:", value=5),
    selectInput("significance", "Minimum Significance", choices=c(0.0, 3.84)),
    textInput("pos.filter", "POS-based filter:", value=paste(drillingControls$posFilter, collapse=' ')),
    selectInput("pearson", "Calculate Pearson's Rho:", choices=c("Yes", "No")),
    textInput("tokenFilter", "Filter for tokens:", value="FOO"),
    sliderInput("max.rank", "Maximum rank:", min=10, max=250, step=10, value=50),
    sliderInput("rotation", "Text rotation:", min=0, max=180, step=1, value=45),
    sliderInput('min.frequency', "Minimum frequency", min=0, max=50, value=0, step=1),
    sliderInput('fontSize', 'Expansion factor in scatterplot', min=0.2, max=2, step=0.1, value=0.7),
    br(),
    actionButton("fullButton", "Go!")
    ),
  
  mainPanel(
    verbatimTextOutput('info.a'),
    verbatimTextOutput('info.b'),
    verbatimTextOutput('rho'),
    plotOutput('scatterplot'),
    dataTableOutput('tab')
    )
))
