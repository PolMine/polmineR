library(shiny)
library(driller)

drillingControls <- getFromNamespace('drillingControls', 'driller')
partitionObjects <- driller:::.getClassObjects('.GlobalEnv', 'partition')

shinyUI(pageWithSidebar(
  
  headerPanel("Explore context"),
  
  sidebarPanel(
    selectInput("partitionObject", "Partition:", choices=names(partitionObjects)),
    textInput("node", "Node:", value="Suche"),
    selectInput("pAttribute", "Select p-attribute:", choices=c("word", "pos", "lemma"), selected=drillingControls$pAttribute),
    numericInput("leftContext", "Left context:", value=drillingControls$leftContext),
    numericInput("rightContext", "Right context:", value=drillingControls$rightContext),
    numericInput("minSignificance", "Minimum significance:", value=drillingControls$minSignificance),
    textInput("posFilter", "POS-based filter:", value=paste(drillingControls$posFilter, collapse=' ')),
    actionButton("goButton", "Go!")
    ),
  
  mainPanel(
    h3(textOutput("query")),
    p(textOutput("frequency")),
    dataTableOutput('table')
    )
))
