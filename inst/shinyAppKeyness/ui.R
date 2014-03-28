library(shiny)
library(driller)

drillingControls <- getFromNamespace('drillingControls', 'driller')
partitionObjects <- driller:::.getClassObjects('.GlobalEnv', 'partition')

shinyUI(pageWithSidebar(
  headerPanel("Comparing corpora: Keyness"),
  sidebarPanel(
    selectInput("coi", "Corpus of interest:", choices=names(partitionObjects), selected=names(partitionObjects)[1]),
    selectInput("ref", "Partition:", choices=names(partitionObjects), selected=names(partitionObjects)[2]),
    selectInput("pAttribute", "P-Attribute:", choices=c("word", "pos", "lemma"), selected=drillingControls$pAttribute),
    selectInput("included", "Is COI part of RC:", choices=c("TRUE", "FALSE"), selected=FALSE),
    numericInput("minSignificance", "Minimum Significance", value=drillingControls$minSignificance),
    numericInput("minFrequency", "Minimum frequency:", value=drillingControls$minFrequency),
    textInput("posFilter", "POS-based filter:", value=""),
    br(),
    actionButton("goButton", "Go!")
    ),
  
  mainPanel(
    dataTableOutput('table')
    )
))
