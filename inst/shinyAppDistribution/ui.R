library(shiny)
library(driller)

drillingControls <- getFromNamespace('drillingControls', 'driller')
partitionObjects <- driller:::.getClassObjects('.GlobalEnv', 'partition')

shinyUI(pageWithSidebar(
  
  headerPanel("Explore distribution"),
  
  sidebarPanel(
    selectInput("partitionObject", "Partition:", choices=names(partitionObjects)),
    textInput("query", "Query:", value="Suche"),
    selectInput("pAttribute", "Select p-attribute:", choices=c("word", "pos", "lemma"), selected=drillingControls$pAttribute),
    selectInput("dim", "S-Attribute:", choices=c("")),
    actionButton("goButton", "Go!")
    ),
  
  mainPanel(
    dataTableOutput("tab"),
    plotOutput('plot')
    )
))
