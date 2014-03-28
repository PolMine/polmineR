library(shiny)
library(driller)

drillingControls <- getFromNamespace('drillingControls', 'driller')
partitionObjects <- driller:::.getClassObjects('.GlobalEnv', 'partition')

shinyUI(pageWithSidebar(
  
  headerPanel("Explore distribution"),
  
  sidebarPanel(
    selectInput("partitionObject", "Partition:", choices=names(partitionObjects)),
    textInput("queries", "Queries:", value="Suche"),
    selectInput("pAttribute", "Select p-attribute:", choices=c("word", "pos", "lemma"), selected=drillingControls$pAttribute),
    selectInput("dim", "S-Attribute:", choices=c("")),
    selectInput("barplot", "Data for barplot:", choices=c("abs", "rel"), selected="rel"),
    actionButton("goButton", "Go!")
    ),
  
  mainPanel(
    dataTableOutput("abs"),
    dataTableOutput("rel"),
    plotOutput('plot')
    )
))
