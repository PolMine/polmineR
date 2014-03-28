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
    selectInput("rows", "Rows:", choices=c("text_year")),
    selectInput("cols", "Colums:", choices=c("text_party")),
    selectInput("what", "Table to show:", choices=c("rel", "abs", "partitions"), selecte="rel"),
    sliderInput("rex", "Bubble expansion:", min=0, max=2, value=1, round=FALSE, step=0.1),
    submitButton("Update")
    ),
  
  mainPanel(
    h3(textOutput("what")),
    dataTableOutput("tab"),
    plotOutput('plot')
    )
))
