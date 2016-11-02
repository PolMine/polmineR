library(shiny)
library(polmineR)

sessionSettings <-  get('session', '.GlobalEnv')
# partitionObjects <- polmineR.shiny:::.getClassObjects('.GlobalEnv', 'partition')

shinyUI(pageWithSidebar(
  
  headerPanel("context"),
  
  sidebarPanel(
    actionButton("partitionButton", "refresh partitions"),
    actionButton("goButton", "Go!"),
    br(), br(),
    selectInput(
      "partitionObject", "Partition:", polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition')
      ),
    textInput("node", "Node:", value="Suche"),
    selectInput("pAttribute", "Select p-attribute:", choices=c("word", "pos", "lemma"), selected=sessionSettings@pAttribute, multiple=TRUE),
    numericInput("left", "Left context:", value=sessionSettings@left),
    numericInput("right", "Right context:", value=sessionSettings@right),
    br(),
    actionButton("goButton", "Go!")
    ),
  
  mainPanel(
    h3(textOutput("query")),
    # p(textOutput("frequency")),
    dataTableOutput('table')
    )
))
