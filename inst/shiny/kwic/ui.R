library(shiny)
library(polmineR)

session <- get('session', '.GlobalEnv')
partitionObjects <- polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition')

shinyUI(fluidPage(
  
  tags$head(tags$style(
    c(".table .alignRight {color: black; text-align:right;}
      .table .alignCenter {color: SteelBlue; text-align:center; font-weight:bold;}
      .table .metadata {font-style:italic; text-align:left; background-color: whitesmoke; border-right: 1px solid DarkGray;}
      .table .sorting {color: black; border-bottom: 1px solid DarkGray; border-top: 1px solid DarkGray;}
  
      ")
    )),
  
  headerPanel("kwic"),
  
  sidebarPanel(
#    actionButton("partitionButton", "refresh partitions"),
    actionButton("goButton", "Go!"),
    br(),br(),
    selectInput(
      "partitionObject", "Partition:",
      choices=polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition')
#      , selected="foo"
      # choices=polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition'),
      # selected=polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition')[1]
      ),
    textInput("node", "Node:", value=session@defaultKwicNode),
    selectInput("pAttribute", "Select p-attribute:", choices=c("word", "pos", "lemma"), selected=session@pAttribute),
    numericInput("leftContext", "Left context:", value=session@left),
    numericInput("rightContext", "Right context:", value=session@right),
    textInput("collocate", "collocate:", value=session@defaultKwicCooccurrence),
    textInput("meta", "Metainformation:", value="text_party,text_date"),
    br(),
    actionButton("goButton", "Go!")
    ),
  
  mainPanel(
    h3(textOutput("query")),
    # p(textOutput("frequency")),
    dataTableOutput('table')
    )
))
