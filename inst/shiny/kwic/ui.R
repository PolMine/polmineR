library(shiny)
library(polmineR)
library(shinythemes)

session <- get('session', '.GlobalEnv')
partitionObjects <- polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition')

shinyUI(fluidPage(
  theme=shinytheme("cerulean")
  
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
      "partitionObject", "partition:",
      choices=polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition')
      ),
    textInput("node", "node/query:", value=session@defaultKwicNode),
    selectInput(
      "meta", "s-attribute/metadata:",
      choices=sAttributes(corpus(get(partitionObjects[1], ".GlobalEnv"))),
      multiple=TRUE
    ),
    selectInput(
      "pAttribute", "p-attribute:",
      choices=pAttributes(corpus(get(partitionObjects[1], ".GlobalEnv")))
      ),
    numericInput("leftContext", "left context:", value=session@left),
    numericInput("rightContext", "right context:", value=session@right),
#    textInput("collocate", "collocate:", value=session@defaultKwicCooccurrence),
    radioButtons("read", "read:", choices=c("TRUE", "FALSE"), selected="FALSE", inline=T),
    br()
    ),
  
  mainPanel(
#    h3(textOutput("query")),
    # p(textOutput("frequency")),
    DT::dataTableOutput('table'),
    uiOutput("fulltext")
    )
))
