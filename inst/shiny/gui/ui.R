library(shiny)
library(polmineR)
library(shinyBS)
library(shinythemes)

controls <- get('session', '.GlobalEnv')
partitionNames <- c(
  polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition'),
  polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'pressPartition'),
  polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'plprPartition')
)
shinyThemeToUse <- shinytheme("cerulean") # alternative: flatly

shinyUI(
  
  navbarPage(
    
    theme = shinyThemeToUse,
    
    "polmineR",
    
    tabPanel(
      "partition",
      sidebarLayout(
        sidebarPanel(
          actionButton("partition_go", "Go!"), br(),br(),
          textInput(inputId = "partition_name", label = "name", value = "FOO"),
          bsTooltip(
            id = "partition_name", title = "This is an input", 
                    placement = "top", trigger = "hover"
            ),
          selectInput("partition_corpus", "corpus", choices = corpus(), selected = corpus()[1]),
          textInput(
            inputId = "partition_def",
            label = "def",
            value = paste(
              grep("date", sAttributes(corpus()[1]), value=T),
              paste('"', sample(sAttributes(corpus()[1], grep("date", sAttributes(corpus()[1]), value=T)), 1), '"', sep=""),
              sep="="
            )),
          selectInput(inputId="partition_pAttribute", label="pAttribute", multiple = TRUE, choices=list(none = "", word = "word", lemma = "lemma")),
          radioButtons("partition_regex", "regex", choices = list("TRUE", "FALSE"), inline = TRUE),
          radioButtons("partition_xml", "xml", choices = list("flat", "nested"), inline = TRUE)
          ),
        mainPanel(
            dataTableOutput('partition_table')
          )

        )
      ),
    
    tabPanel(
      "kwic",
      sidebarLayout(
        sidebarPanel(
          actionButton("kwic_go", "Go!"), br(),br(),
          selectInput("kwic_partition", "partition", choices=partitionNames),
          textInput("kwic_query", "query", value=controls@defaultKwicNode),
          selectInput(
            "kwic_meta", "sAttribute",
            choices=sAttributes(get(partitionNames[1], ".GlobalEnv")@corpus),
            multiple=TRUE
          ),
          selectInput(
            "kwic_pAttribute", "pAttribute",
            choices=pAttributes(get(partitionNames[1], ".GlobalEnv")@corpus)
          ),
          numericInput("kwic_left", "left", value=controls@left),
          numericInput("kwic_right", "right", value=controls@right),
          radioButtons("kwic_read", "read", choices=c("TRUE", "FALSE"), selected="FALSE", inline=T),
          br()
        ),

        mainPanel(DT::dataTableOutput('kwic_table'))
      )
    ),
    
    tabPanel(
      "context",
      sidebarLayout(
        sidebarPanel(
          actionButton("context_go", "Go!"),
          br(), br(),
          selectInput("context_partition", "partition", partitionNames[1]),
          textInput("context_node", "query", value="Suche"),
          selectInput("context_pAttribute", "pAttribute:", choices=c("word", "pos", "lemma"), selected=controls@pAttribute, multiple=TRUE),
          numericInput("context_left", "left", value=controls@left),
          numericInput("context_right", "right", value=controls@right),
          br()
        ),

        mainPanel(
          dataTableOutput('context_table')
        )
      )
    )

   )
)