library(shiny)
library(polmineR)

controls <- get('session', '.GlobalEnv')
partitionNames <- polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition')

shinyUI(
  
  navbarPage(
    "polmineR",
    
    tabPanel(
      "partition",
      sidebarLayout(
        sidebarPanel(
          actionButton("partition_go", "Go!"), br(),br(),
          textInput(inputId = "partition_name", label = "Partition name:", value = "FOO"),
          selectInput("partition_corpus", "Corpus:", choices = cqi_list_corpora(), selected = "PLPRTXT"),
          textInput(inputId = "partition_def", label = "sAttributes:", value = 'text_year="2012"'),
          selectInput(inputId="partition_pAttribute", label="pAttribute:", multiple = TRUE, choices=list(none = "", word = "word", lemma = "lemma")),
          radioButtons("partition_regex", "Use regular expressions:", choices = list("TRUE", "FALSE"), inline = TRUE),
          radioButtons("partition_xml", "XML type:", choices = list("flat", "nested"), inline = TRUE)
          ),
        mainPanel(

          )

        )
      ),
    
    tabPanel(
      "kwic",
      sidebarLayout(
        sidebarPanel(
          actionButton("kwic_go", "Go!"), br(),br(),
          selectInput("kwic_partition", "partition:", choices=partitionNames),
          textInput("kwic_query", "node/query:", value=controls@defaultKwicNode),
          selectInput(
            "kwic_meta", "s-attribute/metadata:",
            choices=sAttributes(get(partitionNames[1], ".GlobalEnv")@corpus),
            multiple=TRUE
          ),
          selectInput(
            "kwic_pAttribute", "p-attribute:",
            choices=pAttributes(get(partitionNames[1], ".GlobalEnv")@corpus)
          ),
          numericInput("kwic_left", "left context:", value=controls@left),
          numericInput("kwic_right", "right context:", value=controls@right),
          radioButtons("kwic_read", "read:", choices=c("TRUE", "FALSE"), selected="FALSE", inline=T),
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
          selectInput("context_partition", "Partition:", partitionNames[1]),
          textInput("context_node", "Node:", value="Suche"),
          selectInput("context_pAttribute", "Select p-attribute:", choices=c("word", "pos", "lemma"), selected=controls@pAttribute, multiple=TRUE),
          numericInput("context_left", "Left context:", value=controls@left),
          numericInput("context_right", "Right context:", value=controls@right),
          br()
        ),

        mainPanel(
          dataTableOutput('context_table')
        )
      )
    )

   )
)