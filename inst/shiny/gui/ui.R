library(shiny)
library(polmineR)
library(shinyBS)
library(shinythemes)

partitionNames <- c(
  polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition'),
  polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'pressPartition'),
  polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'plprPartition')
)
shinyThemeToUse <- shinytheme("cerulean") # alternatives: flatly, cerulean

shinyUI(
  
  navbarPage(
    
    theme = shinyThemeToUse,
    
    title = "polmineR",
    
    id = "polmineR",
    
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
              grep("date", sAttributes(corpus()[1,1]), value=T),
              paste('"', sample(sAttributes(corpus()[1,1], grep("date", sAttributes(corpus()[1,1]), value=T)), 1), '"', sep=""),
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
          textInput("kwic_query", "query", value="Suche"),
          textInput("kwic_neighbor", "neighbor", value=""),
          selectInput(
            "kwic_meta", "sAttribute",
            choices=sAttributes(get(partitionNames[1], ".GlobalEnv")@corpus),
            multiple=TRUE
          ),
          selectInput(
            "kwic_pAttribute", "pAttribute",
            choices=pAttributes(get(partitionNames[1], ".GlobalEnv")@corpus)
          ),
          numericInput("kwic_left", "left", value=getOption("polmineR.left")),
          numericInput("kwic_right", "right", value=getOption("polmineR.right")),
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
          textInput("context_query", "query", value="Suche"),
          selectInput("context_pAttribute", "pAttribute:", choices=c("word", "pos", "lemma"), selected=getOption("polmineR.pAttribute"), multiple=TRUE),
          numericInput("context_left", "left", value=getOption("polmineR.left")),
          numericInput("context_right", "right", value=getOption("polmineR.right")),
          br()
        ),

        mainPanel(
          DT::dataTableOutput('context_table'),
          textOutput("context2kwic")
          )
      )
    )
    ,

    tabPanel(
      "dispersion",
      sidebarLayout(
        sidebarPanel(
          actionButton("dispersion_go", "Go!"),
          br(), br(),
          selectInput("dispersion_partition", "partition", choices=partitionNames, selected=partitionNames[1]),
          textInput("dispersion_query", "query", value="Suche"),
          selectInput(
            "dispersion_pAttribute", "pAttribute",
            choices=pAttributes(get(partitionNames[1], ".GlobalEnv")@corpus)
          ),
          # radioButtons("dispersion_dims", "number of sAttributes", choices=c("1", "2"), selected="1", inline=TRUE),
          selectInput(
            "dispersion_sAttribute_1", "sAttribute",
            choices=sAttributes(get(partitionNames[1])@corpus), multiple=FALSE
            ),
          radioButtons("dispersion_ts", "time series", choices=c("yes", "no"), selected="no", inline=TRUE),
          conditionalPanel(
            condition = "input.dispersion_ts == 'yes'",
            selectInput("dispersion_ts_aggregation", "aggregation", choices=c("none", "month", "quarter", "year"), multiple = FALSE
            )
          )
          
          # , conditionalPanel(
          #   condition = "input.dispersion_dims == '2'",
          #   selectInput(
          #     "dispersion_sAttribute_2", "sAttribute (2)",
          #     choices=sAttributes(get(partitionNames[1])@corpus), multiple = FALSE
          #     )
          # )
        
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Table",
              DT::dataTableOutput('dispersion_table')
              ),
            tabPanel(
              "Plot",
              plotOutput('dispersion_plot')
            )
          )
          
        )
      )
    )

   )
)