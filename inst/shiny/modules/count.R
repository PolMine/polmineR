#' @rdname shiny_helper_functions
#' @export countUiInput
countUiInput <- function(){
  list(
    go = actionButton("count_go", label="", icon = icon("play", lib = "glyphicon")),
    radioButtons("count_object", "class", choices = list("corpus", "partition"), selected = "corpus", inline = TRUE),
    conditionalPanel(
      condition = "input.count_object == 'corpus'",
      selectInput("count_corpus", "corpus", corpus()[["corpus"]])
    ),
    conditionalPanel(
      condition = "input.count_object == 'partition'",
      selectInput("count_partition", "partition", choices = character())
    ),
    textInput("count_query", "query", value = ""),
    selectInput(
      "count_pAttribute", "pAttribute",
      choices = pAttributes(corpus()[1, "corpus"])
    )
  )
}


#' @rdname shiny_helper_functions
#' @export countUiOutput
countUiOutput <- function(){
  DT::dataTableOutput('count_table')
}


#' @rdname shiny_helper_functions
#' @export countServer
countServer <- function(input, output, session){

  output$count_table <- DT::renderDataTable({
    input$count_go
    isolate({
      if (input$count_go > 0){
        
        if (input$count_object == "corpus" && input$count_query == ""){
          if (!input$count_corpus %in% names(values$corpora)){
            print("x")
            withProgress(
              message = "preparing Corpus ...", value = 1, max = 1, detail = "counting",
              {
                values$corpora[[input$count_corpus]] <- Corpus$new(input$count_corpus, pAttribute = input$count_pAttribute)
              }
              
            )
          }
        } else {
          
        }
        
        .Object <- switch(
          input$count_object,
          corpus = values$corpora[[input$count_corpus]],
          partition = values$partition[[input$count_partition]]
        )
        
        if (input$count_query == ""){
          return(polmineR:::.get_slot(.Object, "stat"))
        } else {
          retval <- count(.Object, query = input$count_query, pAttribute = input$count_pAttribute)
          return( retval )
        }

      } else {
        return(data.frame(word = ""[0], count = integer()))
      }
    })
  })
}

