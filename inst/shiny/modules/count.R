#' @rdname shiny_helper_functions
#' @export countUiInput
countUiInput <- function(){
  list(
    go = actionButton("count_go", label="", icon = icon("play", lib = "glyphicon")),
    radioButtons("count_object", "class", choices = list("corpus", "partition"), selected = "corpus", inline = TRUE),
    cqp = radioButtons("count_cqp", "CQP", choices = list("yes", "no"), selected = "no", inline = TRUE),
    breakdown = radioButtons("count_breakdown", "breakdown", choices = list("yes", "no"), selected = "no", inline = TRUE),
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
      "count_p_attribute", "p_attribute",
      choices = p_attributes(corpus()[1, "corpus"]),
      selected = "word"
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
  
  observeEvent(
    input$count_corpus,
    updateSelectInput(session, inputId = "count_p_attribute", choices = p_attributes(input$count_corpus))
  )
  
  observeEvent(
    input$count_partition,
    {
      if (input$count_partition != ""){
        updateSelectInput(
          session,
          inputId = "count_p_attribute",
          choices = p_attributes(values[["partitions"]][[input$count_partition]])
        )
      }
    }
  )
  
  

  output$count_table <- DT::renderDataTable({
    input$count_go
    isolate({
      if (input$count_go > 0){
        
        if (!input$count_corpus %in% names(values$corpora))
          values$corpora[[input$count_corpus]] <- corpus(input$count_corpus)

        obj <- switch(
          input$count_object,
          corpus = values$corpora[[input$count_corpus]],
          partition = values$partition[[input$count_partition]]
        )
        
        if (input$count_query == ""){
          retval <- as.data.frame(count(obj, p_attribute = input$count_p_attribute)@stat)
        } else {
          use_cqp <- if (input$count_cqp == "yes") TRUE else FALSE
          retval <- count(
            .Object = obj,
            query = rectifySpecialChars(input$count_query), # utility functions in file utils.R in module dir
            p_attribute = input$count_p_attribute,
            cqp = use_cqp,
            breakdown = if (input$count_breakdown == "yes") TRUE else FALSE
            )
        }
        id_cols <- grep("_id", colnames(retval))
        if (length(id_cols) > 0L) for (colname in id_cols) retval[[colname]] <- NULL
        return(retval)

      } else {
        return(data.frame(word = character(), count = integer()))
      }
    })
  })
}

