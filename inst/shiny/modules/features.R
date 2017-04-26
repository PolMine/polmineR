#' @rdname shiny_helper_functions
#' @export featuresUiInput
featuresUiInput <- function(){
  list(
    go = actionButton("features_go", label="", icon = icon("play", lib = "glyphicon")),
    actionButton("features_mail", "", icon = icon("envelope", lib = "glyphicon")),
    br(),
    br(),
    
    x_partition = selectInput(
      "features_partition_x", "partition of interest",
      choices = character(),
      selected = character()
      ),
    
    object_y = radioButtons("features_object_y", "class of reference object", choices = list("partition", "corpus"), selected = "partition", inline = TRUE),
    y_corpus = conditionalPanel(
      condition = "input.features_object_y == 'corpus'",
      selectInput("features_corpus_y", "reference corpus", choices = corpus()[["corpus"]], selected = corpus()[["corpus"]][1])
    ),
    y_partition = conditionalPanel(
      condition = "input.features_object_y == 'partition'",
      selectInput("features_partition_y", "reference partition", choices = character())
    ),
    included = radioButtons("features_included", "included", choices = list("TRUE", "FALSE"), selected = "FALSE", inline = TRUE),
    pAttribute = selectInput(
      "features_pAttribute", "pAttribute",
      choices = pAttributes(corpus()[["corpus"]][1])
    )
    
  )
}


#' @rdname shiny_helper_functions
#' @export featuresUiOutput
featuresUiOutput <- function(){
  DT::dataTableOutput('features_table')
}


#' @rdname shiny_helper_functions
#' @export featuresServer
featuresServer <- function(input, output, session){
  
  retval <- data.frame(a = ""[0], b = ""[0], c = ""[0])
  
  observeEvent(
    input$features_go,
    {
      isolate({})
     
       x = values$partitions[[input$features_partition_x]]
      
      if (!identical(x@pAttribute, input$features_pAttribute)){
         x <- enrich(x, pAttribute = input$features_pAttribute)
      }

      y <- switch(
        input$features_object_y,
        partition = values$partitions[[input$features_partition_y]],
        corpus = input$features_corpus_y
      )
       
      if(input$features_object_y == "partition"){
        if (!identical(y@pAttribute, input$features_pAttribute)){
          y <- enrich(y, pAttribute = input$features_pAttribute)
        }
      }
      
      message("... starting feature extraction")
      featuresObject <- compare(x = x, y = y, included = as.logical(input$features_included))
      featuresObject <- round(featuresObject, 2)
      retval <- as.data.frame(featuresObject@stat)
      values[["features"]] <- featuresObject
      output$features_table <- DT::renderDataTable(
        DT::datatable(retval, selection = "single", rownames = FALSE)
      )
    }
  )
  
  observeEvent(
    input$features_table_rows_selected,
    {
      if (length(input$features_table_rows_selected) > 0){
        print("doing updates")
        updateSelectInput(session, "kwic_object", selected = "partition")
        updateSelectInput(
          session, "kwic_partition",
          choices = names(values$partitions),
          selected = input$features_partition_x
          )
        featuresObject <- values[["features"]]
        token <- featuresObject@stat[["word"]][input$features_table_rows_selected]
        updateTextInput(session, "kwic_query", value = token[1])
        updateSelectInput(session, "kwic_pAttribute", selected = input$features_pAttribute)
        updateNavbarPage(session, "polmineR", selected = "kwic")
        Time <- as.character(Sys.time())
        updateSelectInput(session, "kwic_read", choices = Time, selected = Time)
      }
    })
  

}

