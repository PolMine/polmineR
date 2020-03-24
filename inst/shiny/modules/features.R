#' @rdname shiny_helper_functions
#' @export featuresUiInput
featuresUiInput <- function(){
  list(
    go = actionButton("features_go", label="", icon = icon("play", lib = "glyphicon")),
    code = actionButton("features_code", label = "", icon = icon("code", lib = "font-awesome")),
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
    p_attribute = selectInput(
      "features_p_attribute", "p_attribute",
      choices = p_attributes(corpus()[["corpus"]][1])
    )
    
  )
}


#' @rdname shiny_helper_functions
#' @export featuresUiOutput
featuresUiOutput <- function(){
  list(
    DT::dataTableOutput('features_table')
  )
}


#' @rdname shiny_helper_functions
#' @export featuresServer
featuresServer <- function(input, output, session){
  
  observeEvent(
    input$features_partition_x,
    {
      if (input$features_partition_x != ""){
        updateSelectInput(
          session,
          inputId = "features_p_attribute",
          choices = p_attributes(values[["partitions"]][[input$features_partition_x]])
        )
      }
    }
  )
  
  observeEvent(input$features_code, {
    snippet <- sprintf(
      'features(\n  x = %s,\n  y = %s,\n  included = %s,\n  p_attribute = "%s"\n)',
      input$features_partition_x,
      if (input$features_object_y == "corpus") sprintf('"%s"', input$features_corpus_y)  else input$features_partition_y,
      input$features_included,
      input$features_p_attribute
    )
    snippet_html <- highlight::highlight(
      parse.output = parse(text = snippet),
      renderer = highlight::renderer_html(document = TRUE),
      output = NULL
    )
    showModal(modalDialog(title = "Code", HTML(paste(snippet_html, collapse = ""))))
  })
  
  
  
  # the sole purpose of the following block is to show empty table
  output$features_table <- DT::renderDataTable({
    input$features_go
    isolate({
      if (input$features_go == 0){
        df <- data.frame(
          word = character(), count_coi = integer(), count_ref = integer(),
          exp_coi = numeric(), chisquare = numeric(), rank_chisquare = integer() 
        )
        return(df)
      }
    })
  })
  
  
  
  retval <- data.frame(a = character(), b = character(), c = character())
  
  observeEvent(
    input$features_go,
    {
      isolate({})
     
       x = values$partitions[[input$features_partition_x]]
      
      if (!identical(x@p_attribute, input$features_p_attribute)){
         x <- enrich(x, p_attribute = input$features_p_attribute)
      }

      y <- switch(
        input$features_object_y,
        partition = values$partitions[[input$features_partition_y]],
        corpus = input$features_corpus_y
      )
       
      if(input$features_object_y == "partition"){
        if (!identical(y@p_attribute, input$features_p_attribute)){
          y <- enrich(y, p_attribute = input$features_p_attribute)
        }
      }
      
      message("... starting feature extraction")
      features_obj <- features(x = x, y = y, included = as.logical(input$features_included))
      output$features_table <- DT::renderDataTable(as(features_obj, "htmlwidget"))
    }
  )
  
  observeEvent(
    input$features_table_rows_selected,
    {
      if (length(input$features_table_rows_selected) > 0){
        updateSelectInput(session, "kwic_object", selected = "partition")
        updateSelectInput(
          session, "kwic_partition",
          choices = names(values$partitions),
          selected = input$features_partition_x
          )
        featuresObject <- values[["features"]]
        token <- featuresObject@stat[["word"]][input$features_table_rows_selected]
        updateTextInput(session, "kwic_query", value = token[1])
        updateSelectInput(session, "kwic_p_attribute", selected = input$features_p_attribute)
        updateNavbarPage(session, "polmineR", selected = "kwic")
        Time <- as.character(Sys.time())
        updateSelectInput(session, "kwic_read", choices = Time, selected = Time)
      }
    })
  

}

