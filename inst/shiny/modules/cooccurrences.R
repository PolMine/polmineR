
########################
##                    ##
##   cooccurrences    ##
##                    ##
########################

#' @rdname shiny_helper_functions
#' @export cooccurrencesUiInput
cooccurrencesUiInput <- function(){
  list(
    actionButton("cooccurrences_go", "", icon = icon("play", lib = "glyphicon")),
    code = actionButton("cooccurrences_code", label = "", icon = icon("code", lib = "font-awesome")),
    br(), br(),
    radioButtons("cooccurrences_object", "class", choices = list("corpus", "partition"), selected = "corpus", inline = TRUE),
    conditionalPanel(
      condition = "input.cooccurrences_object == 'corpus'",
      selectInput("cooccurrences_corpus", "corpus", corpus()[["corpus"]])
    ),
    conditionalPanel(
      condition = "input.cooccurrences_object == 'partition'",
      selectInput("cooccurrences_partition", "partition", choices = character())
    ),
    textInput("cooccurrences_query", "query", value = ""),
    cqp = radioButtons("cooccurrences_cqp", "CQP", choices = list("yes", "no"), selected = "no", inline = TRUE),
    selectInput("cooccurrences_p_attribute", "p_attribute:", choices = c("word", "pos", "lemma"), selected = "word", multiple = TRUE),
    sliderInput("cooccurrences_left", "left", min = 1, max = 25, value = getOption("polmineR.left")),
    sliderInput("cooccurrences_right", "right", min = 1, max = 25, value = getOption("polmineR.right")),
    br()
  )
}


#' @rdname shiny_helper_functions
#' @export cooccurrencesUiOutput
cooccurrencesUiOutput <- function(){
  list(
    DT::dataTableOutput('cooccurrences_table'),
    textOutput("cooccurrences2kwic")
  )
}


#' @rdname shiny_helper_functions
#' @export cooccurrencesServer
cooccurrencesServer <- function(input, output, session){
  
  observeEvent(
    input$cooccurrences_corpus,{
      updateSelectInput(
        session,
        inputId = "cooccurrences_p_attribute",
        choices = p_attributes(input$cooccurrences_corpus),
        selected = "word"
      )
    }
    
  )
  
  observeEvent(
    input$cooccurrences_partition,
    {
      if (input$cooccurrences_partition != ""){
        updateSelectInput(
          session,
          inputId = "cooccurrences_p_attribute",
          choices = p_attributes(values[["partitions"]][[input$cooccurrences_partition]]),
          selected = "word"
        )
      }
    }
  )
  
  observeEvent(input$cooccurrences_code, {
    snippet <- sprintf(
      'cooccurrences(\n  %s,\n  query = "%s",\n  cqp = %s,\n  p_attribute = "%s",\n  left = %s,\n  right = %s\n)',
      if (input$cooccurrences_object == "corpus") sprintf('"%s"', input$cooccurrences_corpus)  else input$cooccurrences_partition,
      input$cooccurrences_query,
      if (input$cooccurrences_cqp == "yes") "TRUE" else "FALSE", 
      input$cooccurrences_p_attribute,
      input$cooccurrences_left,
      input$cooccurrences_right
    )
    snippet_html <- highlight::highlight(
      parse.output = parse(text = snippet),
      renderer = highlight::renderer_html(document = TRUE),
      output = NULL
    )
    showModal(modalDialog(title = "Code", HTML(paste(snippet_html, collapse = ""))))
  })
  
  
  output$cooccurrences_table <- DT::renderDataTable({
    input$cooccurrences_go
    isolate({
      if (input$cooccurrences_go > 0 && input$cooccurrences_query != ""){
        
        if (input$cooccurrences_object == "corpus"){
          obj <- corpus(input$cooccurrences_corpus)
        } else {
          obj <- values$partitions[[input$cooccurrences_partition]]
        }
        
        withProgress(
          message = "please wait ...", value = 0, max = 6, detail = "getting started",
          {
            values[["cooccurrences"]] <- cooccurrences(
              .Object = obj,
              query = rectifySpecialChars(input$cooccurrences_query),
              cqp = if (input$cooccurrences_cqp == "yes") TRUE else FALSE,
              p_attribute = input$cooccurrences_p_attribute,
              left = input$cooccurrences_left, right = input$cooccurrences_right[1],
              verbose = "shiny"
            )
          })
        
        if (!is.null(values[["cooccurrences"]])){
          return(as(values[["cooccurrences"]], "htmlwidget"))
        } else {
          y <- data.frame(
            word = character(), count_window = character(), count_partition = character(),
            exp_window = integer(), exp_partition = integer(), ll = integer(),
            rank_ll = integer()
          )
          return(DT::datatable(y))
        }
      } else {
        retval <- data.frame(
          word = character(), count_window = character(), count_partition = character(),
          exp_window = integer(), exp_partition = integer(), ll = integer(),
          rank_ll = integer()
        )
        return(retval)
      }
    })
  })
  
  observeEvent(
    input$cooccurrences_table_rows_selected,
    {
      if (length(input$cooccurrences_table_rows_selected) > 0){
        updateTextInput(session, "kwic_query", value = values[["cooccurrences"]]@query)
        updateTextInput(
          session, "kwic_positivelist",
          value = values[["cooccurrences"]]@stat[[input$cooccurrences_p_attribute[1]]][input$cooccurrences_table_rows_selected]
        )
        if (input$kwic_object == "partition"){
          updateSelectInput(session, "kwic_object", selected = "partition")
          updateSelectInput(session, "kwic_partition", selected = input$cooccurrences_partition)
        } else if (input$kwic_object == "corpus"){
          updateSelectInput(session, "kwic_object", selected = "corpus")
          updateSelectInput(session, "kwic_corpus", selected = input$cooccurrences_corpus)
        }
        updateSelectInput(session, "kwic_cqp", selected = input$cooccurrences_cqp)
        updateTextInput(session, "kwic_query", value = input$cooccurrences_query)
        updateSelectInput(session, "kwic_left", selected = input$cooccurrences_left)
        updateSelectInput(session, "kwic_right", selected = input$cooccurrences_right)
        updateSelectInput(session, "kwic_p_attribute", selected = input$cooccurrences_p_attribute)
        updateNavbarPage(session, "polmineR", selected = "kwic")
        values[["kwic_go"]] <- as.character(Sys.time()) # will initiate kwic preparation & display
      }
    })
  
  observeEvent(
    input$cooccurrences_mail,
    {
      if (input$cooccurrences_mail > 0){
        polmineR:::mail(
          subset(round(values[["cooccurrences"]], 2), rank_ll < 100)
          )
      }
    }
  )
}




#' @rdname polmineR_gui
setMethod("cooccurrences", "missing", function(){
  if (requireNamespace("shiny", quietly = TRUE)){
    shiny::runApp(system.file("shiny", "cooccurrences", package = "polmineR"), launch.browser = TRUE)  
    # shiny::runApp("/Users/blaette/Lab/github/polmineR/inst/shiny/cooccurrences", launch.browser=TRUE)
  } else {
    message("package shiny not available")
  }
})


