
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
    actionButton("cooccurrences_mail", "", icon = icon("envelope", lib = "glyphicon")),
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
    selectInput("cooccurrences_pAttribute", "pAttribute:", choices = c("word", "pos", "lemma"), selected = getOption("polmineR.pAttribute"), multiple = TRUE),
    sliderInput("cooccurrences_window", "window", min = 1, max = 25, value = getOption("polmineR.left")),
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
  output$cooccurrences_table <- DT::renderDataTable({
    input$cooccurrences_go
    isolate({
      if (input$cooccurrences_go > 0 && input$cooccurrences_query != ""){
        
        if (input$cooccurrences_object == "corpus"){
          if (!input$cooccurrences_corpus %in% names(values$corpora)){
            withProgress(
              message = "preparing Corpus ...", value = 1, max = 1, detail = "counting",
              {
                C <- Corpus$new(input$cooccurrences_corpus)
                C$count(pAttribute = input$cooccurrences_pAttribute, id2str = FALSE)
                values$corpora[[input$cooccurrences_corpus]] <- C
              }
              
            )
          }
          object <- values$corpora[[input$cooccurrences_corpus]]
        } else {
          object <- values$partitions[[input$cooccurrences_partition]]
        }
        
        withProgress(
          message = "please wait ...", value = 0, max = 6, detail = "getting started",
          {
            values[["cooccurrences"]] <- cooccurrences(
              .Object = object,
              query = rectifySpecialChars(input$cooccurrences_query),
              pAttribute = input$cooccurrences_pAttribute,
              left = input$cooccurrences_window[1], right = input$cooccurrences_window[1],
              verbose = "shiny"
            )
          })
        
        if (!is.null(values[["cooccurrences"]])){
          return(DT::datatable(round(values[["cooccurrences"]], 2)@stat, selection = "single", rownames = FALSE))
        } else {
          return(DT::datatable(data.frame(a = c(), b = c(), d = c())))
        }
      } else {
        retval <- data.frame(
          word = ""[0], count_window = ""[0], count_partition = ""[0],
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
          session, "kwic_neighbor",
          value = values[["cooccurrences"]]@stat[[input$cooccurrences_pAttribute[1]]][input$cooccurrences_table_rows_selected]
        )
        if (input$kwic_object == "partition"){
          updateSelectInput(session, "kwic_object", selected = "partition")
          updateSelectInput(session, "kwic_partition", selected = input$cooccurrences_partition)
        } else if (input$kwic_object == "corpus"){
          updateSelectInput(session, "kwic_object", selected = "corpus")
          updateSelectInput(session, "kwic_corpus", selected = input$cooccurrences_corpus)
        }
        updateTextInput(session, "kwic_query", value = input$cooccurrences_query)
        updateSelectInput(session, "kwic_window", selected = input$cooccurrences_window)
        updateSelectInput(session, "kwic_pAttribute", selected = input$cooccurrences_pAttribute)
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
  if (requireNamespace("shiny", quietly=TRUE)){
    shiny::runApp(system.file("shiny", "cooccurrences", package = "polmineR"), launch.browser = TRUE)  
    # shiny::runApp("/Users/blaette/Lab/github/polmineR/inst/shiny/cooccurrences", launch.browser=TRUE)
  } else {
    message("package shiny not available")
  }
})


