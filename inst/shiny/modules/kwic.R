kwicUiInput <- function(drop = NULL){
  
  divs = list(
    go = actionButton("kwic_go", "", icon = icon("play", lib = "glyphicon")),
    mail = actionButton("kwic_mail", "", icon = icon("envelope", lib = "glyphicon")),
    br1 = br(),
    br2 = br(),
    object = radioButtons("kwic_object", "class", choices = list("corpus", "partition"), selected = "corpus", inline = TRUE),
    corpus = conditionalPanel(
      condition = "input.kwic_object == 'corpus'",
      selectInput("kwic_corpus", "corpus", choices = corpus()[["corpus"]], selected = corpus()[["corpus"]][1])
    ),
    partition = conditionalPanel(
      condition = "input.kwic_object == 'partition'",
      selectInput("kwic_partition", "partition", choices = character())
    ),
    query = textInput("kwic_query", label = "query", value = ""),
    cqp = radioButtons("kwic_cqp", "CQP", choices = list("yes", "no"), selected = "no", inline = TRUE),
    positivelist = textInput("kwic_positivelist", label = "positivelist", value = ""),
    s_attribute = selectInput(
      "kwic_meta", "s_attribute",
      choices = s_attributes(corpus()[["corpus"]][1]),
      multiple = TRUE
    ),
    p_attribute = selectInput(
      "kwic_p_attribute", "p_attribute",
      choices = p_attributes(corpus()[["corpus"]][1]),
      selected = "word"
    ),
    window = sliderInput("kwic_window", "window", min = 1, max = 25, value = getOption("polmineR.left")),
    br3 = br()
  )
  if (!is.null(drop)) for (x in drop) divs[[x]] <- NULL
  divs
}

#' @rdname shiny_helper_functions
#' @export kwicUiOutput
kwicUiOutput <- function(){
  DT::dataTableOutput('kwic_table')
}


kwicServer <- function(input, output, session, ...){
  
  observe({
    x <- input$kwic_partition
    if (x != "" && length(values$partitions) > 0 && input$kwic_partition %in% names(values$partitions)){
      new_sAttr <- s_attributes(values$partitions[[input$kwic_partition]]@corpus)
      new_pAttr <- p_attributes(values$partitions[[input$kwic_partition]]@corpus)
      updateSelectInput(session, "kwic_p_attribute", choices = new_pAttr, selected = NULL)
      updateSelectInput(session, "kwic_meta", choices = new_sAttr, selected = NULL)
    }
  })
  
  observe({
    x <- input$kwic_corpus
    new_sAttr <- s_attributes(input$kwic_corpus)
    new_pAttr <- p_attributes(input$kwic_corpus)
    updateSelectInput(session, "kwic_p_attribute", choices = new_pAttr, selected = NULL)
    updateSelectInput(session, "kwic_meta", choices = new_sAttr, selected = NULL)
  })
  

  output$kwic_table <- DT::renderDataTable({
    
    input$kwic_go
    values[["kwic_go"]]

    isolate({
      
      if ((input$kwic_go > 0 || values[["kwic_go"]] != values[["startingTime"]]) && input$kwic_query != ""){
        
        object <- if (input$kwic_object == "corpus")
          input$kwic_corpus
        else
          values$partitions[[input$kwic_partition]]

        withProgress(
          message = "please wait", value = 0, max = 5, detail = "preparing data",
          {
            poslist <- if (nchar(input$kwic_positivelist) >= 1L) input$kwic_positivelist else NULL
            K <- polmineR::kwic(
              .Object = object,
              query = rectifySpecialChars(input$kwic_query),
              cqp = if (input$kwic_cqp == "yes") TRUE else FALSE,
              p_attribute = if (is.null(input$kwic_p_attribute)) "word" else input$kwic_p_attribute,
              left = input$kwic_window,
              right = input$kwic_window,
              meta = input$kwic_meta,
              verbose = "shiny",
              positivelist = poslist,
              cpos = TRUE # required for reading
            )
            if (!is.null(poslist) & !is.null(K)) K <- highlight(K, yellow = poslist)
            values[["kwic"]] <- K
          }
        ) # end withProgress
        
        if (is.null(values[["kwic"]])){
          retval <- data.frame(left = character(), node = character(), right = character())
        } else if (nrow(values[["kwic"]]@stat) == 0L){
          retval <- data.frame(left = character(), node = character(), right = character())
        } else {
          tab <- values[["kwic"]]@stat
          if ("match_id" %in% colnames(tab)) tab[, "match_id" := NULL]
          if (length(input$kwic_meta) == 0L){
            retval <- data.frame(no = 1L:nrow(tab), tab)
          } else if (length(input$kwic_meta)){
            metaRow <- unlist(lapply(
              1L:nrow(tab),
              function(i)paste(unlist(lapply(tab[i,1L:length(input$kwic_meta)], as.character)), collapse = " | ")
            ))
            retval <- data.frame(meta = metaRow, tab[,(length(input$kwic_meta) + 1L):ncol(tab)])
          }
        }
        
        
      } else {
        retval <- data.frame(left = character(), node = character(), right = character())
      }
      
    })
    
    # format DataTable
    retval <- DT::datatable(retval, selection = "single", rownames = FALSE, escape = FALSE)
    retval <- DT::formatStyle(retval, "node", color = "#4863A0", textAlign = "center")
    retval <- DT::formatStyle(retval, "left", textAlign = "right")
    if (length(input$kwic_meta) > 0){
      retval <- DT::formatStyle(
        retval, "meta", fontStyle = "italic", textAlign = "left", borderRight = "1px solid DarkGray")
    }
    retval
  })
  
  observeEvent(
    input$kwic_table_rows_selected,
    {
      if (length(input$kwic_table_rows_selected) > 0){
        corpus_properties <- registry_get_properties(corpus = values[["kwic"]]@corpus)
        corpusType <- if ("type" %in% names(corpus_properties)) corpus_properties["type"] else NULL
        if (debug) assign("kwicObject", value = values[["kwic"]], envir = .GlobalEnv)
        fulltext <- polmineR::html(
          values[["kwic"]],
          input$kwic_table_rows_selected,
          type = corpusType,
          verbose = TRUE
          )
        if (debug) message("html generated")
        fulltext <- htmltools::HTML(gsub("<head>.*?</head>", "", as.character(fulltext)))
        fulltext <- htmltools::HTML(gsub('<blockquote>', '<blockquote style="font-size:14px">', as.character(fulltext)))
        output$read_fulltext <- renderUI(fulltext)
        
        updateNavbarPage(session, "polmineR", selected = "read")
      }
    })
  
  observeEvent(
    input$kwic_mail,
    {
      success <- polmineR::mail(values[["kwic"]])
      if (success) showNotification(
        sprintf("Results have been sent successfully to %s", getOption("polmineR.email"))
      )
    }
  )
}




#' @rdname polmineR_gui
setMethod("kwic", "missing", function(){

    kwicGadgetUI <- shinyUI(fluidPage(
      theme = shinytheme("cerulean"),
      padding = 5,
      gadgetTitleBar(
        "KWIC",
        left = miniTitleBarCancelButton(),
        right = miniTitleBarButton(inputId = "kwic_go", label = "Go", primary = TRUE)
      ),
      div(br()),
      sidebarLayout(
        sidebarPanel = sidebarPanel(kwicUiInput(drop = c("go", "br1", "br2", "p_attribute", "read"))),
        mainPanel = mainPanel(kwicUiOutput())
      )
    ))
    
    returnValue <- runGadget(
      app = shinyApp(ui = kwicGadgetUI, server = kwicServer),
      viewer = browserViewer()
    )
    return(returnValue)
    
})
