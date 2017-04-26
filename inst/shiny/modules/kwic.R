##################
##              ##
##    kwic      ##
##              ##
##################


#' @rdname shiny_helper_functions
#' @importFrom DT formatStyle
#' @export kwicUiInput
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
    query = textInput("kwic_query", "query", value = ""),
    neighbor = textInput("kwic_neighbor", "neighbor", value = ""),
    sAttribute = selectInput(
      "kwic_meta", "sAttribute",
      choices = sAttributes(corpus()[["corpus"]][1]),
      multiple = TRUE
    ),
    pAttribute = selectInput(
      "kwic_pAttribute", "pAttribute",
      choices = pAttributes(corpus()[["corpus"]][1])
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


#' @rdname shiny_helper_functions
#' @importFrom DT formatStyle datatable
#' @export kwicServer
kwicServer <- function(input, output, session, ...){
  
  observe({
    x <- input$kwic_partition
    if (x != "" && length(values$partitions) > 0 && input$kwic_partition %in% names(values$partitions)){
      new_sAttr <- sAttributes(values$partitions[[input$kwic_partition]]@corpus)
      new_pAttr <- pAttributes(values$partitions[[input$kwic_partition]]@corpus)
      updateSelectInput(session, "kwic_pAttribute", choices = new_pAttr, selected = NULL)
      updateSelectInput(session, "kwic_meta", choices = new_sAttr, selected = NULL)
    }
  })
  
  observe({
    x <- input$kwic_corpus
    new_sAttr <- sAttributes(input$kwic_corpus)
    new_pAttr <- pAttributes(input$kwic_corpus)
    updateSelectInput(session, "kwic_pAttribute", choices = new_pAttr, selected = NULL)
    updateSelectInput(session, "kwic_meta", choices = new_sAttr, selected = NULL)
  })
  

  output$kwic_table <- DT::renderDataTable({
    
    input$kwic_go
    values[["kwic_go"]]

    isolate({
      
      if ((input$kwic_go > 0 || values[["kwic_go"]] != values[["startingTime"]]) && input$kwic_query != ""){
        
        if (input$kwic_object == "corpus"){
          object <- input$kwic_corpus
        } else {
          object <- values$partitions[[input$kwic_partition]]
        }
        
        withProgress(
          message = "please wait", value = 0, max = 5, detail = "preparing data",
          {
            values[["kwic"]] <- polmineR::kwic(
              .Object = object,
              query = rectifySpecialChars(input$kwic_query),
              pAttribute = ifelse(is.null(input$kwic_pAttribute), "word", input$kwic_pAttribute),
              left = input$kwic_window,
              right = input$kwic_window,
              meta = input$kwic_meta,
              verbose = "shiny",
              neighbor = input$kwic_neighbor,
              cpos = TRUE # required for reading
            )
          }
        ) # end withProgress
        
        if (is.null(values[["kwic"]])){
          retval <- data.frame(left = character(), node = character(), right = character())
        } else if (nrow(values[["kwic"]]@table) == 0){
          retval <- data.frame(left = character(), node = character(), right = character())
        } else {
          tab <- values[["kwic"]]@table
          tab[["hit_no"]] <- NULL
          if (length(input$kwic_meta) == 0){
            retval <- data.frame(no = c(1:nrow(tab)), tab)
          } else if (length(input$kwic_meta)){
            metaRow <- unlist(lapply(
              c(1:nrow(tab)),
              function(i){
                paste(unlist(lapply(tab[i,c(1:length(input$kwic_meta))], as.character)), collapse=" | ")
              }
            ))
            retval <- data.frame(meta = metaRow, tab[,(length(input$kwic_meta)+1):ncol(tab)])
          }
        }
        
        
      } else {
        retval <- data.frame(left = character(), node = character(), right = character())
      }
      
    })
    
    # format DataTable
    retval <- DT::datatable(retval, selection = "single", rownames = FALSE)
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
        corpusType <- RegistryFile$new(values[["kwic"]]@corpus)$getProperties()[["type"]]
        fulltext <- html(values[["kwic"]],
                         input$kwic_table_rows_selected,
                         type = corpusType,
                         verbose = TRUE
                         )
        fulltext <- htmltools::HTML(gsub("<head>.*?</head>", "", as.character(fulltext)))
        fulltext <- htmltools::HTML(gsub('<blockquote>', '<blockquote style="font-size:14px">', as.character(fulltext)))
        output$read_fulltext <- renderUI(fulltext)
        
        updateNavbarPage(session, "polmineR", selected = "read")
      }
    })
  
  observeEvent(
    input$kwic_mail,
    {
      if (input$kwic_mail > 0) polmineR:::mail(values[["kwic"]])
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
        sidebarPanel = sidebarPanel(kwicUiInput(drop = c("go", "br1", "br2", "pAttribute", "read"))),
        mainPanel = mainPanel(kwicUiOutput())
      )
    ))
    
    returnValue <- runGadget(
      app = shinyApp(ui = kwicGadgetUI, server = kwicServer),
      viewer = browserViewer()
    )
    return(returnValue)
    
})
