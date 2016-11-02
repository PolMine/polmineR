####################
##                ##
##   partition    ##
##                ##
####################


#' Building blocks for shiny apps and widgets.
#' 
#' 
#' @param input input
#' @param output output
#' @param session session
#' @rdname shiny_helper_functions
#' @export partitionUiInput
#' @import shiny
partitionUiInput <- function(){
  list(
    go = actionButton("partition_go", label="", icon=icon("play", lib="glyphicon")),
    br(),
    br(),
    corpus = selectInput("partition_corpus", "corpus", choices = corpus(), selected = corpus()[1]),
    name = textInput(inputId = "partition_name", label = "name", value = "UNDEFINED"),
    sAttributesA = selectInput(
      inputId = "partition_sAttributes", label = "sAttributes", multiple = TRUE,
      choices = sAttributes(corpus()[1, "corpus"])
    ),
    sAttributesB = uiOutput("partition_sAttributes"),
    pAttribute = selectInput(inputId = "partition_pAttribute", label = "pAttribute", multiple = TRUE, choices = list(none = "", word = "word", lemma = "lemma")),
    regex = radioButtons("partition_regex", "regex", choices = list("TRUE", "FALSE"), inline = TRUE),
    xml = radioButtons("partition_xml", "xml", choices = list("flat", "nested"), inline = TRUE)
  )
}


#' @rdname shiny_helper_functions
#' @export partitionUiOutput
partitionUiOutput <- function(){
  dataTableOutput('partition_table')
}


#' @rdname shiny_helper_functions
#' @export partitionServer
partitionServer <- function(input, output, session){
  observeEvent(
    input$partition_go,
    {
      selectInputToUpdate <- c("kwic_partition", "context_partition", "dispersion_partition")
      if (input$partition_go > 0){
        defList <- lapply(
          setNames(input$partition_sAttributes, input$partition_sAttributes),
          function(x) gsub('\u201E', '"', input[[x]]) 
        )
        assign(
          input$partition_name,
          partition(
            as.character(input$partition_corpus),
            def = defList,
            name = input$partition_name,
            pAttribute = input$partition_pAttribute,
            regex = input$partition_regex,
            xml = input$partition_xml,
            mc = FALSE,
            verbose = TRUE
          ),
          envir=.GlobalEnv
        )
        partitionDf <- partition()
        if (nrow(partitionDF) == 0){
          partitionDF <- data.frame(
            object = ""[0], name = ""[0], corpus = ""[0], size = integer()
          )
        } 
        rownames(partitionDf) <- NULL
        output$partition_table <- DT::renderDataTable(partitionDf)
        
        for (toUpdate in selectInputToUpdate) {
          updateSelectInput(session, toUpdate, choices=partitionDf$object, selected=NULL)  
        }
      }
    }
  )
  
  observeEvent(
    input$partition_corpus,
    {
      updateSelectInput(
        session, inputId = "partition_sAttributes",
        choices = sAttributes(input$partition_corpus)
      )
    }
  )
  
  output$partition_sAttributes <- renderUI({
    tagList(lapply(
      input$partition_sAttributes,
      function(x) textInput(x, x)
    ))
  })
  
  retval <- observeEvent(
    input$partition_done,
    {
      newPartition <- partition(
        as.character(input$partition_corpus),
        def = lapply(
          setNames(input$partition_sAttributes, input$partition_sAttributes),
          function(x) gsub('\u201E', '"', input[[x]]) 
        ),
        name = input$partition_name,
        pAttribute = input$partition_pAttribute,
        regex = input$partition_regex,
        xml = input$partition_xml,
        mc = FALSE,
        verbose = TRUE
      )
      stopApp(returnValue = newPartition)
      
    }
    )
  
  output$partition_table <- renderDataTable(partition())
  
}


##################
##              ##
##    kwic      ##
##              ##
##################


#' @rdname shiny_helper_functions
#' @export kwicUiInput
kwicUiInput <- function(drop = NULL){
  divs = list(
    go = actionButton("kwic_go", "Go!"),
    br1 = br(),
    br2 = br(),
    object = radioButtons("kwic_object", "class", choices = list("corpus", "partition"), selected = "corpus", inline = TRUE),
    corpus = conditionalPanel(
      condition = "input.kwic_object == 'corpus'",
      selectInput("kwic_corpus", "corpus", choices = corpus()[["corpus"]], selected = corpus()[["corpus"]][1])
    ),
    partition = conditionalPanel(
      condition = "input.kwic_object == 'partition'",
      selectInput("kwic_partition", "partition", choices = partitionNames)
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
    left = sliderInput("kwic_left", "left", min = 1, max = 25, value = getOption("polmineR.left")),
    right = sliderInput("kwic_right", "right", min = 1, max = 25, value = getOption("polmineR.right")),
    read = conditionalPanel(
      condition = "input.kwic_go == -1",
      selectInput("kwic_read", "read", choices = Sys.time())
    ),
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
#' @export kwicServer
kwicServer <- function(input, output, session, ...){
  
  observe({
    x <- input$kwic_partition
    if (x != ""){
      new_sAttr <- sAttributes(get(x, ".GlobalEnv")@corpus)
      new_pAttr <- pAttributes(get(x, ".GlobalEnv")@corpus)
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
    input$kwic_read

    isolate({
      
      if ((input$kwic_go > 0 || input$kwic_read != startingTime) && input$kwic_query != ""){

        if (input$kwic_object == "corpus"){
          object <- input$kwic_corpus
        } else {
          object <- get(input$kwic_partition, '.GlobalEnv')
        }
        
        withProgress(
          message="please wait", value = 0, max = 5, detail="preparing data",
          {
            kwicObject <<- polmineR::kwic(
              .Object = object,
              query = input$kwic_query,
              pAttribute = ifelse(is.null(input$kwic_pAttribute), "word", input$kwic_pAttribute),
              left = input$kwic_left,
              right = input$kwic_right,
              meta = input$kwic_meta,
              verbose = "shiny",
              neighbor = input$kwic_neighbor,
              cpos = TRUE # required for reading
            )
          }
        ) # end withProgress
        
        if (is.null(kwicObject)){
          tab <- data.frame(left = "", node = "", right = "")
        } else {
          tab <- kwicObject@table
        }
        
        if (length(input$kwic_meta) == 0 && nrow(tab) > 0){
          retval <- data.frame(no = c(1:nrow(tab)), tab)
        } else if (length(input$kwic_meta) > 0){
          metaRow <- unlist(lapply(
            c(1:nrow(tab)),
            function(i){
              paste(unlist(lapply(tab[i,c(1:length(input$kwic_meta))], as.character)), collapse=" | ")
            }
          ))
          retval <- data.frame(meta = metaRow, tab[,(length(input$kwic_meta)+1):ncol(tab)])
        }
        
      } else {
        retval <- data.frame(left = "", node = "", right = "")
      }
      
    })
    
    # format DataTable
    retval <- DT::datatable(retval, selection = "single", rownames = FALSE) %>% 
      DT::formatStyle("node", color = "#4863A0", textAlign = "center") %>%
      DT::formatStyle("left", textAlign = "right")
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
        fulltext <- html(kwicObject, input$kwic_table_rows_selected, type = "plpr", verbose = TRUE)
        fulltext <- htmltools::HTML(gsub("<head>.*?</head>", "", as.character(fulltext)))
        fulltext <- htmltools::HTML(gsub('<blockquote>', '<blockquote style="font-size:14px">', as.character(fulltext)))
        output$read_fulltext <- renderUI(fulltext)
        updateNavbarPage(session, "polmineR", selected = "read")
        # browse(fulltext)
      }
    })
  
}


##################
##              ##
##   context    ##
##              ##
##################

#' @rdname shiny_helper_functions
#' @export contextUiInput
contextUiInput <- function(){
  list(
    actionButton("context_go", "Go!"),
    br(), br(),
    radioButtons("context_object", "class", choices = list("corpus", "partition"), selected = "corpus", inline = TRUE),
    conditionalPanel(
      condition = "input.context_object == 'corpus'",
      selectInput("context_corpus", "corpus", corpus()[["corpus"]])
    ),
    conditionalPanel(
      condition = "input.context_object == 'partition'",
      selectInput("context_partition", "partition", choices = partitionNames)
    ),
    textInput("context_query", "query", value = ""),
    selectInput("context_pAttribute", "pAttribute:", choices=c("word", "pos", "lemma"), selected = getOption("polmineR.pAttribute"), multiple=TRUE),
    sliderInput("context_left", "left", min = 1, max = 25, value = getOption("polmineR.left")),
    sliderInput("context_right", "right", min = 1, max = 25, value = getOption("polmineR.right")),
    br()
  )
}


#' @rdname shiny_helper_functions
#' @export contextUiOutput
contextUiOutput <- function(){
  list(
    DT::dataTableOutput('context_table'),
    textOutput("context2kwic")
  )
}


#' @rdname shiny_helper_functions
#' @export contextServer
contextServer <- function(input, output, session){
  output$context_table <- DT::renderDataTable({
    input$context_go
    isolate({
      if (input$context_go > 0 && input$context_query != ""){
        
        if (input$context_object == "corpus"){
          if (!input$context_corpus %in% ls(envir = get(".corpora", .GlobalEnv))){
            withProgress(
              message = "preparing Corpus ...", value = 1, max = 1, detail = "counting",
              {assign(
                input$context_corpus,
                Corpus$new(input$context_corpus, pAttribute = input$context_pAttribute),
                envir = get(".corpora", .GlobalEnv)
              )}
              
            )
          }
          object <- get(input$context_corpus, envir = get(".corpora", .GlobalEnv))
        } else {
          object <- get(input$context_partition, '.GlobalEnv')
        }
        
        withProgress(
          message = "please wait ...", value = 0, max = 6, detail = "getting started",
          {
            ctext <<- context(
              .Object = object,
              query = input$context_query,
              pAttribute = input$context_pAttribute,
              left = input$context_left[1], right = input$context_right[1],
              verbose="shiny"
            )
          })
        if (!is.null(ctext)){
          return(DT::datatable(round(ctext, 2)@stat, selection="single", rownames=FALSE))
        } else {
          return(DT::datatable(data.frame(a = c(), b = c(), d = c())))
        }
        }
    })
    if (!exists("ctext")){
      return(DT::datatable(data.frame(a = c(), b = c(), d = c())))
    }
  })
  
  observeEvent(
    input$context_table_rows_selected,
    {
      if (length(input$context_table_rows_selected) > 0){
        updateTextInput(
          session, "kwic_neighbor",
          value = ctext@stat[[input$context_pAttribute[1]]][input$context_table_rows_selected]
        )
        if (input$kwic_object == "partition"){
          updateSelectInput(session, "kwic_object", selected = "partition")
          updateSelectInput(session, "kwic_partition", selected = input$context_partition)
        } else if (input$kwic_object == "corpus"){
          updateSelectInput(session, "kwic_object", selected = "corpus")
          updateSelectInput(session, "kwic_corpus", selected = input$context_corpus)
        }
        updateTextInput(session, "kwic_query", value = input$context_query)
        updateSelectInput(session, "kwic_left", selected = input$context_left)
        updateSelectInput(session, "kwic_right", selected = input$context_right)
        updateSelectInput(session, "kwic_pAttribute", selected = input$context_pAttribute)
        updateNavbarPage(session, "polmineR", selected = "kwic")
        Time <- as.character(Sys.time())
        updateSelectInput(session, "kwic_read", choices = Time, selected = Time)
      }
    })
}


######################
##                  ##
##    dispersion    ##
##                  ##
######################


#' @rdname shiny_helper_functions
#' @export dispersionUiInput
dispersionUiInput <- function(){
  list(
    actionButton("dispersion_go", "Go!"),
    br(), br(),
    radioButtons("dispersion_object", "class", choices = list("corpus", "partition"), selected = "corpus", inline = TRUE),
    conditionalPanel(
      condition = "input.dispersion_object == 'corpus'",
      selectInput("dispersion_corpus", "corpus", corpus()[["corpus"]])
    ),
    conditionalPanel(
      condition = "input.dispersion_object == 'partition'",
      selectInput("dispersion_partition", "partition", choices = partitionNames, selected = partitionNames[1])
    ),
    textInput("dispersion_query", "query", value="Suche"),
    selectInput(
      "dispersion_pAttribute", "pAttribute",
      choices = pAttributes(corpus()[1, "corpus"])
    ),
    # radioButtons("dispersion_dims", "number of sAttributes", choices=c("1", "2"), selected="1", inline=TRUE),
    selectInput(
      "dispersion_sAttribute_1", "sAttribute",
      choices = sAttributes(corpus()[1, "corpus"]), multiple=FALSE
    ),
    radioButtons("dispersion_ts", "time series", choices=c("yes", "no"), selected="no", inline=TRUE),
    conditionalPanel(
      condition = "input.dispersion_ts == 'yes'",
      selectInput("dispersion_ts_aggregation", "aggregation", choices=c("none", "month", "quarter", "year"), multiple = FALSE
      )
    )
  )
}


#' @rdname shiny_helper_functions
#' @export dispersionUiOutput
dispersionUiOutput <- function(){
}


#' @rdname shiny_helper_functions
#' @export dispersionServer
dispersionServer <- function(input, output, session){
  observe({
    x <- input$dispersion_partition
    if (x != ""){
      new_sAttr <- sAttributes(get(x, ".GlobalEnv")@corpus)
      updateSelectInput(
        session, "dispersion_pAttribute",
        choices=pAttributes(get(x, ".GlobalEnv")@corpus), selected=NULL
      )
      updateSelectInput(session, "dispersion_sAttribute_1", choices=new_sAttr, selected=NULL)
    }
  })
  
  observeEvent(
    input$dispersion_go,
    {
      #        if (input$dispersion_dims == "1"){
      sAttrs <- input$dispersion_sAttribute_1
      #          print(sAttrs)
      #        } else if (input$dispersion_dims == "2"){
      #          sAttrs <- c(input$dispersion_sAttribute_1, input$dispersion_sAttribute_2)
      #          print(sAttrs)
      #        }
      tab <- as.data.frame(dispersion(
        get(input$dispersion_partition),
        query=input$dispersion_query,
        sAttribute=sAttrs,
        pAttribute=input$dispersion_pAttribute
      ))
      tab[["freq"]] <- round(tab[["freq"]], 7)
      if (input$dispersion_ts == "yes"){
        if (requireNamespace("zoo", quietly=TRUE)){
          if (input$dispersion_ts_aggregation != "none"){
            dates4zoo <- tab[[input$dispersion_sAttribute_1]]
            tab[[input$dispersion_sAttribute_1]] <- NULL
            zooObject <- zoo::zoo(as.matrix(tab), order.by=as.Date(dates4zoo))
            zooObjectAggr <- switch(
              input$dispersion_ts_aggregation,
              month = zoo::aggregate.zoo(zooObject, zoo::as.Date(zoo::as.yearmon(zoo::index(zooObject)))),
              quarter = zoo::aggregate.zoo(zooObject, zoo::as.Date(zoo::as.yearqtr(zoo::index(zooObject)))),
              year = zoo::aggregate.zoo(zooObject, as.Date(paste(gsub("^(\\d{4}).*?$", "\\1", zoo::index(zooObject)), "-01-01", sep="")))
            )
            tab <- data.frame(date=zoo::index(zooObjectAggr), zooObjectAggr)
            colnames(tab)[1] <- input$dispersion_sAttribute_1
            rownames(tab) <- NULL
          }
          output$dispersion_plot <- renderPlot(zoo::plot.zoo(zooObjectAggr, main=""))
        } else {
          message("package 'zoo' required, but not available")
        }
      } else {
        # output$dispersion_plot <- renderPlot(as.data.frame(tab))
      }
      output$dispersion_table <- DT::renderDataTable(as.data.frame(tab))
      
    }
  )
}


#' @rdname shiny_helper_functions
#' @export readUiInput
readUiInput <- function(){}


#' @rdname shiny_helper_functions
#' @export readUiOutput
readUiOutput <- function() uiOutput("read_fulltext")


#' @rdname shiny_helper_functions
#' @export readServer
readServer <- function(input, output, session){
  
  # newFulltext <- reactive(fulltext)
  
  observeEvent(
    fulltext,
    {
        print("there is something to read!")
    }
  )
}