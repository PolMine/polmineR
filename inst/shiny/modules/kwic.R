kwicUiInput <- function(drop = NULL){
  
  divs = list(
    go = actionButton("kwic_go", "", icon = icon("play", lib = "glyphicon")),
    code = actionButton("kwic_code", label = "", icon = icon("code", lib = "font-awesome")),
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
      "kwic_s_attributes", "s_attribute",
      choices = s_attributes(corpus()[["corpus"]][1]),
      multiple = TRUE
    ),
    region = conditionalPanel(
      condition = "input.kwic_show_regions == 'show'",
      radioButtons(
        "kwic_region", "region", 
        choices = as.list(c(
          "none",
          getOption("polmineR.segments")[getOption("polmineR.segments") %in% s_attributes(corpus()[["corpus"]][1])]
        )),
        selected = "none",
        inline = TRUE
      )
    ),
    p_attribute = selectInput(
      "kwic_p_attribute", "p_attribute",
      choices = p_attributes(corpus()[["corpus"]][1]),
      selected = "word"
    ),
    left = sliderInput("kwic_left", "left", min = 0, max = 25, value = getOption("polmineR.left")),
    right = sliderInput("kwic_right", "right", min = 0, max = 25, value = getOption("polmineR.right")),
    boundary = selectInput(
      "kwic_boundary", "boundary", 
      choices = c("", s_attributes(corpus()[["corpus"]][1])),
      multiple = FALSE
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


kwicServer <- function(input, output, session, ...){
  
  observe({
    x <- input$kwic_partition
    if (x != "" && length(values$partitions) > 0 && input$kwic_partition %in% names(values$partitions)){
      new_sAttr <- s_attributes(values$partitions[[input$kwic_partition]]@corpus)
      new_pAttr <- p_attributes(values$partitions[[input$kwic_partition]]@corpus)
      updateSelectInput(session, "kwic_p_attribute", choices = new_pAttr, selected = NULL)
      updateSelectInput(session, "kwic_s_attributes", choices = new_sAttr, selected = NULL)
      updateSelectInput(session, "kwic_boundary", choices = c("", new_sAttr), selected = NULL)
      
      updateRadioButtons(
        session, "kwic_region",
        choices = as.list(c(
          "none",
          getOption("polmineR.segments")[getOption("polmineR.segments") %in% new_sAttr]
        )),
        selected = "none", inline = TRUE
      )
      if (any(getOption("polmineR.segments") %in% new_sAttr)){
        shinyjs::runjs('Shiny.onInputChange("kwic_show_regions", "show");')
      } else {
        shinyjs::runjs('Shiny.onInputChange("kwic_show_regions", "hide");')
      }

    }
  })
  
  observe({
    x <- input$kwic_corpus
    new_sAttr <- s_attributes(input$kwic_corpus)
    new_pAttr <- p_attributes(input$kwic_corpus)
    updateSelectInput(session, "kwic_p_attribute", choices = new_pAttr, selected = NULL)
    updateSelectInput(session, "kwic_s_attributes", choices = new_sAttr, selected = NULL)
    updateSelectInput(session, "kwic_boundary", choices = c("", new_sAttr), selected = NULL)
    
    updateRadioButtons(
      session, "kwic_region",
      choices = as.list(c(
        "none", getOption("polmineR.segments")[getOption("polmineR.segments") %in% new_sAttr]
      )),
      selected = "none", inline = TRUE
    )
    
    if (any(getOption("polmineR.segments") %in% new_sAttr)){
      shinyjs::runjs('Shiny.onInputChange("kwic_show_regions", "show");')
    } else {
      shinyjs::runjs('Shiny.onInputChange("kwic_show_regions", "hide");')
    }
  })
  
  observeEvent(input$kwic_code, {
    format_string <- if (input$kwic_positivelist == ""){
      'kwic(\n  %s,\n  query = "%s",\n  cqp = %s,\n  boundary = %s,\n  region = %s,\n  %sleft = %s,\n  right = %s\n)' 
    } else {
      'kwic(\n  %s,\n  query = "%s",\n  cqp = %s,\n  boundary = %s,\n  region = %s,\n  positivelist = %s,\n  left = %s,\n  right = %s\n)' 
    }

    snippet <- sprintf(
      format_string,
      if (input$kwic_object == "corpus") sprintf('"%s"', input$kwic_corpus)  else input$kwic_partition,
      input$kwic_query,
      if (input$kwic_cqp == "yes") "TRUE" else "FALSE", 
      if (input$kwic_boundary == "") "NULL" else sprintf('"%s"', input$kwic_boundary),
      if (input$kwic_region == "none") "NULL" else sprintf('"%s"', input$kwic_region),
      if (input$kwic_positivelist != "")
        sprintf("c(%s)", paste(sprintf('"%s"', strsplit(x = input$kwic_positivelist, split = "(;\\s*|,\\s*)")[[1]]), collapse = ", ")) else "",
      input$kwic_left,
      input$kwic_right
    )
    snippet_html <- highlight::highlight(
      parse.output = parse(text = snippet),
      renderer = highlight::renderer_html(document = TRUE),
      output = NULL
    )
    showModal(modalDialog(title = "Code", HTML(paste(snippet_html, collapse = ""))))
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
              region = if (input$kwic_region == "none") NULL else input$kwic_region,
              boundary = if (input$kwic_boundary == "") NULL else input$kwic_boundary,
              left = input$kwic_left,
              right = input$kwic_right,
              s_attributes = input$kwic_s_attributes,
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
          retval <- as(values[["kwic"]], "htmlwidget")
        }
        
        
      } else {
        retval <- data.frame(left = character(), node = character(), right = character())
      }
      retval
    })
    
    retval
  })
  
  observeEvent(
    input$kwic_region,
    {
      if (input$kwic_region %in% getOption("polmineR.segments")){
        updateSliderInput(inputId = "kwic_left", value = 0L)
        updateSliderInput(inputId = "kwic_right", value = 0L)
      } else {
        updateSliderInput(
          inputId = "kwic_left", value = getOption("polmineR.left")
        )
        updateSliderInput(
          inputId = "kwic_right", value = getOption("polmineR.right")
        )
      }
    }
    
  )
  
  observeEvent(
    input$kwic_table_rows_selected,
    {
      if (length(input$kwic_table_rows_selected) > 0){
        corpus_properties <- registry_get_properties(corpus = values[["kwic"]]@corpus)
        corpusType <- if ("type" %in% names(corpus_properties)) corpus_properties["type"] else NULL
        if (debug) assign("kwicObject", value = values[["kwic"]], envir = .GlobalEnv)
        
        kwic_metadata <- values[["kwic"]]@metadata
        template_metadata <- get_template(values[["kwic"]])[["metadata"]]
        if (length(kwic_metadata) > 0L || length(template_metadata) > 0L){
          withProgress(
            fulltext <- polmineR::html(
            values[["kwic"]],
            input$kwic_table_rows_selected,
            type = corpusType,
            verbose = TRUE
          ))
          if (debug) message("html generated")
          fulltext <- htmltools::HTML(gsub("<head>.*?</head>", "", as.character(fulltext)))
          fulltext <- htmltools::HTML(gsub('<blockquote>', '<blockquote style="font-size:14px">', as.character(fulltext)))
          output$read_fulltext <- renderUI(fulltext)
          
          updateNavbarPage(session, "polmineR", selected = "read")
        } else {
          showModal(
            modalDialog(
              title = "s-attribute definition missing",
              HTML(
                paste(
                  c(
                    "Generating fulltext output requires the definition of a ",
                    "structural attribute that will define the region of the ",
                    "query match to be displayed. This is possible either via ",
                    "a template or s_attributes included in the kwic table. ",
                    "Condition not met."
                  ), collapse = ""
                )
              )
            )
          )
        }
        
      }
    })
  
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
