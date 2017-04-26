######################
##                  ##
##    dispersion    ##
##                  ##
######################


#' @rdname shiny_helper_functions
#' @importFrom zoo zoo
#' @export dispersionUiInput
dispersionUiInput <- function(){
  list(
    actionButton("dispersion_go", "", icon = icon("play", lib = "glyphicon")),
    # actionButton("dispersion_mail", "", icon = icon("envelope", lib = "glyphicon")),
    br(), br(),
    radioButtons("dispersion_object", "class", choices = list("corpus", "partition"), selected = "corpus", inline = TRUE),
    conditionalPanel(
      condition = "input.dispersion_object == 'corpus'",
      selectInput("dispersion_corpus", "corpus", corpus()[["corpus"]])
    ),
    conditionalPanel(
      condition = "input.dispersion_object == 'partition'",
      selectInput("dispersion_partition", "partition", choices = character())
    ),
    textInput("dispersion_query", "query", value = "Suche"),
    selectInput(
      "dispersion_pAttribute", "pAttribute",
      choices = pAttributes(corpus()[1, "corpus"])
    ),
    # radioButtons("dispersion_dims", "number of sAttributes", choices=c("1", "2"), selected="1", inline=TRUE),
    selectInput(
      "dispersion_sAttribute_1", "sAttribute",
      choices = sAttributes(corpus()[1, "corpus"]), multiple = FALSE
    ),
    radioButtons("dispersion_ts", "time series", choices = c("yes", "no"), selected = "no", inline = TRUE),
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
#' @importFrom zoo zoo
#' @importFrom data.table copy
dispersionServer <- function(input, output, session){
  
  observe({
    x <- input$dispersion_partition
    if (x != "" && length(values$partitions) > 0 && x %in% names(values$partitions)){
      new_sAttr <- sAttributes(values$partitions[[x]]@corpus)
      new_pAttr <- pAttributes(values$partitions[[x]]@corpus)
      updateSelectInput(session, "dispersion_pAttribute", choices = new_pAttr, selected = NULL)
      updateSelectInput(session, "dispersion_sAttribute_1", choices = new_sAttr, selected = NULL)
    }
  })
  
  observe({
    new_sAttr <- sAttributes(input$dispersion_corpus)
    new_pAttr <- pAttributes(input$dispersion_corpus)
    updateSelectInput(session, "dispersion_pAttribute", choices = new_pAttr, selected = NULL)
    updateSelectInput(session, "dispersion_sAttribute_1", choices = new_sAttr, selected = NULL)
  })
  
  observeEvent(
    input$dispersion_go,
    isolate({
      
      if (input$dispersion_object == "partition"){
        object <- values$partitions[[input$dispersion_partition]]
      } else if (input$dispersion_object == "corpus"){
        object <- input$dispersion_corpus
      }
      
      tab <- as.data.frame(dispersion(
        object,
        query = input$dispersion_query,
        sAttribute = input$dispersion_sAttribute_1,
        pAttribute=input$dispersion_pAttribute
      ))
      
      # tab[["freq"]] <- round(tab[["freq"]], 7)
      
      
      if (input$dispersion_ts == "yes"){
        dates4zoo <- tab[[input$dispersion_sAttribute_1]]
        tab4zoo <- data.table::copy(tab)
        tab4zoo[[input$dispersion_sAttribute_1]] <- NULL
        zooObject <- zoo::zoo(as.matrix(tab4zoo), order.by = as.Date(dates4zoo))
        if (input$dispersion_ts_aggregation != "none"){
          zooObject <- switch(
            input$dispersion_ts_aggregation,
            month = aggregate(zooObject, zoo::as.Date(zoo::as.yearmon(zoo::index(zooObject)))),
            quarter = aggregate(zooObject, zoo::as.Date(zoo::as.yearqtr(zoo::index(zooObject)))),
            year = aggregate(zooObject, as.Date(paste(gsub("^(\\d{4}).*?$", "\\1", zoo::index(zooObject)), "-01-01", sep="")))
          )
          # rework data.frame
          tab <- data.frame(date = zoo::index(zooObject), zooObject)
          colnames(tab)[1] <- input$dispersion_sAttribute_1
          rownames(tab) <- NULL
          output$dispersion_plot <- renderPlot(zoo::plot.zoo(zooObject, main=""))
        }
      } else {
        output$dispersion_plot <- renderPlot(
          barplot(
            height = tab[["count"]],
            names.arg = tab[[input$dispersion_sAttribute_1]]
          )
        )
      }
      
      output$dispersion_table <- DT::renderDataTable(as.data.frame(tab))
      
    })
  )
  
  # observeEvent(
  #   input$dispersion_mail,
  #   {
  #     if (input$dispersion_mail > 0){
  #       polmineR:::mail(
  #         values[["cooccurrences"]]
  #       )
  #     }
  #   }
  # )
  
}

