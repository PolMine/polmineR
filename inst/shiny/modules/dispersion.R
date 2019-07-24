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
    radioButtons("dispersion_freq", "frequencies", choices = list("yes", "no"), selected = "no", inline = TRUE),
    radioButtons("dispersion_cqp", "CQP", choices = list("yes", "no"), selected = "no", inline = TRUE),
    conditionalPanel(
      condition = "input.dispersion_object == 'partition'",
      selectInput("dispersion_partition", "partition", choices = character())
    ),
    textInput("dispersion_query", "query", value = "Suche"),
    selectInput(
      "dispersion_p_attribute", "p_attribute",
      choices = p_attributes(corpus()[1, "corpus"])
    ),
    # radioButtons("dispersion_dims", "number of s_attributes", choices=c("1", "2"), selected="1", inline=TRUE),
    selectInput(
      "dispersion_s_attribute_1", "s_attribute",
      choices = s_attributes(corpus()[1, "corpus"]), multiple = FALSE
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
      new_sAttr <- s_attributes(values$partitions[[x]]@corpus)
      new_pAttr <- p_attributes(values$partitions[[x]]@corpus)
      updateSelectInput(session, "dispersion_p_attribute", choices = new_pAttr, selected = NULL)
      updateSelectInput(session, "dispersion_s_attribute_1", choices = new_sAttr, selected = NULL)
    }
  })
  
  observe({
    new_sAttr <- s_attributes(input$dispersion_corpus)
    new_pAttr <- p_attributes(input$dispersion_corpus)
    updateSelectInput(session, "dispersion_p_attribute", choices = new_pAttr, selected = NULL)
    updateSelectInput(session, "dispersion_s_attribute_1", choices = new_sAttr, selected = NULL)
  })
  


  observeEvent(
    input$dispersion_go,
    isolate({
      
      obj <- if (input$dispersion_object == "partition")
        values$partitions[[input$dispersion_partition]]
      else 
        input$dispersion_corpus

      tab <- dispersion(
        obj,
        query = rectifySpecialChars(input$dispersion_query),
        s_attribute = input$dispersion_s_attribute_1,
        p_attribute = input$dispersion_p_attribute,
        cqp = if (input$dispersion_cqp == "yes") TRUE else FALSE,
        freq = if (input$dispersion_freq == "yes") TRUE else FALSE
      )
      
      if ("freq" %in% colnames(tab)) tab[, "freq" := round(tab[["freq"]] * 100000, 7)]

      if (input$dispersion_ts == "yes"){
        dates <- tab[[input$dispersion_s_attribute_1]]
        dt <- data.table::copy(tab)
        dt[, eval(input$dispersion_s_attribute_1) := NULL]
        if ("query" %in% colnames(dt)) dt[, "query" := NULL]
        print(head(dt))
        zoo_obj <- zoo::zoo(as.matrix(dt), order.by = as.Date(dates))

        if (input$dispersion_ts_aggregation != "none"){
          zoo_obj <- switch(
            input$dispersion_ts_aggregation,
            month = aggregate(zoo_obj, zoo::as.Date(zoo::as.yearmon(zoo::index(zoo_obj)))),
            quarter = aggregate(zoo_obj, zoo::as.Date(zoo::as.yearqtr(zoo::index(zoo_obj)))),
            year = aggregate(zoo_obj, as.Date(paste(gsub("^(\\d{4}).*?$", "\\1", zoo::index(zoo_obj)), "-01-01", sep="")))
          )
          # rework data.frame
          tab <- data.frame(date = zoo::index(zoo_obj), zoo_obj)
          colnames(tab)[1] <- input$dispersion_s_attribute_1
          rownames(tab) <- NULL
          output$dispersion_plot <- renderPlot(zoo::plot.zoo(zoo_obj, main = ""))
        }
      } else {
        output$dispersion_plot <- renderPlot(
          barplot(
            height = if (input$dispersion_freq == "yes") tab[["freq"]] else tab[["count"]],
            names.arg = tab[[input$dispersion_s_attribute_1]]
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

