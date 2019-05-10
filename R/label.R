#' @include S4classes.R
NULL

#' @rdname labels-class
#' @description The \code{labels} class and the \code{label}-method supports generating and
#' working with labelled data. They rely on data in a slot \code{labels} of the
#' S4 object to be modified. Operations are deliberately in-place.
#'
#' The \code{label}-method is designed to be used in a RStudio session. Calling
#' it generates a shiny gadget (see
#' \url{https://shiny.rstudio.com/articles/gadgets.html}) shown in the viewer
#' pane of RStudio. The htmlwidget embedded in the gadget can be either based on
#' the JavaScript library DataTables (default, exposed by R package \code{DT}),
#' or Handsontable (exposed by R package \code{rhandsontable}). The JavaScript
#' library to be used is specified with the argument \code{js}.
#'
#' The pinpoint is that you can modify labels. When using DataTables,
#' doubleclick on the field you with to change, modify the value, and finish
#' data entry by hitting enter or clicking somewhere else. When using
#' Handsontable, simply click on the field you wish to change.
#'
#' The method returns the modified input object. Note however that changes of
#' labels are deliberately in-place operations. Accordingly, the input object is
#' changed even if you do not close the gadget "properly" by hitting the "Done"
#' button and catch the modified object, which may be forgotten easily and would
#' be painful after the work that may have been invested.
#' 
#' @param name An S4 object with a slot \code{labels} (e.g. an object of class
#'   \code{kwic}).
#' @param n The integer index of a label to retrieve or modify.
#' @param value A value to assign.
#' @param js The javascript library to use to display tables (either "DataTables"
#'   or "Handsontable").
#' @return The modified input object is returned invisibly.
#' @param i The row number (single \code{integer} value) of the
#'   \code{data.table} of the \code{labels} object where a new value shall be
#'   assigned.
#' @param j The column number (single \code{integer} value) of the
#'   \code{data.table} of the \code{labels} object where a new value shall be
#'   assigned.
#' @param ... Passed into \code{rhandsontable::rhandsontable}, can be used for
#'   settings such as \code{height} etc.
#' @importFrom utils menu
#' @importFrom DT dataTableProxy replaceData
#' @importFrom utils edit
#' @exportMethod edit
#' @rdname labels-class
#' @aliases labels,kwic-method
#' @examples 
#' use("polmineR")
#' 
#' # upon initializing a kwic object, there is a minimal labels object
#' # in the labels slot of the kwic object, which we can get using the
#' # labels-method
#' o <- kwic("REUTERS", query = "oil")
#' labels(o) # see the result (a data.table)
#' 
#' # assign new columns as follows, using the reference semantics of the
#' # data.table you get by calling the labels-method on an object 
#' labels(o)[, "class" := factor(levels = c("a", "b", "c"))]
#' labels(o)[, "description" := ""]
#' labels(o) # see the result
#' 
#' # the label-method can be used to assign values; note that is an in-place
#' # operation using the reference semantics of the data.table
#' edit(o, i = 77, j = 1, value = FALSE)
#' edit(o, i = 78, j = 1, value = FALSE)
#' labels(o)
#' 
#' \dontrun{
#' edit(o)
#' labels(o) # to see changes made
#' 
#' # maybe we want additional metadata
#' enrich(o, s_attributes = "places")
#' edit(o)
#' labels(o)
#' 
#' # to get some extra context
#' o <- enrich(o, extra = 5L, table = TRUE)
#' edit(o)
#' 
#' # lineview may be better when you use a lot of extra context
#' options(polmineR.lineview = TRUE)
#' o <- enrich(o, extra = 20L)
#' edit(o)
#' 
#' #' edit(o, js = "DataTables")
#' labels(o) # to see changes made
#' }
#' @param object A \code{labels} object.
#' @exportMethod labels
#' @describeIn labels Get the \code{data.table} with labels from the
#'   \code{labels} object in the \code{labels} slot of the \code{kwic} class
#'   object.
setMethod("labels", "kwic", function(object) object@labels@labels)


#' @rdname labels-class
setMethod("edit", "kwic", function(name, i, j, value, js = c("Handsontable", "DataTables"), ...){
  
  if (length(js) > 1L) js <- js[1]
  
  if (missing(i)){
    if (!requireNamespace(package = "shiny", quietly = TRUE))
      stop("Package 'shiny' is required, but not available.")
    
    if (!requireNamespace(package = "miniUI", quietly = TRUE))
      stop("Package 'shiny' is required, but not available.")
    
    kwic_dt <- data.table(format(name), labels(name))
    
    if (js == "DataTables"){
      
      ui <- miniUI::miniPage(
        miniUI::gadgetTitleBar(title = "KWIC Labelling Gadget"),
        miniUI::miniContentPanel(
          shiny::fillRow(DT::dataTableOutput('kwic', height = "100%"))
        )
      )
      
      server <- function(input, output, session) {
        
        v <- shiny::reactiveValues(data = kwic_dt)
        proxy <- DT::dataTableProxy("kwic")
        
        shiny::observeEvent(
          input$kwic_cell_edit,
          {
            i <- input$kwic_cell_edit$row
            j <- input$kwic_cell_edit$col
            k <- input$kwic_cell_edit$value
            
            shiny::isolate({
              if (j %in% match(colnames(name@labels@labels), colnames(v$data))) {
                new_value <- DT::coerceValue(k, v$data[i, colnames(v$data)[j]])
                v$data[i, colnames(v$data)[j]] <<- new_value
                name@labels@labels[i, eval(j - ncol(name@table) + 1L) := new_value]
              } else {
                warning("this column cannot be edited")
              }
            })
            DT::replaceData(proxy, v[["data"]], resetPaging = FALSE)  # replaces data displayed by the updated table
          }
        )
        
        output$kwic <- DT::renderDataTable(
          DT::datatable(kwic_dt, editable = TRUE, options = list(lengthChange = FALSE)) %>%
            DT::formatStyle("node", color = "blue", textAlign = "center") %>% 
            DT::formatStyle("left", textAlign = "right")
        )
        shiny::observeEvent(input$done, shiny::stopApp(v[["data"]]))
      }
      
      y <- shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = NULL))
      return(invisible(y))
      
    } else if (js == "Handsontable"){
      
      if (!requireNamespace(package = "rhandsontable", quietly = TRUE))
        stop("Package 'rhandsontable' is required, but not available.")
      
      ui <- miniUI::miniPage(
        miniUI::gadgetTitleBar("kwic editing gadget"), 
        miniUI::miniContentPanel(
          shiny::fillRow(rhandsontable::rHandsontableOutput("hot"))
        )
      )
      
      server <- function(input, output, session) {
        
        values <- shiny::reactiveValues()
        
        reset_values <- function(df){
          values[["hot"]] <- df
          for (col in colnames(labels(name))) name@labels@labels[, eval(col) := df[[col]]]
        }
        
        reactiveData <- shiny::reactive(kwic_dt)
        
        output$pending <- shiny::renderUI({
          data <- reactiveData()
          if (rhandsontable:::isErrorMessage(data)) htmltools::h4(style = "color: #AA7732;", data$message)
        })
        
        output$hot <- rhandsontable::renderRHandsontable({
          data <- reactiveData()
          if (rhandsontable:::isErrorMessage(data)) return(NULL)
          df <- if (is.null(input$hot)) data else rhandsontable::hot_to_r(input$hot)
          reset_values(df)
          rht <- rhandsontable::rhandsontable(df, allowedTags = "<font><div><u>",  height = 500, ...)
          if ("left" %in% colnames(kwic_dt)){
            rht <- rhandsontable::hot_col(rht, col = match(c("left", "right"), colnames(kwic_dt)), colWidths = "150")
          } else {
            rht <- rhandsontable::hot_col(rht, col = "concordance", colWidths = "300")
          }
          
          rht <- rhandsontable::hot_cols(rht, manualColumnResize = TRUE)
          rht <- rhandsontable::hot_table(rht, highlightCol = TRUE, highlightRow = TRUE, overflow = "hidden", stretchH = "all")
          
          if ("left" %in% colnames(kwic_dt))
            rht <- rhandsontable::hot_col(rht, col = "left", renderer = htmlwidgets::JS("safeHtmlRenderer"))
          if ("node" %in% colnames(kwic_dt)){
            rht <- rhandsontable::hot_col(rht, col = "node", renderer = htmlwidgets::JS("safeHtmlRenderer"))
            
          }
          
          if ("right" %in% colnames(kwic_dt))
            rht <- rhandsontable::hot_col(rht, col = "right", halign = "htCenter", renderer = htmlwidgets::JS("safeHtmlRenderer"))
          if ("concordance" %in% colnames(kwic_dt))
            rht <- rhandsontable::hot_col(rht, col = "concordance", halign = "htCenter", renderer = htmlwidgets::JS("safeHtmlRenderer"))
          rht
        })
        
        shiny::observeEvent(input$done, shiny::stopApp(returnValue = name))
      }
      
      y <- shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 550))
      
      return(invisible(y))
      
    }
    
  } else {
    name@labels@labels[i, eval(j) := value]
    return(invisible(name))
  }
})
