#' @include S4classes.R
NULL

#' @rdname labels-class
#' @description The \code{labels} class and the \code{label}-method supports generating and
#' working with labelled data. They rely on data in a slot \code{labels} of the
#' S4 object to be modified. Operations are deliberately in-place.
#'
#' The \code{edit}-method is designed to be used in a RStudio session. Calling
#' it generates a shiny gadget (see
#' \url{https://shiny.rstudio.com/articles/gadgets.html}) shown in the viewer
#' pane of RStudio.
#'
#' The method returns the modified input object. Note however that changes of
#' labels are deliberately in-place operations. Accordingly, the input object is
#' changed even if you do not close the gadget "properly" by hitting the "Done"
#' button and catch the modified object, which may be forgotten easily and would
#' be painful after the work that may have been invested.
#' 
#' @param x An object to be labelled.
#' @param name An S4 object with a slot \code{labels} (e.g. an object of class
#'   \code{kwic}).
#' @param n The integer index of a label to retrieve or modify.
#' @param value A value to assign.
#' @param viewer The viewer to use, see \code{\link[shiny]{viewer}}.
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
#' labels(o) <- list(name = "class", what = factor(x = "a", levels = c("a", "b", "c")))
#' labels(o) <- list(name = "description", what = "")
#' labels(o) # see the result
#' 
#' # the label-method can be used to assign values; note that is an in-place
#' # operation using the reference semantics of the data.table
#' labels(o, i = 77, j = 1, value = FALSE)
#' labels(o, i = 78, j = 1, value = FALSE)
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
setMethod("labels", "kwic", function(object, i, j, value){
  if (missing(i)){
    return( object@labels@labels )
  } else {
    object@labels@labels[i, eval(j) := value]
    return( invisible(object) )
  }
})

#' @exportMethod labels<-
#' @rdname labels-class
setGeneric("labels<-", function(x, value) standardGeneric("labels<-"))


#' @rdname labels-class
#' @exportMethod labels<-
setReplaceMethod("labels", signature = c(x = "kwic", value = "list"), function(x, value){
  labels(x)[, (value[["name"]]) := value[["what"]]]
  x
})

#' @rdname labels-class
#' @exportMethod labels<-
setReplaceMethod("labels", signature = c(x = "textstat", value = "list"), function(x, value){
  x@stat[, (value[["name"]]) := value[["what"]]]
  x
})



#' @rdname labels-class
setMethod("edit", "kwic", function(name, viewer = shiny::paneViewer(minHeight = 550), ...){
  
  if (!requireNamespace(package = "shiny", quietly = TRUE))
    stop("Package 'shiny' is required, but not available.")
  
  if (!requireNamespace(package = "miniUI", quietly = TRUE))
    stop("Package 'shiny' is required, but not available.")
  
  kwic_dt <- data.table(format(name), labels(name))
  
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
        rht <- rhandsontable::hot_col(rht, col = "left", readOnly = TRUE, renderer = htmlwidgets::JS("safeHtmlRenderer"))
      if ("node" %in% colnames(kwic_dt))
        rht <- rhandsontable::hot_col(rht, col = "node", readOnly = TRUE, renderer = htmlwidgets::JS("safeHtmlRenderer"))
      if ("right" %in% colnames(kwic_dt))
        rht <- rhandsontable::hot_col(rht, col = "right", readOnly = TRUE, halign = "htCenter", renderer = htmlwidgets::JS("safeHtmlRenderer"))
      if ("concordance" %in% colnames(kwic_dt))
        rht <- rhandsontable::hot_col(rht, col = "concordance", readOnly = TRUE, halign = "htCenter", renderer = htmlwidgets::JS("safeHtmlRenderer"))
      rht
    })
    
    shiny::observeEvent(input$done, shiny::stopApp(returnValue = name))
  }
  
  y <- shiny::runGadget(ui, server, viewer = viewer)
  invisible(y)
})
