#' @include S4classes.R
NULL

#' Annotation functionality
#' 
#' Objects that contain analytical results (\code{kwic} objects, objects
#' inheriting from the \code{textstat} class) can be annotated by creating an
#' annotation layer using the \code{annotations}-method. The augmented object
#' can be annotated using a shiny gadget by invoking the \code{edit}-method on
#' it. Note that operations are deliberately in-place, to prevent an unwanted
#' loss of work.
#'
#' The \code{edit}-method is designed to be used in a RStudio session. It
#' generates a shiny gadget (see
#' \url{https://shiny.rstudio.com/articles/gadgets.html}) shown in the viewer
#' pane of RStudio.
#'
#' The \code{edit}-method returns the modified input object. Note however that
#' changes of annotations are deliberately in-place operations: The input object
#' is changed even if you do not close the gadget "properly" by hitting the
#' "Done" button and catch the modified object. That may be forgotten easily and
#' would be painful after the work that may have been invested.
#' 
#' Consult the examples for the intended workflow.
#' 
#' @param x An object to be annotated, a \code{kwic} class object, or an object
#'   inheriting from the \code{textstat} class.
#' @param name An S4 object to be annotated.
#' @param value A value to assign.
#' @param viewer The viewer to use, see \code{\link[shiny]{viewer}}.
#' @return The modified input object is returned invisibly.
#' @param i The row number (single \code{integer} value) of the
#'   \code{data.table} where a new value shall be assigned.
#' @param j The column number (single \code{integer} value) of the
#'   \code{data.table} where a new value shall be assigned.
#' @param ... Passed into \code{rhandsontable::rhandsontable}, can be used for
#'   settings such as \code{height} etc.
#' @importFrom utils menu
#' @importFrom DT dataTableProxy replaceData
#' @importFrom utils edit
#' @exportMethod edit
#' @rdname annotations
#' @aliases annotations,kwic-method
#' @examples 
#' use("polmineR")
#' a <- 2
#' # upon initializing a kwic object, there is a minimal labels object
#' # in the labels slot of the kwic object, which we can get using the
#' # annotations-method
#' o <- kwic("REUTERS", query = "oil")
#' annotations(o) # see the result (a data.table)
#' 
#' # assign new annotations as follows, using the reference semantics of the
#' # data.table you get by calling the labels-method on an object 
#' annotations(o) <- list(name = "class", what = factor(x = "a", levels = c("a", "b", "c")))
#' annotations(o) <- list(name = "description", what = "")
#' annotations(o) # inspect the result
#' 
#' # assign values; note that is an in-place operation using the reference
#' # semantics of the data.table
#' # annotations(o, i = 77, j = 1, value = FALSE)
#' # annotations(o, i = 78, j = 1, value = FALSE)
#' annotations(o)
#' 
#' \dontrun{
#' edit(o)
#' annotations(o) # to see changes made
#' 
#' # maybe we want additional metadata
#' enrich(o, s_attributes = "places")
#' edit(o)
#' annotations(o)
#' 
#' # to get some extra context
#' o <- enrich(o, extra = 5L, table = TRUE)
#' edit(o)
#' 
#' # lineview may be better when you use a lot of extra context
#' options(polmineR.lineview = TRUE)
#' o <- kwic("REUTERS", "oil")
#' o <- enrich(o, extra = 20L)
#' edit(o)
#' 
#' x <- cooccurrences("REUTERS", query = "oil")
#' annotations(x) <- list(name = "keep", what = TRUE)
#' annotations(x) <- list(name = "category", what = factor("a", levels = letters[1:10]))
#' edit(x)
#' }
#' @exportMethod annotations
setGeneric("annotations", function(x, ...) standardGeneric("annotations"))


#' @exportMethod annotations
#' @rdname annotations
setMethod("annotations", "kwic", function(x, i, j, value) callNextMethod())

#' @exportMethod annotations
#' @rdname annotations
setMethod("annotations", "textstat", function(x, i, j, value){
  if (missing(i)){
    return( x@stat[, c("match_id", x@annotation_cols), with = FALSE] )
  } else {
    x@stat[i, eval(j) := value]
    return( invisible(x) )
  }
})


#' @exportMethod annotations<-
#' @rdname annotations
setGeneric("annotations<-", function(x, value) standardGeneric("annotations<-"))


#' @rdname annotations
#' @exportMethod annotations<-
setReplaceMethod("annotations", signature = c(x = "kwic", value = "list"), function(x, value) callNextMethod())

#' @rdname annotations
#' @exportMethod annotations<-
setReplaceMethod("annotations", signature = c(x = "textstat", value = "list"), function(x, value){
  x@stat[, (value[["name"]]) := value[["what"]]]
  x@annotation_cols <- c(x@annotation_cols, value[["name"]])
  x
})


.check_editing_gadget_dependencies <- function(){
  for (pkg in c("shiny", "miniUI", "rhandsontable")){
    if (!requireNamespace(package = pkg, quietly = TRUE)){
      stop(sprintf("Package '%s' is required, but not available.", pkg))
    }
  }
}

.editing_gadget_ui <- function(){
  miniUI::miniPage(
    miniUI::gadgetTitleBar("Editing Gadget"), 
    miniUI::miniContentPanel(shiny::fillRow(rhandsontable::rHandsontableOutput("hot")))
  )
}



#' @exportMethod edit
#' @rdname annotations
setMethod("edit", "textstat", function(name, viewer = shiny::paneViewer(minHeight = 550), ...){
  
  .check_editing_gadget_dependencies()

  dt <- format(name)

  server <- function(input, output, session) {
    
    values <- shiny::reactiveValues()
    reactiveData <- shiny::reactive(dt)
    
    .reset_values <- function(df){
      values[["hot"]] <- df
      for (col in name@annotation_cols) name@stat[, eval(col) := df[[col]]]
    }
    
    output$hot <- rhandsontable::renderRHandsontable({
      data <- reactiveData()
      # Identical result with rhandsontable:::isErrorMessage(data), 
      # using the ::: can be avoided thereby
      if (inherits(data, "error_message")) return(NULL)
      df <- if (is.null(input$hot)) data else rhandsontable::hot_to_r(input$hot)
      .reset_values(df)
      rht <- rhandsontable::rhandsontable(df, allowedTags = "<font><div><u>",  height = 500, ...)
      rht <- rhandsontable::hot_cols(rht, manualColumnResize = TRUE)
      rht <- rhandsontable::hot_table(rht, highlightCol = TRUE, highlightRow = TRUE, overflow = "hidden", stretchH = "all")
      if (all(c("left", "node", "right") %in% colnames(dt))){
        rht <- rhandsontable::hot_col(rht, col = match(c("left", "right"), colnames(dt)), colWidths = "150")
        rht <- rhandsontable::hot_col(rht, col = "left", readOnly = TRUE, renderer = htmlwidgets::JS("safeHtmlRenderer"))
        rht <- rhandsontable::hot_col(rht, col = "node", readOnly = TRUE, renderer = htmlwidgets::JS("safeHtmlRenderer"))
        rht <- rhandsontable::hot_col(rht, col = "right", readOnly = TRUE, halign = "htCenter", renderer = htmlwidgets::JS("safeHtmlRenderer"))
      } else if ("concordance" %in% colnames(dt)){
        rht <- rhandsontable::hot_col(rht, col = "concordance", colWidths = "300")
        rht <- rhandsontable::hot_col(rht, col = "concordance", readOnly = TRUE, halign = "htCenter", renderer = htmlwidgets::JS("safeHtmlRenderer"))
      } else {
        rht <- rhandsontable::hot_col(rht, col = (1L:ncol(dt))[which(!colnames(dt) %in% name@annotation_cols)], readOnly = TRUE)
      }
      rht
    })
    shiny::observeEvent(input$done, shiny::stopApp(returnValue = name))
  }
  y <- shiny::runGadget(.editing_gadget_ui(), server, viewer = viewer)
  invisible(y)
})


