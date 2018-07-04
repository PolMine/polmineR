#' @include S4classes.R
NULL

#' dotplot
#' 
#' @param .Object object
#' @param col column
#' @param n number
#' @param ... further arguments that will be passed into the \code{dotchart} function
#' @exportMethod dotplot
#' @rdname dotplot-method
setGeneric("dotplot", function(.Object, ...) standardGeneric("dotplot") )

#' @rdname dotplot-method
setMethod("dotplot", "textstat", function(.Object, col, n = 20L, ...){
  if (missing(col)) stop("unable to output dotplot without col being specified")
  if (length(col) > 1L) stop("col needs to be a vector of length one")
  dotchart(x = .Object@stat[[col]][n:1L], labels = .Object@stat[[.Object@p_attribute[1]]][n:1L], ...)
})

#' @rdname dotplot-method
setMethod("dotplot", "features", function(.Object, col = NULL, n = 20L, ...){
  if (is.null(col)) col <- .Object@method[1]
  callNextMethod(.Object = .Object, col = col, n = n, ...)
})

#' @rdname dotplot-method
setMethod("dotplot", "features_ngrams", function(.Object, col = NULL, n = 20L, ...){
  if (is.null(col)) col <- .Object@method[1]
  dt_subset <- .Object@stat[n:1L][,grep("^\\d+_.*$", colnames(.Object)), with = FALSE][,n := 1L:.N]
  ngram_vector <- dt_subset[,paste(unlist(.SD[1,]), collapse = " "), by = c("n")][["V1"]]
  dotchart(x = .Object@stat[[col]][n:1L], labels = ngram_vector, ...)
})

#' @rdname dotplot-method
setMethod("dotplot", "partition", function(.Object, col = "count", n = 20L, ...){
  callNextMethod(.Object = .Object, col = col, n = n, ...)
})
