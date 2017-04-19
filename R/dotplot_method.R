#' dotplot
#' 
#' @param .Object object
#' @param col column
#' @param n number
#' @param ... further arguments that will be passed into the dotchart function
#' @exportMethod dotplot
#' @rdname dotplot-method
setGeneric("dotplot", function(.Object, ...) standardGeneric("dotplot") )

#' @rdname dotplot-method
setMethod("dotplot", "textstat", function(.Object, col=NULL, n=20, ...){
  dotchart(x=.Object@stat[[col]][n:1], labels=.Object@stat[[.Object@pAttribute[1]]][n:1], ...)
})

#' @rdname dotplot-method
setMethod("dotplot", "features", function(.Object, col=NULL, n=20, ...){
  if (is.null(col)) col <- .Object@method[1]
  callNextMethod(.Object=.Object, col=col, n=n, ...)
})

#' @rdname dotplot-method
setMethod("dotplot", "partition", function(.Object, col="count", n=20, ...){
  callNextMethod(.Object=.Object, col=col, n=n, ...)
})
