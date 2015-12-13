#' @exportMethod dotplot
setGeneric("dotplot", function(.Object, ...) standardGeneric("dotplot") )

setMethod("dotplot", "textstat", function(.Object, col=NULL, n=20, ...){
  dotchart(x=.Object@stat[[col]][n:1], labels=.Object@stat[[.Object@pAttribute[1]]][n:1], ...)
})

setMethod("dotplot", "comp", function(.Object, col=NULL, n=20, ...){
  if (is.null(col)) col <- .Object@method[1]
  callNextMethod(.Object=.Object, col=col, n=n, ...)
})

setMethod("dotplot", "partition", function(.Object, col="count", n=20, ...){
  callNextMethod(.Object=.Object, col=col, n=n, ...)
})
