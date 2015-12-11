#' @exportMethod dotChart
setGeneric("dotChart", function(x, ...) standardGeneric("dotChart") )

setMethod("dotChart", "textstat", function(x, col, n=20, ...){
  dotchart(x=x@stat[[col]][n:1], labels=x@stat[[1]][n:1], ...)
})