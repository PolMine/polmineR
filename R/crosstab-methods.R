# documented with crosstab class
#' @docType methods
#' @noRd
setMethod("t", "crosstab", function(x){
  x@objects <- as.data.frame(t(x@objects))
  x@abs <- as.data.frame(t(x@abs))
  x@rel <- as.data.frame(t(x@rel))
  rows <- x@rows
  cols <- x@cols
  x@rows <- cols
  x@cols <- rows
  x  
})

#' show a crosstab object
#' 
#' @param object a crosstab object
#' @author Andreas Blaette
#' @rdname show-crosstab-method
#' @exportMethod show
#' @docType methods
#' @noRd
setMethod("show", "crosstab",
          function(object){
            cat('Query:', object@query, '; Rows:', object@rows, '; Columns:', object@cols, '\n\n')
            print(object@rel)
            cat('\n')
            print(object@abs)
          })
