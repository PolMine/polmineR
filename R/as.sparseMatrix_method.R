#' @include polmineR_package.R partition_class.R partitionBundle_class.R context_class.R cooccurrences_class.R contextBundle_class.R as.DocumentTermMatrix_method.R
NULL


#' @exportMethod as.sparseMatrix
setGeneric("as.sparseMatrix", function(x,...){standardGeneric("as.sparseMatrix")})


#' @docType methods
#' @importFrom Matrix sparseMatrix
#' @exportMethod as.sparseMatrix
setMethod("as.sparseMatrix", "TermDocumentMatrix", function(x){
  sparseMatrix(
    i=x$i, j=x$j, x=x$v,
    dims=c(x$nrow, x$ncol),
    dimnames = dimnames(x),
    giveCsparse = TRUE
    )
})


#' @docType methods
#' @rdname bundle-class
setMethod("as.sparseMatrix", "bundle", function(x, col){
  message("... converting partitionBundle to TermDocumentMatrix")
  tdm_stm <- as.TermDocumentMatrix(x=x, col=col)
  message("... converting TermDocumentMatrix to Matrix")
  retval <-  as.sparseMatrix(tdm_stm)
  return(retval)
})



