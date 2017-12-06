#' @include polmineR_package.R partition_class.R partitionBundle_class.R context_class.R cooccurrences_class.R contextBundle_class.R as.DocumentTermMatrix_method.R
NULL

setOldClass("simple_triplet_matrix")

#' Type conversion - get sparseMatrix.
#' 
#' Turn objects into the sparseMatrix as defined in the Matrix package.
#' 
#' @param x object to convert
#' @param col column name to get values from (if x is a bundle)
#' @param ... further parameters
#' @exportMethod as.sparseMatrix
#' @rdname as.sparseMatrix
setGeneric("as.sparseMatrix", function(x,...) standardGeneric("as.sparseMatrix"))


#' @docType methods
#' @importFrom Matrix sparseMatrix
#' @rdname as.sparseMatrix
setMethod("as.sparseMatrix", "simple_triplet_matrix", function(x, ...){
  Matrix::sparseMatrix(
    i = x$i, j = x$j, x = x$v,
    dims = c(x$nrow, x$ncol),
    dimnames = dimnames(x),
    ...
    )
})

#' @importFrom Matrix sparseMatrix
#' @rdname as.sparseMatrix
setMethod("as.sparseMatrix", "TermDocumentMatrix", function(x, ...){
  Matrix::sparseMatrix(
    i = x$i, j = x$j, x = x$v,
    dims = c(x$nrow, x$ncol),
    dimnames = dimnames(x),
    ...
  )
})



#' @docType methods
#' @rdname as.sparseMatrix
setMethod("as.sparseMatrix", "bundle", function(x, col){
  message("... converting partitionBundle to TermDocumentMatrix")
  tdm_stm <- as.TermDocumentMatrix(x=x, col=col)
  message("... converting TermDocumentMatrix to Matrix")
  retval <-  as.sparseMatrix(tdm_stm)
  return(retval)
})



