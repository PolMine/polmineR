#' @include S4classes.R polmineR.R partition.R partition_bundle.R context.R cooccurrences.R TermDocumentMatrix.R 
NULL

setOldClass("simple_triplet_matrix")

#' Type conversion - get sparseMatrix.
#' 
#' Turn objects into the sparseMatrix as defined in the Matrix package.
#' 
#' @param x object to convert
#' @param col column name to get values from (if x is a bundle)
#' @param ... Further arguments that are passed to a call to
#'   \code{sparseMatrix}. Can be used, for instance to set \code{giveCsparse} to
#'   \code{FALSE} to get a \code{dgTMatrix}, not a \code{dgCMatrix}.
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


#' @details Returns a \code{sparseMatrix} based on the counts of term cooccurrences. At this stage,
#' it is required that decoded tokens are present.
#' @exportMethod as.sparseMatrix
#' @rdname all-cooccurrences-class
#' @examples 
#' \dontrun{
#' X <- Cooccurrences("REUTERS", p_attribute = "word", left = 5L, right = 5L)
#' decode(X)
#' sm <- as.sparseMatrix(X)
#' stm <- as.simple_triplet_matrix(X)
#' }
setMethod("as.sparseMatrix", "Cooccurrences", function(x, col = "ab_count", ...){
  
  decoded_tokens <- reindex(x)
  retval <- sparseMatrix(
    i = x@stat[["a_new_index"]],
    j = x@stat[["b_new_index"]],
    x = x@stat[[col]], 
    dims = c(length(decoded_tokens), length(decoded_tokens)),
    dimnames = list(decoded_tokens, decoded_tokens),
    ...
  ) 
  
  # restore original data.table and remove columns generated during reindexing
  x@stat[, "a_new_index" := NULL][, "b_new_index" := NULL]
  retval
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
setMethod("as.sparseMatrix", "bundle", function(x, col, ...){
  message("... converting partition_bundle to TermDocumentMatrix")
  tdm_stm <- as.TermDocumentMatrix(x = x, col = col)
  message("... converting TermDocumentMatrix to Matrix")
  retval <-  as.sparseMatrix(tdm_stm, ...)
  return(retval)
})




