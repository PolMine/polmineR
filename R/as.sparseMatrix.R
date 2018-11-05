#' @include S4classes.R polmineR.R partition.R partition_bundle.R context.R cooccurrences.R TermDocumentMatrix.R 
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


#' @details Returns a \code{sparseMatrix} based on the counts of term cooccurrences. At this stage,
#' it is required that decoded tokens are present.
#' @exportMethod as.sparseMatrix
#' @rdname all-cooccurrences-class
#' @examples 
#' X <- Cooccurrences("REUTERS", p_attribute = "word", left = 5L, right = 5L)
#' decode(X)
#' sm <- as.sparseMatrix(X)
#' stm <- as.simple_triplet_matrix(X)
setMethod("as.sparseMatrix", "Cooccurrences", function(x, col = "ab_count"){
  a_col <- paste("a", x@p_attribute, sep = "_")
  b_col <- paste("b", x@p_attribute, sep = "_")
  unique_terms <- unique(c(x@stat[[a_col]], x@stat[[b_col]]))
  key_vector <- setNames(1L:length(unique_terms), unique_terms)
  splitted_tab <- split(x = x@stat[,c(col, b_col), with = FALSE], f = x@stat[[a_col]])
  
  i <- unname(unlist(lapply(names(splitted_tab), function(n) rep(key_vector[n], times = nrow(splitted_tab[[n]]))))) #nodes
  j <- unname(unlist(lapply(splitted_tab, function(tab) key_vector[tab[[b_col]] ]))) # cooccurrences
  v <- unname(unlist(lapply(splitted_tab, function(tab) tab[[col]]))) # values
  
  sparseMatrix(
    i = i,
    j = j,
    x = v, 
    dims = c(length(unique_terms), length(unique_terms)),
    dimnames = list(names(key_vector), names(key_vector)),
    giveCsparse = TRUE
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
  message("... converting partition_bundle to TermDocumentMatrix")
  tdm_stm <- as.TermDocumentMatrix(x = x, col = col)
  message("... converting TermDocumentMatrix to Matrix")
  retval <-  as.sparseMatrix(tdm_stm)
  return(retval)
})




