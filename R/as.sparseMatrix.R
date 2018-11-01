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


#' @details Returns a simple triplet
#'   matrix based on the counts of term cooccurrences. If counts are not yet
#'   present, that is done first.
#' @exportMethod as.sparseMatrix
#' @rdname all-cooccurrences-class
setMethod("as.sparseMatrix", "Cooccurrences", function(x, col){
  uniqueTerms <- unique(c(x@stat[,"node"], x@stat[,"cooccurrence"]))
  keyVector <- setNames(1L:length(uniqueTerms), uniqueTerms)
  splittedTab <- split(x = x@stat[,c(col, "cooccurrence")], f = x@stat[,"node"])
  
  bag <- list()
  i <- unname(unlist(lapply(names(splittedTab), function(n) rep(keyVector[n], times = nrow(splittedTab[[n]]))))) #nodes
  j <- unname(unlist(lapply(splittedTab, function(tab) keyVector[tab[,"cooccurrence"]]))) # cooccurrences
  v <- unname(unlist(lapply(splittedTab, function(tab) tab[,col]))) # values
  
  sparseMatrix(
    i = i,
    j = j,
    x = v, 
    dims = c(length(uniqueTerms), length(uniqueTerms)),
    dimnames = list(names(keyVector), names(keyVector)),
    giveCsparse = TRUE
  )   
})




