#' Methods for TermDocumentMatrix / DocumentTermMatrix
#' 
#' @param x object
#' @rdname TermDocumentMatrix
#' @name TermDocumentMatrix
setOldClass("TermDocumentMatrix")
setOldClass("DocumentTermMatrix")


#' @importFrom slam as.simple_triplet_matrix
setMethod("cbind2", signature=c(x="TermDocumentMatrix", y="TermDocumentMatrix"), function(x,y){
  combinedMatrix <- do.call(cbind2, lapply(list(x,y), as.sparseMatrix))
  returnedMatrix <- as.simple_triplet_matrix(combinedMatrix)
  class(returnedMatrix) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  returnedMatrix
})

