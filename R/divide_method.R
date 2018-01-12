#' @rdname divide
setGeneric("divide", function(.Object, n) standardGeneric("divide"))

#' Divide an object into roughly equally sized parts.
#' 
#' This is a helper to prepare lapply/mclapply operations on vectors, or matrices.
#' 
#' @param .Object object to split up
#' @param n number of objects
#' @rdname divide
#' @exportMethod divide
#' @seealso \code{\link[text2vec]{split_into}} in the text2vec
#' package offers a similar, and faster function for vectors
#' @return a list of objects of the same class as the input object
#' @examples
#' divide(1:10, n = 3)
#' M <- matrix(1:20, ncol = 2, byrow = TRUE)
#' divide(M, n = 3)
#' divide(M, n = 4)
setMethod("divide", "matrix", function(.Object, n){
  chunkFactor <- cut(
    1:nrow(.Object),
    unique( c(1, floor(c(1:(n-1))*(nrow(.Object)/n)), nrow(.Object)) ),
    include.lowest = TRUE
  )
  chunkedMatrix <- split(.Object, chunkFactor)
  retval <- lapply(chunkedMatrix, function(m) matrix(m, ncol = ncol(.Object)))
  unname(retval)
})

#' @rdname divide
setMethod("divide", "vector", function(.Object, n){
  chunkFactor <- cut(
    1:length(.Object),
    unique(c(1, floor(c(1:(n-1))*(length(.Object)/n)), length(.Object))),
    include.lowest=TRUE
  )
  retval <- split(.Object, chunkFactor)
  unname(retval)
})
