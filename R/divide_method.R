#' @rdname divide
setGeneric("divide", function(.Object, n) standardGeneric("divide"))

#' divide an object into equally sized parts
#' 
#' @param .Object object to divide
#' @param n number of objects
#' @rdname divide
#' @exportMethod divide
setMethod("divide", "matrix", function(.Object, n){
  chunkFactor <- cut(
    c(1:nrow(.Object)),
    unique(c(1, floor(c(1:(n-1))*(nrow(.Object)/n)), nrow(.Object))),
    include.lowest=TRUE
  )
  chunkedMatrix <- split(.Object, chunkFactor)
  lapply(chunkedMatrix, function(m) matrix(m, ncol=ncol(.Object)))
})

#' @rdname divide
setMethod("divide", "vector", function(.Object, n){
  chunkFactor <- cut(
    c(1:length(.Object)),
    unique(c(1, floor(c(1:(n-1))*(length(.Object)/n)), length(.Object))),
    include.lowest=TRUE
  )
  split(.Object, chunkFactor)
})
