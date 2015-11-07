#' perform t-test
#' 
#' S4 method for context object to perform t-test
#' @param .Object a context or keyness object
#' @param partitionObject partition to be passed in
#' @param ... further parameters
#' @rdname tTest
#' @name tTest
setGeneric("tTest", function(.Object, ...){standardGeneric("tTest")})

#' @rdname tTest
setMethod("tTest", "context", function(.Object, partitionObject){
  pRandom <- (.Object@stat$countCoi / partitionObject@size) * ( .Object@frequency / partitionObject@size)
  pSample <- .Object@stat$countCooc / partitionObject@size
  tValues <- (pSample - pRandom) / sqrt( pSample / partitionObject@size )
  .Object@stat[, t := tValues]
  .Object@statisticalTest <- c(.Object@statisticalTest, "t")
  return(.Object)
})

