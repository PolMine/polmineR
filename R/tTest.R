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
  pRandom <- (.Object@stat[["count_window"]] / .Object@partitionSize) * ( .Object@count / .Object@partitionSize)
  pSample <- .Object@stat[["count_"]] / .Object@partitionSize
  tValues <- (pSample - pRandom) / sqrt( pSample / partitionObject@size )
  .Object@stat[, t := tValues]
  .Object@statisticalTest <- c(.Object@statisticalTest, "t")
  return(.Object)
})

