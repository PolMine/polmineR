#' perform t-test
#' 
#' S4 method for context object to perform t-test
#' @param object a context or keyness object
#' @param partitionObject partition to be passed in
#' @param ... further parameters
#' @rdname tTest
#' @name tTest
setGeneric("tTest", function(object, ...){standardGeneric("tTest")})

#' @rdname tTest
setMethod("tTest", "context", function(object, partitionObject){
  pRandom <- (object@stat$countCoi / partitionObject@size) * ( object@frequency / partitionObject@size)
  pSample <- object@stat$countCooc / partitionObject@size
  object@stat$t <- (pSample - pRandom) / sqrt( pSample / partitionObject@size )
  object@statisticalTest <- c(object@statisticalTest, "t")
  return(object)
})

