#' @include S4classes.R
NULL

#' perform t-test
#' 
#' S4 method for context object to perform t-test
#' @param .Object a context or features object
#' @rdname t_test
#' @name t_test
setGeneric("t_test", function(.Object){standardGeneric("t_test")})

#' @rdname t_test
setMethod("t_test", "context", function(.Object){
  pRandom <- (.Object@stat[["count_partition"]] / .Object@partitionSize) * ( .Object@count / .Object@partitionSize)
  pSample <- .Object@stat[["count_window"]] / .Object@partitionSize
  tValues <- (pSample - pRandom) / sqrt( pSample / .Object@partitionSize )
  .Object@stat[, "t" := tValues]
  .Object <- sort(.Object, by="t")
  .Object@stat[, "rank_t" := c(1:nrow(.Object@stat))]
  .Object@method <- c(.Object@method, "t")
  return(.Object)
})

