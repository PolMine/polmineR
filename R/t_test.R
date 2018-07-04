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
  p_random <- (.Object@stat[["count_partition"]] / .Object@size_partition) * ( .Object@count / .Object@size_partition)
  p_sample <- .Object@stat[["count_window"]] / .Object@size_partition
  t_values <- (p_sample - p_random) / sqrt( p_sample / .Object@size_partition )
  .Object@stat[, "t" := t_values]
  .Object <- sort(.Object, by = "t")
  .Object@stat[, "rank_t" := 1L:nrow(.Object@stat)]
  .Object@method <- c(.Object@method, "t")
  .Object
})

