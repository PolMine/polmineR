#' @include S4classes.R
NULL

#' Perform t-test.
#' 
#' S4 method to perform t-test.
#' 
#' The calculation of the t-test is based on the formula
#' \deqn{t = \frac{\overline{x} - \mu}{\sqrt{\frac{s^2}{N}}}}{}
#' where \eqn{\mu} is the mean of the distribution, x the sample mean, \eqn{s^2}{s^2} the sample variance,
#' and N the sample size.
#' 
#' @param .Object A \code{context} or \code{features} object
#' @references Manning, Christopher D.; Schuetze, Hinrich (1999): \emph{Foundations of Statistical Natural Language
#' Processing}. MIT Press: Cambridge, Mass., pp. 163-166.
#' @rdname t_test
#' @name t_test
setGeneric("t_test", function(.Object) standardGeneric("t_test") )

#' @rdname t_test
setMethod("t_test", "context", function(.Object){
  p_random <- (.Object@stat[["count_partition"]] / .Object@size_partition) * ( .Object@count / .Object@size_partition)
  p_sample <- .Object@stat[["count_coi"]] / .Object@size_partition
  t_values <- (p_sample - p_random) / sqrt( p_sample / .Object@size_partition )
  .Object@stat[, "t" := t_values]
  setorderv(x = .Object@stat, cols = "t")
  # .Object <- sort(.Object, by = "t") # replace by setorderv
  .Object@stat[, "rank_t" := 1L:nrow(.Object@stat)]
  .Object@method <- c(.Object@method, "t")
  invisible(.Object)
})

