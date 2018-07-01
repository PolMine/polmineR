#' @include partition.R hits.R S4classes.R
NULL

#' @rdname dispersion-method
#' @aliases dispersion,partition-method
setGeneric("dispersion", function(.Object, ...){standardGeneric("dispersion")})


#' Dispersion of a query or multiple queries
#' 
#' The function returns the frequencies of a query or a multiple queries
#' in sub-partitions defined by one or two dimensions. This is a wrapper function, so the output will depend
#' on the number of queries and dimensions provided.
#' 
#' @param .Object a partition object
#' @param query a character vector containing one or multiple queries
#' @param s_attribute a character vector of length 1 or 2 providing the s-attributes 
#' @param p_attribute the p-attribute that will be looked up, typically 'word'
#' or 'lemma'
#' @param cqp if logical, whether the query is a CQP query (TRUE/FALSE), if it is a function that is passed in, the function will be applied to the query to guess whether query is a CQP query
#' @param freq logical, whether to calculate normalized frequencies
#' @param mc logical, whether to use multicore
#' @param verbose logical, whether to be verbose
#' @param progress logical, whether to shop progress
#' @param ... further parameters
#' @return depends on the input, as this is a wrapper function
#' @seealso \code{crosstab-class}
#' @exportMethod dispersion
#' @examples
#' use("polmineR")
#' test <- partition("GERMAPARLMINI", date = ".*", p_attribute = NULL, regex = TRUE)
#' integration <- dispersion(
#'   test, query = "Integration",
#'   p_attribute = "word", s_attribute = "date"
#' )
#' integration <- dispersion(test, "Integration", s_attribute = c("date", "party"))
#' integration <- dispersion(test, '"Integration.*"', s_attribute = "date", cqp = TRUE)
#' @seealso count
#' @author Andreas Blaette
#' @docType methods
#' @exportMethod dispersion
#' @rdname dispersion-method
#' @name dispersion
setMethod("dispersion", "partition", function(.Object, query, s_attribute, cqp = FALSE, p_attribute = getOption("polmineR.p_attribute"), freq = FALSE, mc = FALSE, progress = TRUE, verbose = FALSE, ...){
  dot_list <- list(...)
  if ("sAttribute" %in% names(dot_list)) s_attribute <- dot_list[["sAttribute"]]
  if ("pAttribute" %in% names(dot_list)) p_attribute <- dot_list[["pAttribute"]]
  
  dispersion(
    hits(
      .Object = .Object, query = query, cqp = cqp,
      s_attribute = s_attribute, p_attribute = p_attribute, freq = freq,
      mc = mc, verbose = verbose, progress = progress
    ),
    s_attribute = s_attribute, freq = freq
  )
})

#' @rdname dispersion-method
setMethod("dispersion", "character", function(.Object, query, s_attribute, cqp = is.cqp, p_attribute = getOption("polmineR.p_attribute"), freq = FALSE, mc = FALSE, progress = TRUE, verbose = TRUE, ...){
  
  dot_list <- list(...)
  if ("sAttribute" %in% names(dot_list)) s_attribute <- dot_list[["sAttribute"]]
  if ("pAttribute" %in% names(dot_list)) p_attribute <- dot_list[["pAttribute"]]
  
  dispersion(
    hits(
      .Object, query = query, cqp = cqp, s_attribute = s_attribute, p_attribute = p_attribute, freq = freq,
      mc = mc, verbose = verbose, progress = progress
    ),
    s_attribute = s_attribute, freq = freq
  )
})


#' @rdname dispersion-method
setMethod("dispersion", "hits", function(.Object, s_attribute, freq = FALSE, verbose = TRUE, ...){
  
  dot_list <- list(...)
  if ("sAttribute" %in% names(dot_list)) s_attribute <- dot_list[["sAttribute"]]
  if ("pAttribute" %in% names(dot_list)) p_attribute <- dot_list[["pAttribute"]]
  
  
  if (length(s_attribute) == 2){
    retval <- data.table::dcast.data.table(
      .Object@stat, formula(paste(s_attribute, collapse = "~")),
      value.var = if (freq) "freq" else "count", fun.aggregate = sum, fill = 0
      )  
  } else if (length(s_attribute) == 1){
    if (freq == FALSE){
      sumup <- function(.SD) sum(.SD[["count"]])
      retval <- .Object@stat[, sumup(.SD), by = c(s_attribute), with = TRUE]
      data.table::setnames(retval, old = "V1", new = "count")
    } else {
      retval <- .Object@stat
    }
  } else {
    warning("length(s_attribute) needs to be 1 or 2")
  }
  retval
})
