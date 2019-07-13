#' @include partition.R hits.R S4classes.R
NULL

#' @rdname dispersion-method
#' @aliases dispersion,partition-method
setGeneric("dispersion", function(.Object, ...){standardGeneric("dispersion")})


#' Dispersion of a query or multiple queries.
#' 
#' The method returns counts (optionally frequencies) of a query or a multiple
#' queries in subcorpora defined by one or two s-attributes.
#' 
#' @param .Object A \code{partition} object or a corpus provided by a character
#'   string.
#' @param query A \code{character} vector stating one or multiple queries.
#' @param s_attribute A \code{character} vector (length 1 or 2) providing s-attributes.
#' @param p_attribute Length one \code{character} vector, the p-attribute that
#'   will be looked up (typically 'word' or 'lemma').
#' @param cqp If \code{logical}, whether the query is a CQP query, if it is a
#'   function that is passed in, the function will be applied to the query to
#'   guess whether query is a CQP query
#' @param freq A \code{logical} value, whether to calculate normalized frequencies.
#' @param mc A \code{logical} value, whether to use multicore.
#' @param verbose A \code{logical} value, whether to be verbose.
#' @param progress A \code{logical} value, whether to show progress.
#' @param ... Further parameters.
#' @return depends on the input, as this is a wrapper function
#' @seealso The worker behind the \code{dispersion}-method is the \code{hits}-method.
#' @exportMethod dispersion
#' @examples
#' use("polmineR")
#' dispersion("GERMAPARLMINI", query = "Integration", s_attribute = "date")
#' 
#' test <- partition("GERMAPARLMINI", date = ".*", p_attribute = NULL, regex = TRUE)
#' integration <- dispersion(
#'   test, query = "Integration",
#'   p_attribute = "word", s_attribute = "date"
#' )
#' integration <- dispersion(test, "Integration", s_attribute = c("date", "party"))
#' integration <- dispersion(test, '"Integration.*"', s_attribute = "date", cqp = TRUE)
#' @seealso count
#' @return A \code{data.table}.
#' @author Andreas Blaette
#' @docType methods
#' @exportMethod dispersion
#' @rdname dispersion-method
#' @aliases dispersion,slice-method
#' @name dispersion
setMethod("dispersion", "slice", function(.Object, query, s_attribute, cqp = FALSE, p_attribute = getOption("polmineR.p_attribute"), freq = FALSE, mc = FALSE, progress = TRUE, verbose = FALSE, ...){
  dot_list <- list(...)
  if ("sAttribute" %in% names(dot_list)) s_attribute <- dot_list[["sAttribute"]]
  if ("pAttribute" %in% names(dot_list)) p_attribute <- dot_list[["pAttribute"]]
  
  h <- hits(
    .Object = .Object, query = query, cqp = cqp,
    s_attribute = s_attribute, p_attribute = p_attribute, freq = freq,
    mc = mc, verbose = verbose, progress = progress
  )
  dispersion(h, s_attribute = s_attribute, freq = freq)
})

#' @rdname dispersion-method
setMethod("dispersion", "partition", function(
  .Object, query, s_attribute, cqp = FALSE, p_attribute = getOption("polmineR.p_attribute"),
  freq = FALSE, mc = FALSE, progress = TRUE, verbose = FALSE, ...){
  callNextMethod()
})

#' @rdname dispersion-method
setMethod("dispersion", "subcorpus", function(
  .Object, query, s_attribute, cqp = FALSE, p_attribute = getOption("polmineR.p_attribute"),
  freq = FALSE, mc = FALSE, progress = TRUE, verbose = FALSE, ...){
  callNextMethod()
})


#' @rdname dispersion-method
setMethod("dispersion", "corpus", function(.Object, query, s_attribute, cqp = is.cqp, p_attribute = getOption("polmineR.p_attribute"), freq = FALSE, mc = FALSE, progress = TRUE, verbose = TRUE, ...){
  dot_list <- list(...)
  if ("sAttribute" %in% names(dot_list)) s_attribute <- dot_list[["sAttribute"]]
  if ("pAttribute" %in% names(dot_list)) p_attribute <- dot_list[["pAttribute"]]
  
  h <- hits(
    .Object, query = query, cqp = cqp, s_attribute = s_attribute, p_attribute = p_attribute, freq = freq,
    mc = mc, verbose = verbose, progress = progress
  )
  dispersion(h, s_attribute = s_attribute, freq = freq)
})


#' @rdname dispersion-method
setMethod("dispersion", "character", function(.Object, query, s_attribute, cqp = is.cqp, p_attribute = getOption("polmineR.p_attribute"), freq = FALSE, mc = FALSE, progress = TRUE, verbose = TRUE, ...){
  dispersion(
    .Object = corpus(.Object),
    query = query,
    s_attribute = s_attribute,
    cqp = cqp,
    p_attribute = p_attribute,
    freq = freq,
    mc = mc,
    progress = progress,
    verbose = verbose,
    ...
  )
})


#' @rdname dispersion-method
setMethod("dispersion", "hits", function(.Object, s_attribute, freq = FALSE, verbose = TRUE, ...){
  
  dot_list <- list(...)
  if ("sAttribute" %in% names(dot_list)) s_attribute <- dot_list[["sAttribute"]]
  if ("pAttribute" %in% names(dot_list)) p_attribute <- dot_list[["pAttribute"]]
  
  if (length(.Object@query) > 1L){
    if (!freq){
      .Object@stat <- .Object@stat[, {sum(.SD[["count"]])}, by = s_attribute][, "query" := paste(.Object@query, collapse = "//")]
      setnames(.Object@stat, old = "V1", new = "count")
      setcolorder(.Object@stat, neworder = c("query", "count", s_attribute))
    } else {
      .Object@stat <- .Object@stat[, "freq" := NULL][, {list(count = sum(.SD[["count"]]), size = sum(.SD[["size"]]))}, by = s_attribute]
      .Object@stat[, "freq" := .Object@stat[["count"]] / .Object@stat[["size"]]][, "query" := paste(.Object@query, collapse = "//")]
      setcolorder(.Object@stat, neworder = c("query", "count", s_attribute))
    }
  }
  
  
  if (length(s_attribute) == 2L){
    for (s_attr in s_attribute) if ("" %in% .Object@stat[[s_attr]]){
      warning(
        "There is a zero-length character vector for s_attribute ",
        s_attr,
        ", this will result in a column V1 (V2, V3, ...)."
      )
    }
    retval <- data.table::dcast.data.table(
      .Object@stat, formula(paste(s_attribute, collapse = "~")),
      value.var = if (freq) "freq" else "count", fun.aggregate = sum, fill = 0
      )  
  } else if (length(s_attribute) == 1L){
    retval <- .Object@stat
    # if (!freq){
    #   sumup <- function(.SD) sum(.SD[["count"]])
    #   retval <- .Object@stat[, sumup(.SD), by = c(s_attribute), with = TRUE]
    #   data.table::setnames(retval, old = "V1", new = "count")
    # } else {
    #   retval <- .Object@stat
    # }
  } else {
    warning("length(s_attribute) needs to be 1 or 2")
  }
  retval
})
