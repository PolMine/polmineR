#' @include partition.R hits.R S4classes.R
NULL

#' @rdname dispersion-method
#' @aliases dispersion,partition-method
setGeneric("dispersion", function(.Object, ...){standardGeneric("dispersion")})


#' Dispersion of a query or multiple queries.
#' 
#' The method returns a \code{data.table} with the number of matches of a query
#' or multiple queries (optionally frequencies) in a corpus or subcorpus as
#' partitioned by one or two s-attributes.
#' 
#' @details Augmenting the \code{data.table} with zeros for subcorpora that do
#'   not yield query matches (argument \code{fill} = \code{TRUE}) may require
#'   adding many new columns. A respective warning issued by the
#'   \code{data.table} package is supplemented an additional explanatory note
#'   of the polmineR package.
#' @param .Object A \code{corpus}, \code{subcorpus} or \code{partition} object
#'   or a corpus provided by a character string.
#' @param query A \code{character} vector stating one or multiple queries.
#' @param s_attribute A \code{character} vector (length 1 or 2) providing s-attributes.
#' @param p_attribute Length one \code{character} vector, the p-attribute that
#'   will be looked up (typically 'word' or 'lemma').
#' @param cqp If \code{logical}, whether the query is a CQP query, if it is a
#'   function that is passed in, the function will be applied to the query to
#'   guess whether query is a CQP query
#' @param freq A \code{logical} value, whether to calculate normalized frequencies.
#' @param fill A \code{logical} value, whether to report zero matches. Defaults
#'   to \code{TRUE}. But note that if there are few matches and many values of
#'   the s-attribute(s), the resulting data structure is sparse and potentially
#'   bloated.
#' @param mc A \code{logical} value, whether to use multicore.
#' @param verbose A \code{logical} value, whether to be verbose.
#' @param progress A \code{logical} value, whether to show progress.
#' @param ... Further parameters.
#' @return A \code{data.table}.
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
setMethod("dispersion", "slice", function(.Object, query, s_attribute, cqp = FALSE, p_attribute = getOption("polmineR.p_attribute"), freq = FALSE, fill = TRUE, mc = FALSE, progress = FALSE, verbose = FALSE, ...){
  dot_list <- list(...)
  if ("sAttribute" %in% names(dot_list)){
    warning("The `sAttribute` argument of `dispersion()` is deprecated. Please use argument `s_attribute` instead.")
    s_attribute <- dot_list[["sAttribute"]]
  }
  if ("pAttribute" %in% names(dot_list)){
    warning("The `pAttribute` argument of `dispersion()` is deprecated. Please use argument `p_attribute` instead.")
    p_attribute <- dot_list[["pAttribute"]]
  }
  
  h <- hits(
    .Object = .Object, query = query, cqp = cqp,
    s_attribute = s_attribute, p_attribute = p_attribute, freq = freq,
    mc = mc, verbose = verbose, progress = progress
  )
  dispersion(h, s_attribute = s_attribute, source = .Object, freq = freq, fill = fill)
})

#' @rdname dispersion-method
setMethod("dispersion", "partition", function(
  .Object, query, s_attribute, cqp = FALSE, p_attribute = getOption("polmineR.p_attribute"),
  freq = FALSE, fill = TRUE, mc = FALSE, progress = TRUE, verbose = FALSE, ...){
  callNextMethod()
})

#' @rdname dispersion-method
setMethod("dispersion", "subcorpus", function(
  .Object, query, s_attribute, cqp = FALSE, p_attribute = getOption("polmineR.p_attribute"),
  freq = FALSE, fill = FALSE, mc = FALSE, progress = TRUE, verbose = FALSE, ...){
  callNextMethod()
})


#' @rdname dispersion-method
setMethod("dispersion", "corpus", function(.Object, query, s_attribute, cqp = is.cqp, p_attribute = getOption("polmineR.p_attribute"), freq = FALSE, fill = TRUE, mc = FALSE, progress = FALSE, verbose = FALSE, ...){
  dot_list <- list(...)
  if ("sAttribute" %in% names(dot_list)){
    warning("The `sAttribute` argument of `dispersion()` is deprecated. Please use argument `s_attribute` instead.")
    s_attribute <- dot_list[["sAttribute"]]
  }
  if ("pAttribute" %in% names(dot_list)){
    warning("The `pAttribute` argument of `dispersion()` is deprecated. Please use argument `p_attribute` instead.")
    p_attribute <- dot_list[["pAttribute"]]
  }

  h <- hits(
    .Object, query = query, cqp = cqp,
    s_attribute = s_attribute, p_attribute = p_attribute, 
    freq = freq,
    mc = mc, verbose = verbose, progress = progress
  )
  dispersion(h, s_attribute = s_attribute, source = .Object, freq = freq, fill = fill)
})


#' @rdname dispersion-method
setMethod("dispersion", "character", function(.Object, query, s_attribute, cqp = is.cqp, p_attribute = getOption("polmineR.p_attribute"), freq = FALSE, fill = TRUE, mc = FALSE, progress = TRUE, verbose = TRUE, ...){
  dispersion(
    .Object = corpus(.Object),
    query = query,
    s_attribute = s_attribute,
    cqp = cqp,
    p_attribute = p_attribute,
    freq = freq,
    fill = fill,
    mc = mc,
    progress = progress,
    verbose = verbose,
    ...
  )
})


#' @importFrom data.table truelength
#' @rdname dispersion-method
#' @param source The source of the evaluation the hits reported in
#'   \code{.Object} are based on, a \code{corpus}, \code{subcorpus} or
#'   \code{partition} object.
setMethod("dispersion", "hits", function(.Object, source, s_attribute, freq = FALSE, fill = TRUE, verbose = TRUE, ...){
  
  dot_list <- list(...)
  if ("sAttribute" %in% names(dot_list)){
    warning("The `sAttribute` argument of `dispersion()` is deprecated. Please use argument `s_attribute` instead.")
    s_attribute <- dot_list[["sAttribute"]]
  }
  if ("pAttribute" %in% names(dot_list)){
    warning("The `pAttribute` argument of `dispersion()` is deprecated. Please use argument `p_attribute` instead.")
    p_attribute <- dot_list[["pAttribute"]]
  }

  if (!length(s_attribute) %in% c(1L, 2L))
    stop(sprintf("Number of s-attributes is %d but only 1 or 2 s-attributes are allowed.", length(s_attribute)))
  
  if (length(.Object@query) > 1L){
    if (isFALSE(freq)){
      .Object@stat <- .Object@stat[, {sum(.SD[["count"]])}, by = s_attribute][, "query" := paste(.Object@query, collapse = "//")]
      setnames(.Object@stat, old = "V1", new = "count")
      setcolorder(.Object@stat, neworder = c("query", "count", s_attribute))
    } else {
      .Object@stat <- .Object@stat[, "freq" := NULL][, {list(count = sum(.SD[["count"]]), size = unique(.SD[["size"]]))}, by = s_attribute]
      .Object@stat[, "freq" := .Object@stat[["count"]] / .Object@stat[["size"]]][, "query" := paste(.Object@query, collapse = "//")]
      setcolorder(.Object@stat, neworder = c("query", "count", s_attribute))
    }
  }
  
  
  if (length(s_attribute) == 1L){
    dt <- .Object@stat
    # ensure that zero matches are reported for all values of the s-attribute
    if (isTRUE(fill)){
      s_attr_values <- s_attributes(source, s_attribute = s_attribute, unique = TRUE)
      dt <- dt[do.call(data.table, setNames(list(s_attr_values, s_attribute), c(s_attribute, "key"))), on = s_attribute]
      dt[, "count" := ifelse(is.na(dt[["count"]]), 0L, dt[["count"]])]
    }
    if (any(is.na(dt[["query"]]))) dt[, "query" := unique(dt[["query"]][!is.na(dt[["query"]])])]
  } else if (length(s_attribute) == 2L){
    for (s_attr in s_attribute) if ("" %in% .Object@stat[[s_attr]]){
      warning(
        "There is a zero-length character vector for s_attribute ",
        s_attr,
        ", this will result in a column V1 (V2, V3, ...)."
      )
    }
    dt <- data.table::dcast.data.table(
      .Object@stat, formula(paste(s_attribute, collapse = "~")),
      value.var = if (freq) "freq" else "count", fun.aggregate = sum, fill = 0L
    )
    
    if (isTRUE(fill)){
      s_attr_values <- s_attributes(source, s_attribute = s_attribute[1], unique = TRUE)
      dt <- dt[do.call(data.table, setNames(list(s_attr_values, s_attribute[1]), c(s_attribute[1], "key"))), on = s_attribute[1]]
      dt[is.na(dt)] <- 0L
      
      s_attr_values <- s_attributes(source, s_attribute = s_attribute[2], unique = TRUE)
      s_attr_values_missing <- s_attr_values[!s_attr_values %in% colnames(dt)]
      length_old <- length(dt)
      dt[, (s_attr_values_missing) := 0L]
      if (truelength(dt) > length_old + 10000L){
        warning(
          "Supplementary explanatory note on the data.table warning (issued by polmineR): ",
          "Augmenting the data.table with zeros for all corpus subsets that do not evoke query matches ", 
          sprintf("requires adding %d columns (with zeroes). This is more than data.table does without issuing ", length(s_attr_values_missing)),
          "the warning you see."
        )
      }
    }
    
  } else {
    warning("length(s_attribute) required to be either 1 or 2")
  }
  dt
})


#' @rdname dispersion-method
setMethod("dispersion", "remote_corpus", function(.Object, ...){
  ocpu_exec(fn = "dispersion", corpus = .Object@corpus, server = .Object@server, restricted = .Object@restricted, .Object = as(.Object, "corpus"), ...)
})

#' @rdname dispersion-method
setMethod("dispersion", "remote_subcorpus", function(.Object, ...){
  ocpu_exec(fn = "dispersion", corpus = .Object@corpus, server = .Object@server, restricted = .Object@restricted, .Object = as(.Object, "subcorpus"), ...)
})