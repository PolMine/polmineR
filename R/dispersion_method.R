#' @include partition_class.R hits_class.R
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
#' @param sAttribute a character vector of length 1 or 2 providing the sAttributes 
#' @param pAttribute the p-attribute that will be looked up, typically 'word'
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
#' \dontrun{
#'   use("polmineR.sampleCorpus")
#'   test <- partition("PLPRBTTXT", text_year = "2009", pAttribute = NULL)
#'   integration <- dispersion(
#'     test, query = "Integration",
#'     pAttribute = "word", sAttribute = "text_date"
#'     )
#'   integration <- dispersion(
#'     test, "Integration",
#'     sAttribute = c("text_date", "text_party")
#'     )
#'   integration <- dispersion(
#'     test, '"Integration.*"',
#'     sAttribute = "text_year", cqp = TRUE
#'     )
#' }
#' @seealso count
#' @author Andreas Blaette
#' @docType methods
#' @exportMethod dispersion
#' @rdname dispersion-method
#' @name dispersion
setMethod("dispersion", "partition", function(.Object, query, sAttribute, cqp = FALSE, pAttribute = getOption("polmineR.pAttribute"), freq = FALSE, mc=FALSE, progress = TRUE, verbose = FALSE){
  dispersion(
    hits(
      .Object = .Object, query = query, cqp = cqp,
      sAttribute = sAttribute, pAttribute = pAttribute, freq = freq,
      mc = mc, verbose = verbose, progress = progress
      ),
    sAttribute = sAttribute, freq = freq
  )
})

#' @rdname dispersion-method
setMethod("dispersion", "character", function(.Object, query, sAttribute, cqp = FALSE, pAttribute=getOption("polmineR.pAttribute"), freq=FALSE, mc=FALSE, progress=TRUE, verbose=TRUE){
  dispersion(
    hits(
      .Object, query = query, cqp = cqp, sAttribute = sAttribute, pAttribute = pAttribute, freq = freq,
      mc = mc, verbose = verbose, progress = progress
    ),
    sAttribute=sAttribute, freq=freq
  )
})


#' @rdname dispersion-method
setMethod("dispersion", "hits", function(.Object, sAttribute, freq = FALSE){
  if (length(sAttribute) == 2){
    retval <- data.table::dcast.data.table(
      .Object@dt, formula(paste(sAttribute, collapse="~")),
      value.var = ifelse(freq == TRUE, "freq", "count"), fun.aggregate = sum, fill = 0
      )  
  } else if (length(sAttribute) == 1){
    if (freq == FALSE){
      sumup <- function(.SD) sum(.SD[["count"]])
      retval <- .Object@dt[, sumup(.SD), by = c(sAttribute), with = TRUE]
      data.table::setnames(retval, old = "V1", new = "count")
    } else {
      stop("not implemented for freq = TRUE")
    }
  } else {
    warning("length(sAttribute) needs to be 1 or 2")
  }
  retval
})
