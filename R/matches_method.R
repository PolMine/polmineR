#' @rdname matches
setGeneric("matches", function(.Object, ...) standardGeneric("matches") )

#' Matches for queries. 
#'
#' If one query is supplied, the function returns a frequency breakdown of the results of the query.
#' If several queries are supplied, frequencies for the individual queries are retrieved.
#' 
#' @param query a query, CQP syntax may be used
#' @param .Object a partition object
#' @param pAttribute pAttribute
#' @param cqp either logical to indicate whether the query uses CQP syntax, or a function to determine
#' whether query is a CQP query (defaults to helper function \code{is.cqp})
#' @param ... further parameters
#' @return a data.table
#' @exportMethod matches
#' @docType methods
#' @rdname matches
#' @name matches
#' @examples 
#' \dontrun{
#' use("polmineR.sampleCorpus")
#' matches("PLPRBTTXT", '"Integration.*"')
#' 
#' P <- partition("PLPRBTTXT", text_date = "2009-11-11")
#' matches(P, '"Integration.*"')
#' }
#' @aliases matches,partition-method
#' @seealso \code{\link{hits-class}}
setMethod("matches", "partition", function(.Object, query, cqp = is.cqp, pAttribute = getOption("polmineR.pAttribute")){
  dts <- lapply(
    query,
    function(x){
      cposHits <- cpos(.Object = .Object, query = x, cqp = cqp, pAttribute = pAttribute)
      if (is.null(cposHits)) return( NULL )
      hitsString <- apply(
        cposHits, 1,
        function(x) paste(CQI$cpos2str(.Object@corpus, pAttribute, x[1]:x[2]), collapse = ' ')
      )
      result <- table(hitsString)
      dt <- data.table(query = x, match = names(result), count = as.vector(unname(result)))
      if (nrow(dt) > 0){
        Encoding(dt[["match"]]) <- .Object@encoding
        dt[["match"]] <- as.nativeEnc(dt[["match"]], from = .Object@encoding)
        setorderv(dt, cols = "count", order = -1L)
        dt[["share"]] <- round(dt[["count"]] / sum(dt[["count"]]) * 100, 2)
      } else {
        dt <- NULL
      }
      dt
    }
  )
  rbindlist(dts)
})

#' @rdname matches
setMethod("matches", "character", function(.Object, query, cqp = is.cqp, pAttribute = getOption("polmineR.pAttribute")){
  C <- Corpus$new(.Object)
  C$pAttribute <- pAttribute
  matches(.Object = C$as.partition(), query = query, cqp = cqp, pAttribute = pAttribute)
})
