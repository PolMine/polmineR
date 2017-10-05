#' @rdname matching
setGeneric("matching", function(.Object, ...) standardGeneric("matching") )

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
#' @exportMethod matching
#' @docType methods
#' @rdname matching
#' @name matching
#' @examples 
#' \dontrun{
#' use("polmineR.sampleCorpus")
#' matching("PLPRBTTXT", '"Integration.*"')
#' 
#' P <- partition("PLPRBTTXT", text_date = "2009-11-11")
#' matching(P, '"Integration.*"')
#' }
#' @aliases matching,partition-method
#' @seealso \code{\link{hits-class}}
setMethod("matching", "partition", function(.Object, query, cqp = is.cqp, pAttribute = getOption("polmineR.pAttribute")){
})

#' @rdname matching
setMethod("matching", "character", function(.Object, query, cqp = is.cqp, pAttribute = getOption("polmineR.pAttribute")){
  C <- Corpus$new(.Object)
  C$pAttribute <- pAttribute
  matching(.Object = C$as.partition(), query = query, cqp = cqp, pAttribute = pAttribute)
})
