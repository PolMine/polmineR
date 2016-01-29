setGeneric("score", function(.Object, ...){standardGeneric("score")})


#' score partitions in a bundle
#' 
#' Assign scores to the partitions in a partitionBundle object.
#' 
#' @param .Object a partitionBundle object
#' @param token a character vector supplying tokens to be looked up
#' @param method either "additive" or "vsm"
#' @param pAttribute the p-attribute the analysis is based on
#' @param rel logical, whether to use relative frequencies
#' @param tfMethod what method to use for term frequencies
#' @return a named numeric vector (names of the partitions in the bundle)
#' @exportMethod score
#' @docType methods
#' @rdname score-method
#' @aliases score score-method score,partitionBundle-method
#' @name score
setMethod("score", "partitionBundle", function(.Object, by, pAttribute, method="count", freq=FALSE){
  if (is.null(names(.Object@objects))){
    stop("names needed but missing")
  }
  if ((length(unique(names(.Object@objects)))) != length(.Object@objects)){
    warning("please ensure that are names are present and unique")
  }
  if (method == "count"){
    tab <- count(.Object, query=by, pAttribute=pAttribute, freq=freq)
    score <- tab[, TOTAL := rowSums(.SD), by=partition]
    setnames(score, "V1", "score")
  }
  tab
})

