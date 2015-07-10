setGeneric("score", function(object, ...){standardGeneric("score")})


#' score partitions in a bundle
#' 
#' Assign scores to the partitions in a partitionBundle object.
#' 
#' @param object a partitionBundle object
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
setMethod("score", "partitionBundle", function(object, token, method, pAttribute, rel=FALSE, tfMethod=FALSE){
  if (is.null(names(object@objects))) warning("names needed but missing")
  if ((length(unique(names(object@objects)))) != length(object@objects)) warning("please ensure that are names are present and unique")
  if (method == "additive"){
    how <- ifelse(tfMethod==FALSE, "in", "grep")
    tab <- tf(object=object, token=token, pAttribute=pAttribute, rel=rel, method=tfMethod)
    score <- rowSums(tab)
  }
  score
})

