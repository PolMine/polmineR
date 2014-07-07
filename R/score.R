#' score partitions in a cluster
#' 
#' Assign scores to the partitions in a partitionCluster object.
#' 
#' @param object a partitionCluster object
#' @param token a character vector supplying tokens to be looked up
#' @param method either "additive" or "vsm"
#' @param pAttribute the p-attribute the analysis is based on
#' @param rel logical, whether to use relative frequencies
#' @param tfMethod what method to use for term frequencies
#' @return a named numeric vector (names are the labels of the partitions in the cluster)
#' @exportMethod score
#' @rdname score-method
#' @aliases score score-method score,partitionCluster-method
#' @name score
setMethod("score", "partitionCluster", function(object, token, method, pAttribute, rel=FALSE, tfMethod=FALSE){
  if (is.null(names(object@partitions))) warning("labels needed but missing")
  if ((length(unique(names(object@partitions)))) != length(object@partitions)) warning("please ensure that are labels are present and unique")
  if (method == "additive"){
    how <- ifelse(tfMethod==FALSE, "in", "grep")
    tab <- tf(object=object, token=token, pAttribute=pAttribute, rel=rel, method=tfMethod)
    score <- rowSums(tab)
  }
  score
})

