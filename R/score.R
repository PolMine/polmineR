#' score partitions in a cluster
#' 
#' Assign scores to the partitions in a partitionCluster object.
#' 
#' @param object a partitionCluster object
#' @param query query a character vector supplying tokens to be looked up
#' @param method either "addedFreq" or "SVM"
#' @return a named numeric vector (names are the labels of the partitions in the cluster)
#' @exportMethod score
#' @rdname score-method
#' @aliases score score-method score,partitionCluster-method
#' @name score
setMethod("score", "partitionCluster", function(object, query, method){
  if (is.null(names(object@partitions))) warning("labels needed but missing")
  if ((length(unique(names(object@partitions)))) != length(object@partitions)) warning("please ensure that are labels are present and unique")
})