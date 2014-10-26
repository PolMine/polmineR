#' @include collocations-class.R
NULL

setGeneric("as.igraph", function(x, ...){UseMethod("as.igraph")})


#' @importFrom igraph as.igraph graph.data.frame
#' @exportMethod as.igraph
#' @noRd
setMethod("as.igraph", "collocations", function(x, edgeAttributes="ll", verticeAttributes="tf"){
  if (!all(edgeAttributes %in% colnames(x@stat))) warning("edgeAttribute supplied is not available")
  g <- graph.data.frame(x@stat[,c("node","collocate", edgeAttributes)])
  if ("tf" %in% verticeAttributes){
    tf <- get(x@partition, ".GlobalEnv")@tf[[x@pAttribute]] # this will be a data.frame
    tfVector <- setNames(tf[,"tf"], rownames(tf))
    V(g)$tf <- tfVector[V(g)]
  }
  return(g)
})

setMethod("as.igraph", "keyness", function(x, edgeAttributes="ll"){
  graph.data.frame(x@stat[,c("term1", "term2", edgeAttributes)])
})