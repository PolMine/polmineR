setGeneric("egoNetwork", function(object, ...){standardGeneric("egoNetwork")})


#' Prepare data for an ego-network
#' 
#' For a node word, cooccurrences of n degrees are calculated
#' 
#' The function returns a data frame that can be converted into an igraph object easily.
#' This conversion is not part of the function to keep number of dependencies of the 
#' package low.
#' 
#' @param node query, which may by a multi-word unit
#' @param object a partition object
#' @param degrees the degrees of the resulting egoNetwork
#' @param pAttribute p-attribute of the query
#' @param left no of tokens and to the left of the node word
#' @param right no of tokens to the right of the node word
#' @param minSignificance minimum log-likelihood value
#' @param posFilter character vector with the POS tags to be included - may not be empty!!
#' @return a data frame that can be turned into an igraph object with graph.data.frame (see example)
#' @exportMethod egoNetwork
#' @aliases egoNetwork egoNetwork,partition-method
#' @examples
#' \dontrun{
#'  nw <- partition(list(text_year="2005", text_type="speech"), corpus="PLPRNWHTM")
#'  net <- egoNetwork(nw, "Integration", 1, "word", 5,5, 3.84, "NN")
#'  g <- graph.data.frame(net[,c(1,2,3,7)])
#'  tklplot(g)
#'  }
#' @author Andreas Blaette
#' @docType methods
#' @export egoNetwork
setMethod("egoNetwork", "partition", function(object, node, degrees, pAttribute="useControls", left=0, right=0, minSignificance, posFilter=c()) {
  gData <- context(object, node, pAttribute, left, right, minSignificance, posFilter)@stat
  gData <- cbind(node=rep(node, times=nrow(gData)), target=rownames(gData), degree=rep(1, times=nrow(gData)), gData)
  rownames(gData) <- NULL
  for ( degree in 2:degrees ) {
    terms <- gData[which(gData$degree==(degree-1)),2]
    for ( term in terms ) {
      dataNew <- context(object, term, pAttribute, left, right, minSignificance, posFilter)@stat
      dataNew <- cbind(node=rep(term, times=nrow(dataNew)), target=rownames(dataNew), degree=degree, dataNew)
      rownames(dataNew) <- NULL
      gData <- rbind(gData, dataNew)
    }
  }
  vertices <- unique(c(as.vector(unname(unlist(gData[,1]))), as.vector(unname(unlist(gData[,2])))))
  # verticeData <- data.frame(vertices=vertices, object@tf[vertices,"count"])
  gData
})

