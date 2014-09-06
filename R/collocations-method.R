setGeneric("collocations", function(object, ...){standardGeneric("collocations")})


#' calculate all collocations in a partition
#' 
#' the result is meant to serve as a result for an analysis of collocation graphs
#' 
#' @param object a partition object
#' @param pAttribute p-attribute, typically "word" or "token"
#' @param window no of tokens to the left and to the right
#' @param filter defaults to TRUE
#' @param posFilter what POS to keep
#' @param multicore whether to use multicore
#' @return a data frame
#' @exportMethod collocations
#' @docType methods
#' @author Andreas Blaette
#' @export collocations
#' @name collocations
#' @rdname collocations-method
#' @aliases collocations collocations-method collocations,partition-method
setMethod("collocations", "partition", function(object, pAttribute="word", window=5, filter=TRUE, posFilter=c("ADJA", "NN"), multicore=FALSE){
  tokenAttr <- paste(object@corpus,".",pAttribute, sep="")
  posAttr <- paste(object@corpus,".pos", sep="")
  getIdsWindow <- function(x, window, cposMax, ids, pos){
    j <- c((x-window):(x-1), (x+1):(x+window))
    j <- j[which(j > 0)]
    j <- j[which(j <= cposMax)]
    id <- ids[j]
    names(id) <- pos[j]
    id
  }
  getNodeIds <- function(x, neighbours, ids, pos) {
    v <- rep(ids[x], times=length(neighbours[[x]]))
    names(v) <- rep(pos[x], times=length(neighbours[[x]]))
    v
  } 
  movingContext <- function (cposRow, window, object, tokenAttr, posAttr) {
    bag <- list()
    cposRange <- c(object@cpos[cposRow,1]:object@cpos[cposRow,2])
    ids <- cqi_cpos2id(tokenAttr, cposRange)
    pos <- cqi_cpos2id(posAttr,cposRange)
    neighbours <- lapply(c(1:length(cposRange)), function(x) getIdsWindow(x,window,length(cposRange), ids, pos))
    bag[['nodes']] <- unlist(lapply(c(1:length(cposRange)),
                                    function(x) getNodeIds(x, neighbours, ids, pos)))
    bag[['neighbourhood']] <- unlist(neighbours)
    bag
  }
  message('... creating window lists')
  if (multicore==FALSE){
    bag <- lapply(c(1:nrow(object@cpos)), function(cposRow) {b <- movingContext(cposRow, window, object, tokenAttr, posAttr)})
  } else {
    bag <- mclapply(c(1:nrow(object@cpos)), function(cposRow) {b <- movingContext(cposRow, window, object, tokenAttr, posAttr)})
  }
  nodes <- lapply(bag, function(x) x$nodes)
  neighbourhood <- lapply(bag, function(x) x$neighbourhood)
  idFrame <- data.frame(
    nodeId=unname(unlist(nodes)),
    nodePos=as.integer(names(unlist(nodes))),
    tokenId=unname(unlist(neighbourhood)),
    podId=as.integer(names(unlist(neighbourhood)))
  )
  idFrameSelect <- idFrame[which(idFrame[,2] %in% cqi_str2id(posAttr, posFilter)),]
  idFrameSelect <- idFrameSelect[which(idFrameSelect[,4] %in% cqi_str2id(posAttr, posFilter)),]
  message('... pre-sorting for frequency count')
  frameSplit <- split(idFrameSelect[,1], idFrameSelect[,3])
  message('... now for the actual frequency count')
  if (multicore==FALSE){
    raw <- lapply(frameSplit, table)
  } else {
    raw <- mclapply(frameSplit, table)
  }
  message('... re-arrange data')
  nodeId <- unlist(lapply(names(raw), function(x) rep(as.numeric(x), times=length(raw[[x]]))))
  collocateId <- unlist(lapply(raw, function(x) as.numeric(names(x))))
  collocateWindowFreq <- unlist(lapply(raw, function(x) unname(x)))
  windowSize <- unlist(lapply(raw, function(x) rep(sum(x), times=length(x))))
  message('... g2-Test')
  calc <- cbind(nodeId, .g2Statistic(collocateId, collocateWindowFreq, windowSize, object, pAttribute))
  tab <- data.frame(node=cqi_id2str(tokenAttr, calc[,1]),
                    collocate=cqi_id2str(tokenAttr, calc[,2]),
                    calc)
  tab[,1] <- as.character(tab[,1])
  tab[,2] <- as.character(tab[,2])
  Encoding(tab[,1]) <- object@encoding
  Encoding(tab[,2]) <- object@encoding
  tab <- tab[order(tab[,9], decreasing=T),]
  tab
})

