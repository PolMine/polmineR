setGeneric("collocations", function(object, ...){standardGeneric("collocations")})


#' calculate all collocations in a partition
#' 
#' the result is meant to serve as a result for an analysis of collocation graphs
#' 
#' @param object a partition object
#' @param pAttribute p-attribute, typically "word" or "token"
#' @param window no of tokens to the left and to the right
#' @param method the statistical test to use 
#' @param filter defaults to TRUE
#' @param posFilter what POS to keep
#' @param mc whether to use multicore
#' @return a data frame
#' @exportMethod collocations
#' @docType methods
#' @author Andreas Blaette
#' @export collocations
#' @name collocations
#' @rdname collocations-method
#' @aliases collocations collocations-method collocations,partition-method collocations,partitionCluster-method
setMethod("collocations", "partition", function(object, pAttribute="word", window=5, method="ll", filter=TRUE, posFilter=c("ADJA", "NN"), mc=FALSE){
  if (!pAttribute %in% names(object@tf)){
    object <- enrich(object, tf=pAttribute)
  }
  coll <- new(
    "collocations",
    pAttribute=pAttribute, posFilter=posFilter,
    leftContext=window, rightContext=window,
    corpus=object@corpus, encoding=object@encoding
    )
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
  if (mc==FALSE){
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
  if (mc==FALSE){
    raw <- lapply(frameSplit, table)
  } else {
    raw <- mclapply(frameSplit, table)
  }
  message('... preparing stat table')
  coll@stat <- data.frame(
    nodeId=unlist(lapply(names(raw), function(x) rep(as.numeric(x), times=length(raw[[x]])))),
    collocateId=unlist(lapply(raw, function(x) as.numeric(names(x)))),
    collocateWindowFreq=unlist(lapply(raw, function(x) unname(x))),
    windowSize=unlist(lapply(raw, function(x) rep(sum(x), times=length(x))))
  )
  coll@stat$collocateCorpusFreq <- object@tf[[pAttribute]][match(coll@stat[,"collocateId"], object@tf[[pAttribute]][,1]),2]
  coll@stat <- data.frame(node=as.vector(cqi_id2str(tokenAttr, coll@stat[,"nodeId"])),
                    collocate=as.vector(cqi_id2str(tokenAttr, coll@stat[,"collocateId"])),
                    coll@stat, stringsAsFactors=FALSE)
  Encoding(coll@stat[,"node"]) <- object@encoding
  Encoding(coll@stat[,"collocate"]) <- object@encoding
  if ("ll" %in% method) {
    message('... g2-Test')
    coll <- ll(coll, partitionSize=object@size)
    coll@stat <- coll@stat[order(coll@stat[,"ll"], decreasing=TRUE),]
    coll@stat <- data.frame(rank=c(1:nrow(coll@stat)), coll@stat, stringsAsFactors=FALSE)
  }
  rownames(coll@stat) <- paste(coll@stat[,"node"], "->", coll@stat[,"collocate"], sep="")
  coll
})

setMethod("collocations", "partitionCluster", function(object, pAttribute="word", window=5, filter=TRUE, posFilter=c("ADJA", "NN"), mc=FALSE){
  cluster <- new(
    "collocationsCluster",
    encoding=unique(vapply(object@partitions, function(x) x@encoding, FUN.VALUE="character")),
    corpus=unique(vapply(object@partitions, function(x) x@corpus, FUN.VALUE="character"))
    )
  cluster@collocations <- lapply(
    setNames(object@partitions, names(object@partitions)),
    function(x) {
      message('Calculating collocations for partition ', x@label)
      collocations(x, pAttribute=pAttribute, window=window, filter=filter, posFilter)
    })
  cluster
})