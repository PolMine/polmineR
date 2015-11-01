setGeneric("cooccurrences", function(object, ...){standardGeneric("cooccurrences")})


#' calculate all cooccurrences in a partition
#' 
#' the result is meant to serve as a result for an analysis of collocation graphs
#' 
#' @param object a partition object
#' @param pAttribute p-attribute, typically "word" or "token"
#' @param window no of tokens to the left and to the right
#' @param method the statistical test to use 
#' @param filter defaults to TRUE
#' @param posFilter what POS to keep
#' @param progress logical, whether to show progress bar
#' @param mc whether to use multicore
#' @return a data frame
#' @exportMethod cooccurrences
#' @docType methods
#' @author Andreas Blaette
#' @export cooccurrences
#' @name cooccurrences
#' @rdname cooccurrences-method
#' @aliases cooccurrences cooccurrences-method cooccurrences,partition-method cooccurrences,partitionBundle-method
#' @examples
#' \dontrun{
#' bt17merkel <- partition("PLPRTXT", list(text_lp="17", text_type="speech", text_speaker="Angela Merkel"))
#' bt17merkelColl <- cooccurrences(bt17merkel)
#' }
setMethod("cooccurrences", "partition", function(object, pAttribute="word", window=5, method="ll", filter=TRUE, posFilter=c("ADJA", "NN"), progress=TRUE, mc=FALSE){
  if (!pAttribute %in% object@pAttribute) object <- enrich(object, tf=pAttribute)
  if (mc == TRUE) noCores <- slot(get('session', '.GlobalEnv'), "cores")
  coll <- new(
    "cooccurrences",
    pAttribute=pAttribute, posFilter=posFilter,
    leftContext=window, rightContext=window,
    corpus=object@corpus, encoding=object@encoding,
    partitionSize=object@size
    )
  coll@call <- deparse(match.call())
  coll@partition <- strsplit(deparse(sys.call(-1)), "\\(|\\)|,")[[1]][2]
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
  if (mc == FALSE){
    bag <- lapply(c(1:nrow(object@cpos)), function(cposRow) {
      if (progress==TRUE) .progressBar(i=cposRow, total=nrow(object@cpos))
      b <- movingContext(cposRow, window, object, tokenAttr, posAttr)
      })
  } else {
    bag <- mclapply(
      c(1:nrow(object@cpos)),
      function(cposRow) {b <- movingContext(cposRow, window, object, tokenAttr, posAttr)},
      mc.cores=noCores
      )
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
    raw <- mclapply(frameSplit, table, mc.cores=noCores)
  }
  message('... preparing stat table')
  coll@stat <- data.frame(
    nodeId=unlist(lapply(names(raw), function(x) rep(as.numeric(x), times=length(raw[[x]])))),
    cooccurrenceId=unlist(lapply(raw, function(x) as.numeric(names(x)))),
    cooccurrenceWindowFreq=unlist(lapply(raw, function(x) unname(x))),
    windowSize=unlist(lapply(raw, function(x) rep(sum(x), times=length(x))))
  )
  coll@stat$cooccurrenceCorpusFreq <- object@tf[match(coll@stat[,"cooccurrenceId"], object@tf[,1]),2]
  nodeStr <- as.vector(cqi_id2str(tokenAttr, coll@stat[,"nodeId"]))
  Encoding(nodeStr) <- object@encoding
  coll@stat <- data.frame(
    node=nodeStr,
    cooccurrence=as.vector(cqi_id2str(tokenAttr, coll@stat[,"cooccurrenceId"])),
    coll@stat, nodeTf=object@tf[nodeStr, "tf"],
    stringsAsFactors=FALSE
    )
  Encoding(coll@stat[,"cooccurrence"]) <- object@encoding
  if ("ll" %in% method) {
    message('... g2-Test')
    coll <- ll(coll, partitionSize=object@size)
    coll@stat <- coll@stat[order(coll@stat[,"ll"], decreasing=TRUE),]
    coll@stat <- data.frame(rank=c(1:nrow(coll@stat)), coll@stat, stringsAsFactors=FALSE)
  }
  rownames(coll@stat) <- paste(coll@stat[,"node"], "->", coll@stat[,"cooccurrence"], sep="")
  coll
})

setMethod("cooccurrences", "partitionBundle", function(object, pAttribute="word", window=5, method="ll", filter=TRUE, posFilter=c("ADJA", "NN"), mc=FALSE){
  bundle <- new(
    "cooccurrencesBundle",
    encoding=unique(vapply(object@objects, function(x) x@encoding, FUN.VALUE="character")),
    corpus=unique(vapply(object@objects, function(x) x@corpus, FUN.VALUE="character"))
    )
  if (mc == FALSE){
    bundle@objects <- lapply(
      setNames(object@objects, names(object@objects)),
      function(x) {
        message('Calculating cooccurrences for partition ', x@name)
        cooccurrences(x, pAttribute=pAttribute, window=window, method=method, filter=filter, posFilter=posFilter)
      })
    
  } else {
    bundle@objects <- mclapply(
      setNames(object@objects, names(object@objects)),
      function(x) {
        message('Calculating cooccurrences for partition ', x@name)
        cooccurrences(
          x, pAttribute=pAttribute, window=window, method=method, filter=filter, posFilter=posFilter, mc=FALSE, progress=FALSE
          )
      }, mc.cores=slot(get('session', '.GlobalEnv'), "cores"))    
  }
  bundle
})
