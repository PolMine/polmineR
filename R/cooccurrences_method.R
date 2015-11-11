setGeneric("cooccurrences", function(object, ...){standardGeneric("cooccurrences")})


#' calculate all cooccurrences in a partition
#' 
#' the result is meant to serve as a result for an analysis of collocation graphs
#' 
#' @param object a partition object
#' @param pAttribute p-attribute, typically "word" or "token"
#' @param window no of tokens to the left and to the right
#' @param method the statistical test to use 
#' @param pos what POS to keep
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
setMethod("cooccurrences", "partition", function(
  object, pAttribute="word", window=5, method="ll",
  pos=c("ADJA", "NN"), progress=TRUE, mc=FALSE, verbose=TRUE
  ){
  if (!pAttribute %in% object@pAttribute) object <- enrich(object, tf=pAttribute)
  if (mc == TRUE) noCores <- slot(get('session', '.GlobalEnv'), "cores")
  coll <- new(
    "cooccurrences",
    pAttribute=pAttribute, pos=pos,
    leftContext=window, rightContext=window,
    corpus=object@corpus, encoding=object@encoding,
    partitionSize=object@size, stat=data.table()
    )
  coll@call <- deparse(match.call())
  coll@partition <- strsplit(deparse(sys.call(-1)), "\\(|\\)|,")[[1]][2]
  tokenAttr <- paste(object@corpus,".",pAttribute, sep="")
  posAttr <- paste(object@corpus,".pos", sep="")
  .getIdsWindow <- function(x, window, cposMax, ids, pos){
    j <- c((x-window):(x-1), (x+1):(x+window))
    j <- j[which(j > 0)]
    j <- j[which(j <= cposMax)]
    list(id=ids[j], pos=pos[j])
  }
  .getNodeIds <- function(x, noNeighbours, ids, pos) {
    list(
      id=rep(ids[x], times=noNeighbours[x]),
      pos=rep(pos[x], times=noNeighbours[x])
    )
  } 
  .movingContext <- function (cposMin, cposMax, window, object, tokenAttr, posAttr) {
    if (cposMin != cposMax){
      cpos <- c(cposMin:cposMax)
      ids <- cqi_cpos2id(tokenAttr, cpos)
      pos <- cqi_cpos2id(posAttr, cpos)
      neighbourhood <- lapply(
        c(1:length(cpos)),
        function(x) .getIdsWindow(x, window, length(cpos), ids, pos)
        )
      noNeighbours <- sapply(neighbourhood, function(x) length(x[["id"]]))
      nodes <- lapply(
        c(1:length(cpos)),
        function(x) .getNodeIds(x, noNeighbours, ids, pos)
      )
      retval <- list(
        contextIds=unlist(lapply(neighbourhood, function(x) x[["id"]])),
        contextPos=unlist(lapply(neighbourhood, function(x) x[["pos"]])),
        nodeIds=unlist(lapply(nodes, function(x) x[["id"]])),
        nodePos=unlist(lapply(nodes, function(x) x[["pos"]]))
      )
    } else {
      retval <- NULL
    }
    retval 
  }
  if (verbose == TRUE) message('... performing frequency counts')
  if (mc == FALSE){
    bag <- lapply(c(1:nrow(object@cpos)), function(i) {
      if (progress==TRUE) .progressBar(i=i, total=nrow(object@cpos))
      b <- .movingContext(
        cposMin=object@cpos[i,1], cposMax=object@cpos[i,2],
        window, object, tokenAttr, posAttr
        )
      })
  } else {
    bag <- mclapply(
      c(1:nrow(object@cpos)),
      function(i) {b <- .movingContext(
        cposMin=object@cpos[i,1], object@cpos[i,2], window, object, tokenAttr, posAttr
        )},
      mc.cores=noCores
      )
  }
  # nodes <- lapply(bag, function(x) x$nodes)
  # neighbourhood <- lapply(bag, function(x) x$neighbourhood)
  if (verbose == TRUE) message("... aggregating and trimming counts")
  idFrame <- data.table(
    nodeId=unlist(lapply(bag, function(x) x[["nodeIds"]])),
    nodePos=unlist(lapply(bag, function(x) x[["nodePos"]])),
    tokenId=unlist(lapply(bag, function(x) x[["contextIds"]])),
    tokenPos=unlist(lapply(bag, function(x) x[["contextPos"]]))
  )
  idFrameSelect <- idFrame[which(idFrame[["nodePos"]] %in% cqi_str2id(posAttr, pos)),]
  idFrameSelect <- idFrameSelect[which(idFrameSelect[["tokenPos"]] %in% cqi_str2id(posAttr, pos)),]
  message('... pre-sorting for frequency count')
  frameSplit <- split(idFrameSelect[["tokenId"]], idFrameSelect[["nodeId"]])
  frameSplitUnfiltered <- split(idFrame[["tokenId"]], idFrame[["nodeId"]])
  message('... now for the actual frequency count')
  if (mc==FALSE){
    raw <- lapply(frameSplit, table)
  } else {
    raw <- mclapply(frameSplit, table, mc.cores=noCores)
  }
  message('... preparing stat table')
  coll@stat <- data.table(
    nodeId=unlist(lapply(names(raw), function(x) rep(as.numeric(x), times=length(raw[[x]])))),
    cooccurrenceId=unlist(lapply(raw, function(x) as.numeric(names(x)))),
    cooccurrenceWindowFreq=unlist(lapply(raw, function(x) unname(x))),
    windowSize=unlist(lapply(names(raw), function(i) {rep(length(frameSplitUnfiltered[[i]]), times=length(raw[[i]])) }))
  )
  coll@stat[, cooccurrenceCorpusFreq := object@stat[match(coll@stat[["cooccurrenceId"]], object@stat[["ids"]]), "tf", with=FALSE] ]
  coll@stat[, nodeCorpusFreq := object@stat[match(coll@stat[["nodeId"]], object@stat[["ids"]]), "tf", with=FALSE] ]
  # coll@stat[, nodeTf := object@stat[coll@stat[["nodeId"]], "tf", with=FALSE]]
  nodeStr <- cqi_id2str(tokenAttr, coll@stat[["nodeId"]]) %>% as.vector %>% as.utf8
  coll@stat[, node := nodeStr]
  cooc <- cqi_id2str(tokenAttr, coll@stat[["cooccurrenceId"]]) %>% as.vector %>% as.utf8
  coll@stat[, cooccurrence := cooc]
  if ("ll" %in% method) {
    message('... g2-Test')
    coll <- ll(coll, partitionSize=object@size)
    coll@stat <- setorderv(coll@stat, cols="ll", order=-1)
    coll@stat[, rank := c(1:nrow(coll@stat))]
  }
  coll@stat[, relation := paste(coll@stat[["node"]], "->", coll@stat[["cooccurrence"]], sep="")]
  setcolorder(coll@stat, c(
    "rank", "relation", 
    "node", "nodeId", "nodeCorpusFreq",
    "cooccurrence", "cooccurrenceId", "cooccurrenceWindowFreq", "cooccurrenceCorpusFreq",
    "windowSize", "expCorpus", "expCoi", "ll")
    )
  coll
})

setMethod("cooccurrences", "partitionBundle", function(object, pAttribute="word", window=5, method="ll", filter=TRUE, pos=c("ADJA", "NN"), mc=FALSE){
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
        cooccurrences(x, pAttribute=pAttribute, window=window, method=method, filter=filter, pos=pos)
      })
    
  } else {
    bundle@objects <- mclapply(
      setNames(object@objects, names(object@objects)),
      function(x) {
        message('Calculating cooccurrences for partition ', x@name)
        cooccurrences(
          x, pAttribute=pAttribute, window=window, method=method, filter=filter, pos=pos, mc=FALSE, progress=FALSE
          )
      }, mc.cores=slot(get('session', '.GlobalEnv'), "cores"))    
  }
  bundle
})


cooc <- function(object, pAttribute="word", window=5, pos=TRUE, ...){
  if (identical(pAttribute, object@pAttribute) == FALSE) object <- enrich(object, tf=pAttribute)
  coll <- new(
    "cooccurrences",
    pAttribute=pAttribute, corpus=object@corpus, encoding=object@encoding, pos=pos,
    leftContext=window, rightContext=window, partitionSize=object@size, stat=data.table()
  )
  coll@call <- deparse(match.call())
  coll@partition <- strsplit(deparse(sys.call(-1)), "\\(|\\)|,")[[1]][2]
  
  tokenAttr <- paste(object@corpus,".",pAttribute[1], sep="")
  posAttr <- paste(object@corpus,".pos", sep="")
  posId <- cqi_str2id(posAttr, pos)
  .movingContext <- function (cposMin, cposMax, window) {
    if (cposMin != cposMax){
      retval <- lapply(
        setNames(c(cposMin:cposMax), c(cposMin:cposMax)),
        function(x) {
          cpos1 <- c((x-window):(x-1), (x+1):(x+window))
          cpos2 <- cpos1[which(cpos1 > cposMin)]
          cpos3 <- cpos2[which(cpos2 <= cposMax)]
          cpos3
        })
    } else {
      retval <- NULL
    }
    retval 
  }
  bag <- apply(
    object@cpos, 1, function(row) .movingContext(cposMin=row[1], cposMax=row[2], window)
  )
  DT <- data.table(
    cposContext=unlist(bag),
    cposNode=unlist(lapply(bag, function(x) lapply(names(x), function(y) rep(as.numeric(y), times=length(x[[y]])))))
  )
  DT[, contextId := cqi_cpos2id(tokenAttr, DT[["cposContext"]])]
  DT[, nodeId := cqi_cpos2id(tokenAttr, DT[["cposNode"]])]
  DT[, contextPos := cqi_cpos2id(posAttr, DT[["cposContext"]])]
  DT[, nodePos := cqi_cpos2id(posAttr, DT[["cposNode"]])]
  setkey(DT, nodeId)
#   .countFast <- function(contextId){
#     a <- table(contextId)
#     data.frame(
#       contextId=as.integer(names(a)),
#       count=as.integer(as.vector(a))
#     )
#   }
#   
  # fooFast <- DT[, .countFast(contextId), by=.(nodeId)]
  .count <- function(nodeId, nodePos, contextId, contextPos){
    chunks <- split(contextId, contextPos)
    tfNode <- lapply(chunks, length)
    chunks <- lapply(chunks, table)
    data.frame(
      cooccurrenceId=unname(unlist(lapply(chunks, function(chunk) as.integer(names(chunk))))),
      cooccurrenceWindowFreq=unname(unlist(lapply(chunks, function(chunk) as.vector(chunk)))),
      contextPos=unlist(lapply(names(chunks), function(id) rep(as.integer(id), times=length(chunks[[id]])))),
      nodePos=nodePos,
      windowSize=length(contextId)
      )
  }
  DTpos <- DT[nodePos %in% posId][contextPos %in% posId]
  idTab <- DTpos[, .count(nodeId, nodePos, contextId, contextPos), by=.(nodeId, nodePos)]
  idTab[, node := cqi_id2str(tokenAttr, idTab[["nodeId"]]) %>% as.vector %>% as.utf8]
  idTab[, posStr := cqi_id2str(posAttr, idTab[["nodePos"]]) %>% as.vector %>% as.utf8]
  idTab[, contextStr := cqi_id2str(tokenAttr, idTab[["cooccurrenceId"]]) %>% as.vector %>% as.utf8]
  idTab[, contextPos := cqi_id2str(posAttr, idTab[["contextPos"]]) %>% as.vector %>% as.utf8]
  idTab[, nodeKey := paste(idTab[["node"]], "//", idTab[["posStr"]], sep="")]
  # idTab[, idKey := paste(idTab[["nodeId"]], "//", idTab[["nodePos"]], sep="")]
  idTab[, cooccurrenceKey := paste(idTab[["contextStr"]], "//", idTab[["contextPos"]], sep="")]
  
  nodeTf <- DT[, nrow(.SD), by=.(nodeId, nodePos)]
  # nodeTf[, idKey := paste(nodeTf[["nodeId"]], "//", nodeTf[["nodePos"]], sep="")]
  setkey(nodeTf, nodeId, nodePos)
  setkey(idTab, nodeId, nodePos)
  idTab[, nodeFreq := nodeTf[idTab][["V1"]]]
  setkey(idTab, nodeKey)
  setkey(object@stat, token)
  idTab[, nodeCorpusFreq := object@stat[idTab][["tf"]]]
  setkey(idTab, cooccurrenceKey)
  idTab[ , cooccurrenceCorpusFreq := object@stat[idTab][["tf"]]]

  if ("ll" %in% method) {
    message('... g2-Test')
    coll <- ll(coll, partitionSize=object@size)
    coll@stat <- setorderv(coll@stat, cols="ll", order=-1)
    coll@stat[, rank := c(1:nrow(coll@stat))]
  }
  coll@stat[, relation := paste(coll@stat[["node"]], "->", coll@stat[["cooccurrence"]], sep="")]
  setcolorder(coll@stat, c(
    "rank", "relation", 
    "node", "nodeId", "nodeCorpusFreq",
    "cooccurrence", "cooccurrenceId", "cooccurrenceWindowFreq", "cooccurrenceCorpusFreq",
    "windowSize", "expCorpus", "expCoi", "ll")
  )
  coll
}