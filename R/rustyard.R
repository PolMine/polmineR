# #' Size of sub-partitions
# #' @param Partition a partition object
# #' @param rows what to find in rows
# #' @param cols what to find in cols
# #' @noRd
# .crosstabulationSizes <- function(Partition, rows, cols){
#   strucSize= Partition@cpos[,2] - Partition@cpos[,1] + 1
#   sAttrRows <- paste(Partition@corpus,'.', rows, sep='')
#   sAttrCols <- paste(Partition@corpus,'.', cols, sep='')
#   tab <- data.frame(
#     strucSize,
#     rows=Partition@metadata$table[,rows],
#     cols=Partition@metadata$table[,cols]
#   )
#   ctab <- xtabs(strucSize~rows+cols, data=tab)
#   ctab <- data.frame(as.matrix(unclass(ctab)))
#   colnames(ctab)[which(colnames(ctab)=="NA.")] <- "NA"
#   rownames(ctab)[which(colnames(ctab)=="NA.")] <- "NA"
#   ctab
# }


# .map <- function(tableToMatch, tableToAdjust) {
#   colnames(tableToMatch) <- sub('X(\\d)', '\\1', colnames(tableToMatch))
#   rownames(tableToAdjust)[which(rownames(tableToAdjust)=="")] <- 'NA'
#   rownames(tableToMatch)[which(rownames(tableToMatch)=="")] <- 'NA'
#   colnames(tableToAdjust)[which(colnames(tableToAdjust)=="V1")] <- 'NA'
#   colnames(tableToMatch)[which(colnames(tableToMatch)=="V1")] <- 'NA'  
#   tableToAdjust <- merge(t(tableToMatch), t(tableToAdjust), by.x="row.names", by.y="row.names", all.x=TRUE, all.y=TRUE)
#   tableToAdjust <- tableToAdjust[,(nrow(tableToMatch)+2):ncol(tableToAdjust)]
#   tableToAdjust <- t(tableToAdjust)
#   rownames(tableToAdjust) <- sub('(.*?)\\.y', '\\1', rownames(tableToAdjust))
#   tableToAdjust <- merge(tableToMatch, tableToAdjust, by.x="row.names", by.y="row.names", all.x=TRUE, all.y=TRUE)
#   tableToAdjust <- tableToAdjust[,grep('V\\d+', colnames(tableToAdjust))]
#   dimnames(tableToAdjust) <- dimnames(tableToMatch)
#   colnames(tableToAdjust) <- gsub('^X(.*?)', '\\1', colnames(tableToAdjust))
#   tableToAdjust
# }
# 

# #' @docType methods
# #' @exportMethod trim
# #' @rdname trim-method
# setMethod("trim", "partition", function(object, pAttribute, minFrequency=0, posFilter=NULL,  tokenFilter=NULL, ...){
#   rework <- object
#   if (length(pAttribute) > 1) warning("taking only one pAttribute at a time")
#   message("Trimming partition ", rework@name)
#   if (!is.null(posFilter)) {
#     if (! pAttribute %in% names(object@pos) ){
#       message("... pos need to be added first")
#       rework <- addPos(rework, pAttribute)
#     }
#     rework@pos[[pAttribute]] <- rework@pos[[pAttribute]][rework@pos[[pAttribute]] %in% posFilter]
#     rework@tf <- rework@tf[rownames(rework@tf) %in% names(rework@pos[[pAttribute]]),]
#   }
#   if (minFrequency > 0){
#     rework@tf <- rework@tf[which(rework@tf[,"tf"] >= minFrequency),]
#     rework@pos[[pAttribute]] <- rework@pos[[pAttribute]][names(rework@pos[[pAttribute]]) %in% rownames(rework@tf)]
#   }
#   if(!is.null(tokenFilter)) {
#     tokenFilter <- .adjustEncoding(tokenFilter, rework@encoding)
#     rework@tf <- rework@tf[which(rownames(rework@tf) %in% tokenFilter),]
#   }
#   rework 
# })
# 

# setMethod("cooccurrences", "partition", function(
#   object, pAttribute="word", window=5, method="ll",
#   pos=c("ADJA", "NN"), progress=TRUE, mc=FALSE, verbose=TRUE
# ){
#   if (!pAttribute %in% object@pAttribute) object <- enrich(object, tf=pAttribute)
#   if (mc == TRUE) noCores <- slot(get('session', '.GlobalEnv'), "cores")
#   coll <- new(
#     "cooccurrences",
#     pAttribute=pAttribute, pos=pos,
#     leftContext=window, rightContext=window,
#     corpus=object@corpus, encoding=object@encoding,
#     partitionSize=object@size, stat=data.table()
#   )
#   coll@call <- deparse(match.call())
#   coll@partition <- strsplit(deparse(sys.call(-1)), "\\(|\\)|,")[[1]][2]
#   tokenAttr <- paste(object@corpus,".",pAttribute, sep="")
#   posAttr <- paste(object@corpus,".pos", sep="")
#   .getIdsWindow <- function(x, window, cposMax, ids, pos){
#     j <- c((x-window):(x-1), (x+1):(x+window))
#     j <- j[which(j > 0)]
#     j <- j[which(j <= cposMax)]
#     list(id=ids[j], pos=pos[j])
#   }
#   .getNodeIds <- function(x, noNeighbours, ids, pos) {
#     list(
#       id=rep(ids[x], times=noNeighbours[x]),
#       pos=rep(pos[x], times=noNeighbours[x])
#     )
#   } 
#   .movingContext <- function (cposMin, cposMax, window, object, tokenAttr, posAttr) {
#     if (cposMin != cposMax){
#       cpos <- c(cposMin:cposMax)
#       ids <- cqi_cpos2id(tokenAttr, cpos)
#       pos <- cqi_cpos2id(posAttr, cpos)
#       neighbourhood <- lapply(
#         c(1:length(cpos)),
#         function(x) .getIdsWindow(x, window, length(cpos), ids, pos)
#       )
#       noNeighbours <- sapply(neighbourhood, function(x) length(x[["id"]]))
#       nodes <- lapply(
#         c(1:length(cpos)),
#         function(x) .getNodeIds(x, noNeighbours, ids, pos)
#       )
#       retval <- list(
#         contextIds=unlist(lapply(neighbourhood, function(x) x[["id"]])),
#         contextPos=unlist(lapply(neighbourhood, function(x) x[["pos"]])),
#         nodeIds=unlist(lapply(nodes, function(x) x[["id"]])),
#         nodePos=unlist(lapply(nodes, function(x) x[["pos"]]))
#       )
#     } else {
#       retval <- NULL
#     }
#     retval 
#   }
#   if (verbose == TRUE) message('... performing frequency counts')
#   if (mc == FALSE){
#     bag <- lapply(c(1:nrow(object@cpos)), function(i) {
#       if (progress==TRUE) .progressBar(i=i, total=nrow(object@cpos))
#       b <- .movingContext(
#         cposMin=object@cpos[i,1], cposMax=object@cpos[i,2],
#         window, object, tokenAttr, posAttr
#       )
#     })
#   } else {
#     bag <- mclapply(
#       c(1:nrow(object@cpos)),
#       function(i) {b <- .movingContext(
#         cposMin=object@cpos[i,1], object@cpos[i,2], window, object, tokenAttr, posAttr
#       )},
#       mc.cores=noCores
#     )
#   }
#   # nodes <- lapply(bag, function(x) x$nodes)
#   # neighbourhood <- lapply(bag, function(x) x$neighbourhood)
#   if (verbose == TRUE) message("... aggregating and trimming counts")
#   idFrame <- data.table(
#     nodeId=unlist(lapply(bag, function(x) x[["nodeIds"]])),
#     nodePos=unlist(lapply(bag, function(x) x[["nodePos"]])),
#     tokenId=unlist(lapply(bag, function(x) x[["contextIds"]])),
#     tokenPos=unlist(lapply(bag, function(x) x[["contextPos"]]))
#   )
#   idFrameSelect <- idFrame[which(idFrame[["nodePos"]] %in% cqi_str2id(posAttr, pos)),]
#   idFrameSelect <- idFrameSelect[which(idFrameSelect[["tokenPos"]] %in% cqi_str2id(posAttr, pos)),]
#   message('... pre-sorting for frequency count')
#   frameSplit <- split(idFrameSelect[["tokenId"]], idFrameSelect[["nodeId"]])
#   frameSplitUnfiltered <- split(idFrame[["tokenId"]], idFrame[["nodeId"]])
#   message('... now for the actual frequency count')
#   if (mc==FALSE){
#     raw <- lapply(frameSplit, table)
#   } else {
#     raw <- mclapply(frameSplit, table, mc.cores=noCores)
#   }
#   message('... preparing stat table')
#   coll@stat <- data.table(
#     nodeId=unlist(lapply(names(raw), function(x) rep(as.numeric(x), times=length(raw[[x]])))),
#     cooccurrenceId=unlist(lapply(raw, function(x) as.numeric(names(x)))),
#     cooccurrenceWindowFreq=unlist(lapply(raw, function(x) unname(x))),
#     windowSize=unlist(lapply(names(raw), function(i) {rep(length(frameSplitUnfiltered[[i]]), times=length(raw[[i]])) }))
#   )
#   coll@stat[, cooccurrenceCorpusFreq := object@stat[match(coll@stat[["cooccurrenceId"]], object@stat[["ids"]]), "tf", with=FALSE] ]
#   coll@stat[, nodeCorpusFreq := object@stat[match(coll@stat[["nodeId"]], object@stat[["ids"]]), "tf", with=FALSE] ]
#   # coll@stat[, nodeTf := object@stat[coll@stat[["nodeId"]], "tf", with=FALSE]]
#   nodeStr <- cqi_id2str(tokenAttr, coll@stat[["nodeId"]]) %>% as.vector %>% as.utf8
#   coll@stat[, node := nodeStr]
#   cooc <- cqi_id2str(tokenAttr, coll@stat[["cooccurrenceId"]]) %>% as.vector %>% as.utf8
#   coll@stat[, cooccurrence := cooc]
#   if ("ll" %in% method) {
#     message('... g2-Test')
#     coll <- ll(coll, partitionSize=object@size)
#     coll@stat <- setorderv(coll@stat, cols="ll", order=-1)
#     coll@stat[, rank := c(1:nrow(coll@stat))]
#   }
#   coll@stat[, relation := paste(coll@stat[["node"]], "->", coll@stat[["cooccurrence"]], sep="")]
#   #   setcolorder(coll@stat, c(
#   #     "rank", "relation", 
#   #     "node", "nodeId", "nodeCorpusFreq",
#   #     "cooccurrence", "cooccurrenceId", "cooccurrenceWindowFreq", "cooccurrenceCorpusFreq",
#   #     "windowSize", "expCorpus", "expCoi", "ll")
#   #     )
#   #   coll
# })
# 
