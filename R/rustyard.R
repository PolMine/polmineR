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
#   #     "windowSize", "exp_b", "exp_a", "ll")
#   #     )
#   #   coll
# })
# 



# #' @include partition_class.R partitionBundle_class.R keyness_class.R
# NULL
# 
# #' add pos information
# #' 
# #' Add information on part-of-speech tags of tokens to tokens. The method is 
# #' available for objects of the classes 'partition', 'partitionBundle' and
# #' 'keyness' respectively. 
# #' \code{method?addPos("partitionBundle")}, \code{method?addPos("keyness")}).
# #' @param object either a partition, a partitionBundle or a keyness object
# #' @param ... further arguments
# #' @return the original, enhanced object
# #' @docType methods
# #' @rdname addPos-method
# #' @name addPos
# #' @aliases addPos addPos-method
# setGeneric("addPos", function(object,...){standardGeneric("addPos")})
# 
# 
# #' Add POS tags
# #' 
# #' Add the POS tags to a table with tokens in the rows
# #' 
# #' The POS tags that occur for a given token are counted. The POS tag with the
# #' highest share is added to the table
# #' 
# #' @param object a context object
# #' @return object with pimped stat table
# #' @author Andreas Blaette
# #' @noRd
# .addPos <- function(object) {
#   ids = cqi_str2id(paste(object@corpus, ".", object@pAttribute, sep=""), rownames(object@stat))
#   posIds <- unlist(mclapply(ids, function (x){
#     idPos <- cqi_cpos2id(paste(object@corpus, ".pos", sep=""), cqi_id2cpos(paste(object@corpus, ".", object@pAttribute, sep=""), x))
#     posIdFrequencies <- tabulate(idPos+1)
#     mostFrequent <- which.max(posIdFrequencies) - 1
#     return(mostFrequent)
#   }))
#   pos <- cqi_id2str(paste(object@corpus, ".pos", sep=""), posIds)
#   object@stat <- cbind(object@stat, pos=pos)
#   object
# }
# 
# 
# #' Fill slot 'pos' of partition (or partitionBundle) object
# #' 
# #' The 'pos'-slot of the partition (or partitionBundle) object is filled with tables
# #' providing a statistic on the frequency of a pos-tag of a token
# #' in the partition.
# #' 
# #' @param pAttribute character vector (typically c("word") or c("lemma") or c("word", "lemma"))
# #' @return an augmented partition or partitionBundle object (includes pos now)
# #' @author Andreas Blaette
# #' @exportMethod addPos
# #' @docType methods
# #' @rdname addPos-method
# setMethod("addPos", "partition", function(object, pAttribute){
#   if (length(pAttribute) > 1) warning("taking only one pAttribute at a time")
#   message("Adding pos information to partition object ", object@name)
#   cpos <- unlist(apply(object@cpos, 1, function(x) c(x[1]:x[2])))
#   message("... retrieving corpus information")
#   bag <- data.frame(
#     token=cqi_cpos2id(paste(object@corpus, '.', pAttribute, sep=''), cpos),
#     pos=cqi_cpos2id(paste(object@corpus, '.pos', sep=''), cpos)
#   )
#   message("... doing the calculations")
#   object@pos[[pAttribute]] <- list()
#   crosstab <- table(bag)
#   rownames(crosstab) <- cqi_id2str(paste(object@corpus, '.', pAttribute, sep=''), as.integer(rownames(crosstab)))
#   colnames(crosstab) <- cqi_id2str(paste(object@corpus, '.pos', sep=''), as.integer(colnames(crosstab)))
#   object@pos[[pAttribute]] <- apply(crosstab, 1, function(x) colnames(crosstab)[which.max(x)])
#   Encoding(names(object@pos[[pAttribute]])) <- object@encoding
#   # to make sure that there are no superfluous pos information
#   object@pos[[pAttribute]] <- object@pos[[pAttribute]][names(object@pos[[pAttribute]]) %in% rownames(object@stat)]
#   object
# })
# 
# #' @rdname addPos-method
# setMethod("addPos", "partitionBundle", function(object, pAttribute){
#   pimpedBundle <- object
#   if (slot(get('session', '.GlobalEnv'), "multicore") == TRUE) {
#     pimpedBundle@objects <- mclapply(object@objects, function(x) addPos(x, pAttribute))
#   } else {
#     pimpedBundle@objects <- lapply(object@objects, function(x) addPos(x, pAttribute))    
#   }
#   pimpedBundle
# })
# 
# #' supplement keyness object with pos information
# #' 
# #' A keyness object will be supplemented with pos information. The working of the
# #' method is potentially slow. It is recommended to trim the object first, before
# #' adding pos information.
# #' 
# #' @param Partition a partition object (the corpus of interest)
# #' @return an enhanced keyness object 
# #' @docType methods
# #' @rdname addPos-method
# setMethod("addPos", "keyness",
#           function(object, Partition=NULL){
#             if (is.null(Partition)){
#               object <- .addPos(object)
#             } else if (class(Partition) == "partition"){
#               if (object@pAttribute %in% names(Partition@pos)) {
#                 pos <- vapply(
#                   rownames(object@stat[1:50, ]),
#                   function(x) return(Partition@pos[[object@pAttribute]][["max"]][x]),
#                   USE.NAMES=FALSE,
#                   FUN.VALUE="character")
#                 object@stat <- cbind(object@stat, pos=pos)
#               }
#             }
#             object 
#           }
# )
# 


# #' @aliases trim,context-method
# #' @docType methods
# #' @rdname trim-method
# setMethod("trim", "textstat", function(object, min=list(), max=list(), drop=list(), keep=list()){
#   if (length(min) > 0){
#     stopifnot(all((names(min) %in% colnames(object@stat)))) # ensure that colnames provided are actually available
#     rowsToKeep <- as.vector(unique(sapply(
#       names(min),
#       function(column) which(object@stat[[column]] >= min[[column]])
#     )))
#     if (length(rowsToKeep) > 0) object@stat <- object@stat[rowsToKeep,]
#   }
#   if (length(max) > 0){
#     stopifnot(all((names(max) %in% colnames(object@stat)))) # ensure that colnames provided are actually available
#     rowsToKeep <- as.vector(unique(sapply(
#       names(max),
#       function(column) which(object@stat[[column]] <= max[[column]])
#     )))
#     if (length(rowsToDrop) > 0) object@stat <- object@stat[-rowsToKeep,]
#   }
#   if (length(drop) > 0){
#     stopifnot(all((names(drop) %in% colnames(object@stat))))
#     rowsToDrop <- as.vector(unlist(sapply(
#       names(drop),
#       function(column) sapply(drop[[column]], function(x) grep(x, object@stat[[column]]))
#     )))
#     if (length(rowsToDrop) > 0) object@stat <- object@stat[-rowsToDrop,]
#   }
#   if (length(keep) > 0){
#     stopifnot(all((names(keep) %in% colnames(object@stat))))
#     for (col in names(keep)){
#       object@stat <- object@stat[which(object@stat[[col]] %in% keep[[col]]),]
#     }
#   }
#   object
# })

# .g2Statistic <- function(ids, windowFreq, corpusFreq, windows.total, corpus.total){
#   calc <- matrix(data=0, nrow=length(windowFreq), ncol=6)
#   colnames(calc) <- c("id", "count_a", "count_b", "exp_a", "exp_b", "ll")
#   calc[,"id"] <- ids
#   calc[,"count_a"] <- windowFreq
#   calc[,"count_b"] <- corpusFreq
#   calc[,"exp_a"] <- windows.total*calc[,"count_b"]/corpus.total
#   calc[,"exp_b"] <- (corpus.total-windows.total)*calc[,"count_b"]/corpus.total
#   calc[,"ll"] <- 2*(calc[,"count_a"]*log(calc[,"count_a"]/calc[,"exp_a"])+((calc[,"count_b"]-calc[,"exp_a"])*log((calc[,"count_b"]-calc[,"exp_a"])/calc[,"exp_b"])))
#   calc
# }



# #' Transform a context bundle into a Term Context Matrix
# #' 
# #' Method based on the tm package, adds to as.TermDocumentMatrix
# #' 
# #' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
# #' 
# #' @param x a contextBundle object (S3 class)
# #' @param col the col of the stat table to take
# #' @param ... to make the check happy
# #' @method as.TermContextMatrix contextBundle
# #' @return a TermContextMatrix
# #' @author Andreas Blaette
# #' @docType method
# #' @importFrom slam simple_triplet_matrix
# #' @exportMethod as.TermContextMatrix
# #' @noRd
# setMethod("as.TermContextMatrix", "contextBundle", function (x, col, ...) {
#   encoding <- unique(unlist(lapply(x@objects, function(c) c@encoding)))
#   corpus <- unique(unlist(lapply(x@objects, function(c) c@corpus)))
#   pAttribute <- unique(unlist(lapply(x@objects, function(c) c@pAttribute)))
#   pAttr <- paste(corpus, '.', pAttribute, sep='')
#   i <- unlist(lapply(x@objects, function(c) (cqi_str2id(pAttr, rownames(c@stat))+1)))
#   j <- unlist(lapply(c(1:length(x@objects)), function(m) {rep(m,times=nrow(x[[m]]@stat))}))
#   v <- unlist(lapply(x@objects, function(c) c@stat[,col]))
#   lexiconSize <- cqi_lexicon_size(pAttr)
#   mat <- simple_triplet_matrix(i=i, j=j, v=v,
#                                ncol=length(x@objects),
#                                nrow=lexiconSize+1,
#                                dimnames=list(
#                                  Terms=cqi_id2str(pAttr, c(0:lexiconSize)),
#                                  Docs=names(x@objects))
#   )
#   mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")
#   class(mat) <- c("TermContextMatrix", "TermDocumentMatrix", "simple_triplet_matrix")
#   mat
# })
# 

# 
# #' @method as.TermDocumentMatrix partitionBundle
# #' @importFrom slam simple_triplet_matrix
# #' @importFrom tm as.TermDocumentMatrix
# #' @rdname coerce-methods
# #' @docType methods
# setMethod("as.TermDocumentMatrix", "partitionBundle", function (x, weight=NULL, rmBlank=TRUE, verbose=TRUE, ...) {
#   encoding <- unique(unlist(lapply(x@objects, function(c) c@encoding)))
#   pAttribute <- unique(unlist(lapply(x@objects, function(x) x@pAttribute)))
#   if (length(pAttribute) != 1) warning("incoherence of pAttributes/tf")
#   corpus <- unique(unlist(lapply(x@objects, function(c) c@corpus)))
#   message("... putting together the matrix")
#   i <- as.integer(unname(unlist(lapply(x@objects,
#                                        function(c) {a <- c@stat[,1]
#                                        a <- a+1
#                                        a})
#   )))
#   j <- unlist(lapply(c(1:length(x@objects)), function(m) rep(m,times=nrow(x@objects[[m]]@stat))))
#   v <- as.integer(unlist(lapply(x@objects, function(c) c@stat[,2])))
#   attr <- paste(corpus, '.', pAttribute, sep='')
#   lexicon.size <- cqi_lexicon_size(attr)
#   mat <- simple_triplet_matrix(i=i, j=j, v=v,
#                                ncol=length(x@objects),
#                                nrow=lexicon.size+1,
#                                dimnames=list(
#                                  Terms=cqi_id2str(attr, c(0:lexicon.size)),
#                                  Docs=names(x@objects))
#   )
#   mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")  
#   class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
#   if (rmBlank == TRUE) mt <- .rmBlank(mat, verbose=verbose)
#   if (!is.null(weight)){
#     if (weight == "tfidf"){
#       message("... applying tf/idf as a weight")
#       mat <- weigh(mat, method="tfidf", corpusSizes=summary(x)$token)
#     } else if (weight == "rel"){
#       message("... computing relative frequencies")
#       mat <- weigh(mat, method="rel", corpusSizes=summary(x)$token)
#     }
#   }
#   mat
# })
# 




# #' @rdname kwic
# setMethod("kwic", "plprPartition", function(
#   .Object, query, leftContext=5, rightContext=5,
#   meta=NULL, pAttribute="word", neighbor=c(), verbose=TRUE
# ){
#   Partition <- new("partition")
#   for (x in slotNames(.Object)) slot(Partition, x) <- slot(.Object, x)
#   kwicObject <- kwic(
#     Partition, query=query, leftContext=leftContext,
#     rightContext=rightContext, meta=meta, pAttribute=pAttribute,
#     neighbor=neighbor, verbose=verbose
#   )
#   if (is.null(kwicObject)) {
#     message("... no hits for query")
#     return()
#   }
#   plprKwicObject <- as(kwicObject, "plprKwic")
#   plprKwicObject@sAttributes <- .Object@sAttributes
#   plprKwicObject@corpus <- .Object@corpus
#   plprKwicObject
# })
# 


# #' @docType methods
# setMethod('show', 'kwic', function(object){
#   .showKwicLine <- function(object, i){
#     metaoutput <- paste(as.vector(unname(unlist(object@table[i,c(1:length(object@metadata))]))), collapse=" | ")
#     Encoding(metaoutput) <- object@encoding
#     cat('[',metaoutput, '] ', sep='')
#     cat(paste(as.vector(unname(unlist(object@table[i,c((ncol(object@table)-2):ncol(object@table))]))), collapse=" * "), "\n\n")
#   }
#   sessionKwicNo <- slot(get("session", '.GlobalEnv'), 'kwicNo')
#   if (sessionKwicNo == 0 ) {
#     for (i in 1:nrow(object@table)) .showKwicLine(object, i)
#   } else if (sessionKwicNo > 0) {
#     if (nrow(object@table) <= sessionKwicNo) {
#       for (i in 1:nrow(object@table)) .showKwicLine(object, i)
#     } else {
#       chunks <- trunc(nrow(object@table)/sessionKwicNo)
#       for ( i in c(0:(chunks-1))) {
#         lines <- i*sessionKwicNo+c(1:sessionKwicNo)
#         cat ('---------- KWIC output', min(lines), 'to', max(lines), 'of', nrow(object@table),'----------\n\n')
#         for (j in lines) .showKwicLine(object, j)
#         cat("(press 'q' to quit or ENTER to continue)\n")
#         loopControl <- readline()
#         if (loopControl == "q") break
#       }
#       if ((chunks*sessionKwicNo < nrow(object@table)) && (loopControl != "q")){
#         cat ('---------- KWIC output', chunks*sessionKwicNo, 'to', nrow(object@table), 'of', nrow(object@table),'----------\n\n')
#         lines <- c((chunks*sessionKwicNo):nrow(object@table))
#         for (j in lines) .showKwicLine(object, j)
#       }
#     }
#   }    
# })

# #' @rdname kwic-class
# setClass("plprKwic", contains="kwic", slots=c(sAttributes="list", corpus="character"))
