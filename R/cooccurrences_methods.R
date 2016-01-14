#' @include cooccurrences_class.R generics.R
NULL

# setMethod("[", "cooccurrences", function(x,i){
#   setkeyv(x@stat, col=)
#   aTab <- subset(x@stat, node == i)
#   tab <- tab[, c("cooccurrenceWindowFreq", "cooccurrenceCorpusFreq", "exp_a", "exp_b", "ll")]
#   colnames(tab) <- c("cooccurrence", "count_a", "count_b", "exp_a", "exp_b", "ll")
#   return(tab)
# })

#' @rdname cooccurrences-class
setMethod("show", "cooccurrences", function(object){
  cat("Object of 'cooccurrences'-class\n")
  cat("Number of rows: ", nrow(object@stat), "\n")
})


#' @rdname cooccurrences-class
setMethod("summary", "cooccurrences", function(object){
  return(.statisticalSummary(object))
})

#' @rdname cooccurrences-class
#' @importFrom parallel mcparallel mccollect
setMethod("as.sparseMatrix", "cooccurrences", function(x, col, mc=FALSE){
  uniqueTerms <- unique(c(x@stat[,"node"], x@stat[,"cooccurrence"]))
  keyVector <- setNames(c(1:length(uniqueTerms)), uniqueTerms)
  splittedTab <- split(x=x@stat[,c(col, "cooccurrence")], f=x@stat[,"node"])
  system.time(
  if (mc==FALSE){
    bag <- list()
    bag[["i"]] <- unname(unlist(lapply(names(splittedTab), function(n) rep(keyVector[n], times=nrow(splittedTab[[n]]))))) #nodes
    bag[["j"]] <- unname(unlist(lapply(splittedTab, function(tab) keyVector[tab[,"cooccurrence"]]))) # cooccurrences
    bag[["x"]] <- unname(unlist(lapply(splittedTab, function(tab) tab[,col]))) # values
  } else if (mc==TRUE) {
    i <- mcparallel(unname(unlist(lapply(names(splittedTab), function(n) rep(keyVector[n], times=nrow(splittedTab[[n]])))))) #nodes
    j <- mcparallel(unname(unlist(lapply(splittedTab, function(tab) keyVector[tab[,"cooccurrence"]])))) # cooccurrences
    x <- mcparallel(unname(unlist(lapply(splittedTab, function(tab) tab[,col])))) # values
    bag <- mccollect(list(i,j,x), wait=TRUE)
    names(bag) <- c("i", "j", "x")
  }
  )
  retval <- sparseMatrix(
    i=bag[["i"]], j=bag[["j"]], x=bag[["x"]], 
    dims=c(length(uniqueTerms), length(uniqueTerms)),
    dimnames=list(names(keyVector), names(keyVector)),
    giveCsparse=TRUE
  )   
  return(retval)
})

setGeneric("compress", function(.Object, ...) standardGeneric("compress"))
# called by trim, cooccurrences-method

setMethod("compress", "cooccurrences", function(.Object, ...){
  DT <- copy(.Object@stat)
  aColsStr <- paste("a_", .Object@pAttribute, sep="")
  bColsStr <- paste("b_", .Object@pAttribute, sep="")
  KEY <- data.table(
    i=c(1:nrow(DT)),
    aKey=apply(DT, 1, function(x) paste(x[aColsStr], collapse="//")),
    bKey=apply(DT, 1, function(x) paste(x[bColsStr], collapse="//"))
  )
  # DT[, "order" := KEY[, order(c(aKey, bKey))[1], by=c("i")][["V1"]]]
  DT[, "order" := KEY[, order(c(.SD[["aKey"]][1], .SD[["bKey"]][1]))[1], by=c("i")][["V1"]]]
  setkey(DT, "order")
  aToB <- DT[list(1)]
  setkeyv(aToB, cols=c(aColsStr, bColsStr))
  bToA <- DT[list(2)]
  setnames(bToA, old=c(aColsStr, bColsStr), new=c(bColsStr, aColsStr))
  setkeyv(bToA, cols=c(aColsStr, bColsStr))
  merger <- merge(aToB, bToA, all.x=FALSE, all.y=TRUE)
  FIN <- merger[, c(aColsStr, bColsStr, "ab_count.x", "ll.x", "ll.y", "a_count.x", "b_count.x"), with=FALSE]
  setnames(
    FIN,
    c("ab_count.x", "ll.x", "ll.y", "a_count.x", "b_count.x"),
    c("ab_count", "ab_ll", "ba_ll", "a_count", "b_count"))
  setcolorder(FIN, c(aColsStr, bColsStr, "ab_count", "a_count", "b_count", "ab_ll", "ba_ll"))
  setkeyv(FIN, cols=c(aColsStr, bColsStr))
  retval <- new("cooccurrencesReshaped")
  for (x in (slotNames(.Object))) if (x != "stat") slot(retval, x) <- slot(.Object, x)
  retval@stat <- FIN
  return(retval)
})


# .reshapeCooccurrences <- function(object, mc=TRUE, verbose=TRUE){
#   if (verbose == TRUE) message("... ordering cooccurrences and nodes by id")
#   
#   minMaxMatrix <- t(apply(
#     as.matrix(object@stat[,c("nodeId", "cooccurrenceId"), with=FALSE]),
#     1, .minMaxId
#   )) # .minMaxId -> utils.R
#   colnames(minMaxMatrix) <- c("a", "b", "minId", "maxId")
#   minMaxOrder <- apply(minMaxMatrix, 1, function(row) ifelse(row["a"] <= row["b"], 0, 1 ))
#   if (verbose == TRUE) message("... merging and trimming")
#   statPlus <- data.table(
#     minMaxMatrix[,c("minId", "maxId")], idOrder=minMaxOrder,
#     object@stat
#   )
#   setkey(statPlus, "idOrder")
#   aToB <- statPlus[.(0)]
#   setkey(aToB, minId, maxId)
#   bToA <- statPlus[.(1)]
#   setkey(bToA, minId, maxId)
#   merger <- merge(aToB, bToA)
#   colnamesToDrop <- c(
#     "minId", "maxId" , "idOrder.x", "idOrder.y", "rank.x", "rank.y", "relation.x", "relation.y",
#     "cooccurrence.x", "cooccurrence.y", "cooccurrenceId.x", "cooccurrenceId.y", "cooccurrenceCorpusFreq.x", "cooccurrenceCorpusFreq.y",
#     "windowSize.x", "windowSize.y", "exp_b.x", "exp_b.y", "exp_a.x", "exp_a.y",
#     "cooccurrenceWindowFreq.y"
#   )
#   merger[, c(colnamesToDrop) := NULL]
#   setnames(
#     merger,
#     c(
#       "node.x", "nodeId.x", "nodeCorpusFreq.x", "cooccurrenceWindowFreq.x", "ll.x",
#       "node.y", "nodeId.y", "nodeCorpusFreq.y", "ll.y"),
#     c("a", "a_id", "count_a", "count_ab", "ll_a2b", "b", "b_id", "count_b", "ll_b2a")
#   )
#   setcolorder(merger, c("a", "b", "a_id", "b_id", "count_ab", "count_a", "count_b", "ll_a2b", "ll_b2a"))
#   setkey(merger, a, b)
#   retval <- new("cooccurrencesReshaped")
#   for (x in (slotNames(object))) if (x != "stat") slot(retval, x) <- slot(object, x)
#   retval@stat <- merger
#   
#   return(retval)
# }

