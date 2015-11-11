#' @include cooccurrences_class.R generics.R
NULL

setMethod("[", "cooccurrences", function(x,i){
  tab <- subset(x@stat, node == i)
  tab <- tab[, c("cooccurrenceWindowFreq", "cooccurrenceCorpusFreq", "expCoi", "expCorpus", "ll")]
  colnames(tab) <- c("cooccurrence", "countCoi", "countCorpus", "expCoi", "expCorpus", "ll")
  return(tab)
})

setMethod("show", "cooccurrences", function(object){
  cat("Object of 'cooccurrences'-class\n")
  cat("Number of rows: ", nrow(object@stat), "\n")
})


setMethod("summary", "cooccurrences", function(object){
  return(.statisticalSummary(object))
})

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

# calles by trim, cooccurrences-method
.reshapeCooccurrences <- function(object, mc=TRUE, verbose=TRUE){
  if (verbose == TRUE) message("... ordering cooccurrences and nodes by id")
  minMaxMatrix <- t(apply(
    as.matrix(object@stat[,c("nodeId", "cooccurrenceId"), with=FALSE]),
    1, .minMaxId
    )) # .minMaxId -> utils.R
  colnames(minMaxMatrix) <- c("a", "b", "minId", "maxId")
  minMaxOrder <- apply(minMaxMatrix, 1, function(row) ifelse(row["a"] <= row["b"], 0, 1 ))
  if (verbose == TRUE) message("... merging and trimming")
  statPlus <- data.table(
    minMaxMatrix[,c("minId", "maxId")], idOrder=minMaxOrder,
    object@stat
  )
  setkey(statPlus, "idOrder")
  aToB <- statPlus[.(0)]
  setkey(aToB, minId, maxId)
  bToA <- statPlus[.(1)]
  setkey(bToA, minId, maxId)
  merger <- merge(aToB, bToA)
  colnamesToDrop <- c(
    "minId", "maxId" , "idOrder.x", "idOrder.y", "rank.x", "rank.y", "relation.x", "relation.y",
    "cooccurrence.x", "cooccurrence.y", "cooccurrenceId.x", "cooccurrenceId.y", "cooccurrenceCorpusFreq.x", "cooccurrenceCorpusFreq.y",
    "windowSize.x", "windowSize.y", "expCorpus.x", "expCorpus.y", "expCoi.x", "expCoi.y",
    "cooccurrenceWindowFreq.y"
  )
  merger[, c(colnamesToDrop) := NULL]
  setnames(
    merger,
    c(
      "node.x", "nodeId.x", "nodeCorpusFreq.x", "cooccurrenceWindowFreq.x", "ll.x",
      "node.y", "nodeId.y", "nodeCorpusFreq.y", "ll.y"),
    c("a", "a_id", "tf_a", "tf_ab", "ll_a2b", "b", "b_id", "tf_b", "ll_b2a")
    )
  setcolorder(merger, c("a", "b", "a_id", "b_id", "tf_ab", "tf_a", "tf_b", "ll_a2b", "ll_b2a"))
  setkey(merger, a, b)
  retval <- new("cooccurrencesReshaped")
  for (x in (slotNames(object))) if (x != "stat") slot(retval, x) <- slot(object, x)
  retval@stat <- merger
  
  return(retval)
}


