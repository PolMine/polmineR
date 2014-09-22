#' @include collocations-class.R
NULL

setMethod("[", "collocations", function(x,i){
  tab <- subset(x@stat, node == i)
  tab <- tab[, c("collocateWindowFreq", "collocateCorpusFreq", "expCoi", "expCorpus", "ll")]
  colnames(tab) <- c("collocate", "countCoi", "countCorpus", "expCoi", "expCorpus", "ll")
  return(tab)
})

setMethod("show", "collocations", function(object){
  cat("Object of 'collocations'-class\n")
  cat("Number of rows: ", nrow(object@stat), "\n")
})


setMethod("summary", "collocations", function(object){
  return(.statisticalSummary(object))
})

#' @importFrom parallel mcparallel mccollect
setMethod("as.sparseMatrix", "collocations", function(x, col, mc=FALSE){
  uniqueTerms <- unique(c(x@stat[,"node"], x@stat[,"collocate"]))
  keyVector <- setNames(c(1:length(uniqueTerms)), uniqueTerms)
  splittedTab <- split(x=x@stat[,c(col, "collocate")], f=x@stat[,"node"])
  system.time(
  if (mc==FALSE){
    bag <- list()
    bag[["i"]] <- unname(unlist(lapply(names(splittedTab), function(n) rep(keyVector[n], times=nrow(splittedTab[[n]]))))) #nodes
    bag[["j"]] <- unname(unlist(lapply(splittedTab, function(tab) keyVector[tab[,"collocate"]]))) # collocates
    bag[["x"]] <- unname(unlist(lapply(splittedTab, function(tab) tab[,col]))) # values
  } else if (mc==TRUE) {
    i <- mcparallel(unname(unlist(lapply(names(splittedTab), function(n) rep(keyVector[n], times=nrow(splittedTab[[n]])))))) #nodes
    j <- mcparallel(unname(unlist(lapply(splittedTab, function(tab) keyVector[tab[,"collocate"]])))) # collocates
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

