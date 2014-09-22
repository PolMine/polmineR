#' @include textstat-class.R keyness-class.R context-class.R
NULL

#' Log-likelihood-test 
#' 
#' @exportMethod ll
#' @noRd
setGeneric("ll", function(object, ...){standardGeneric("ll")})

#' @param ids the numeric ids (integer) of the tokens
#' @param windowFreq term frequency of the tokens in corpus A
#' @param corpusFreq term frequency of the tokens in the corpus B
#' @param windows.total total size of the window 
#' @param corpus.total total size of the corpus
#' @return a Matrix with six columns
#' @noRd
.g2Statistic <- function(ids, windowFreq, corpusFreq, windows.total, corpus.total){
  calc <- matrix(data=0, nrow=length(windowFreq), ncol=6)
  colnames(calc) <- c("collocateId", "countCoi", "countCorpus", "expCoi", "expCorpus", "ll")
  calc[,1] <- ids
  calc[,2] <- windowFreq
  calc[,3] <- corpusFreq
  calc[,4] <- windows.total*calc[,3]/corpus.total
  calc[,5] <- (corpus.total-windows.total)*calc[,3]/corpus.total
  calc[,6] <- 2*(calc[,2]*log(calc[,2]/calc[,4])+((calc[,3]-calc[,4])*log((calc[,3]-calc[,4])/calc[,5])))
  calc
}

setMethod("ll", "context", function(object, partitionObject){
  mat <- .g2Statistic(
    ids=object@stat$id,
    windowFreq=object@stat$countCoi,
    corpusFreq=object@stat$countCorpus,
    windows.total=object@size,
    corpus.total=partitionObject@size
    )
  
  object@stat <- cbind(
    object@stat,
    expCoi=mat[,"expCoi"],
    expCorpus=mat[,"expCorpus"],
    ll=mat[, "ll"]
    )
  object@statisticalTest <- c(object@statisticalTest, "ll")
  return(object)
})

setMethod("ll", "keyness", function(object){
  mat <- .g2Statistic(
    ids=rep(0, times=nrow(object@stat)),
    windowFreq=object@stat$countCoi,
    corpusFreq=object@stat$countRef,
    windows.total=object@sizeCoi,
    corpus.total=object@sizeRef
    )
  if (! "expCoi" %in% colnames(object@stat)) object@stat$expCoi <- mat[, "expCoi"]
  if (! "expRef" %in% colnames(object@stat)) object@stat$expRef <- mat[, "expCorpus"]
  object@stat$ll <- mat[,"ll"]
  object@statisticalTest <- c(object@statisticalTest, "ll")
  return(object)
})

setMethod("ll", "collocations", function(object, partitionSize){
  mat <- .g2Statistic(
    ids=rep(0, times=nrow(object@stat)),
    windowFreq=object@stat[,"collocateWindowFreq"],
    corpusFreq=object@stat[,"collocateCorpusFreq"],
    windows.total=object@stat[,"windowSize"],
    corpus.total=partitionSize
    )
  object@stat <- cbind(
    object@stat,
    expCoi=mat[,"expCoi"],
    expCorpus=mat[,"expCorpus"],
    ll=mat[, "ll"]
  )
  object@method <- c(object@method, "ll")
  return(object)
})

