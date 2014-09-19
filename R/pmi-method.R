#' @include textstat-class.R keyness-class.R context-class.R
NULL

setGeneric("pmi", function(object){standardGeneric("pmi")})

#' Calculate pointwise mutual information (PMI)
#' 
#' There may be a (small) problem: the counts used are not cooccurrences ...
#' 
#' @param windowIds ids of cooccurring words, used for matching
#' @param windowFreq count for occurrence of collocates
#' @param countTarget counts of targe in partition (total) 
#' @param partitionObject here we get the counts for cooccurring tokens
#' @param pattribute needed for matching
#' @noRd
.pmi <- function(windowIds, windowFreq, partitionFreq, countTarget, partitionSize){
  calc <- matrix(data=0, nrow=length(windowFreq), ncol=4)
  size <- partitionSize
  colnames(calc) <- c("collocateId", "countCoi", "countPartition", "pmi")
  calc[,1] <- windowIds
  calc[,2] <- windowFreq
  calc[,3] <- partitionFreq
  calc[,4] <- log2((calc[,2]/size)/((countTarget/size)*(calc[,3]/size)))
  calc
}

setMethod("pmi", "context", function(object){
  mat <- .pmi(
    windowIds=object@stat$id,
    windowFreq=object@stat$countCoi,
    partitionFreq=object@stat$countCorpus,
    countTarget=object@frequency,
    partitionSize=object@partitionSize
    )
  object@stat <- data.frame(
    object@stat,
    pmi=mat[,4]
    )
  object@statisticalTest <- c(object@statisticalTest, "pmi")
  return(object)
})