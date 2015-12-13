#' @include textstat_class.R comp_class.R context_class.R
NULL

#' Calculate pointwise mutual information (PMI)
#' 
#' There may be a (small) problem: the counts used are not cooccurrences ...
#' 
#' @param windowIds ids of cooccurring words, used for matching
#' @rdname textstatistics
#' @name textstatistics
setGeneric("pmi", function(.Object){standardGeneric("pmi")})


setMethod("pmi", "context", function(.Object){
  .Object@stat[, pmi := log2((count_window/.Object@partitionSize)/((.Object@count/.Object@partitionSize)*(count_partition/.Object@partitionSize)))]
  .Object <- sort(.Object, by="pmi")
  .Object@stat[, rank_pmi := c(1:nrow(.Object@stat))]
  .Object@method <- c(.Object@method, "pmi")
  return(.Object)
})
