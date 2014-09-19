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