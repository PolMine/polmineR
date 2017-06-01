setGeneric("reindex", function(x) standardGeneric("reindex"))

setMethod("reindex", "DocumentTermMatrix", function(x){
  i_uniqueValues <- unique(x$i)
  i_uniqueValuesOrdered <- i_uniqueValues[order(i_uniqueValues)]
  i_newIndex <- rep(0, times = i_uniqueValuesOrdered[length(i_uniqueValuesOrdered)])
  i_newIndex[i_uniqueValuesOrdered] <- 1:length(i_uniqueValues)
  x$i <- i_newIndex[x$i]
  x$nrow <- length(i_uniqueValues)
  
  j_uniqueValues <- unique(x$j)
  j_uniqueValuesOrdered <- j_uniqueValues[order(j_uniqueValues)]
  j_newIndex <- rep(0, times=j_uniqueValuesOrdered[length(j_uniqueValuesOrdered)])
  j_newIndex[j_uniqueValuesOrdered] <- c(1:length(j_uniqueValues))
  x$j <- j_newIndex[x$j]
  x$ncol <- length(j_uniqueValues)
  
  x
})

setMethod("reindex", "TermDocumentMatrix", function(x){
  t(reindex(t(x)))
})