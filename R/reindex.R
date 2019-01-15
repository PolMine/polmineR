#' @include S4classes.R
NULL

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

setMethod("reindex", "Cooccurrences", function(x){
  
  if (length(x@p_attribute) > 1L) stop("Method 'reindex' only works if one and only one p-attribute is used.")
  verbose <- interactive()
  
  if (verbose) message("... creating data.table for reindexing")
  dt <- data.table(id = unique(x@stat[["a_id"]]))
  setkeyv(dt, cols = "id")
  setorderv(dt, cols = "id")
  dt[, "id_new" := 1L:nrow(dt), with = TRUE]
  setkeyv(x@stat, "a_id")
  
  if (verbose) message("... reindexing a")
  x@stat[, "a_new_index" := x@stat[dt][["id_new"]]]
  setkeyv(x@stat, "b_id")
  
  if (verbose) message("... reindexing b")
  x@stat[, "b_new_index" := x@stat[dt][["id_new"]]]
  
  if (verbose) message("... decoding tokens")
  as.nativeEnc(
    cl_id2str(corpus = x@corpus, p_attribute = x@p_attribute, id = dt[["id"]]),
    from = getEncoding(x@corpus)
  )
})