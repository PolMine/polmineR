#' get terms available in a corpus
#' 
#' @param x a partition object
#' @param pAttribute the pAttribute to be queried
#' @param regex a regex to filter results
#' @exportMethod terms
#' @include partition.R
#' @name terms-partition-method
#' @rdname terms-partition-method
#' @aliases terms,partition-method
#' @docType methods
setMethod("terms", "partition", function(x, pAttribute, regex=NULL){
  if (!pAttribute %in% names(x@tf)) x <- enrich(x, tf=pAttribute) 
  terms <- rownames(x@tf[[pAttribute]])
  terms <- enc2utf8(terms)
  if (!is.null(regex)) {
    termsSelectRaw <- lapply(regex, function(r) terms[grep(r, terms)])
    terms <- unlist(termsSelectRaw)
  }
  return(terms)
})
