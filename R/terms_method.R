#' @include partition_class.R
NULL

#' get terms available in a corpus
#' 
#' @param x a partition object
#' @param pAttribute the pAttribute to be queried
#' @param regex a regex to filter results
#' @exportMethod terms
#' @docType methods
#' @name terms-partition-method
#' @rdname terms-partition-method
#' @aliases terms,partition-method
#' @docType methods
setMethod("terms", "partition", function(x, pAttribute, regex=NULL){
  if (!identical(pAttribute, x@pAttribute)) x <- enrich(x, pAttribute=pAttribute) 
  terms <- rownames(x@stat)
  terms <- enc2utf8(terms)
  if (!is.null(regex)) {
    termsSelectRaw <- lapply(regex, function(r) terms[grep(r, terms)])
    terms <- unlist(termsSelectRaw)
  }
  return(terms)
})
