#' get values of metadata
#' 
#' Returns the values that an sAttribute takes within a partition.
#' 
#' @param object a partition object
#' @param sAttribute the s-attribute to be looked up
#' @return a character vector
#' @aliases meta meta-method meta,partition-method
#' @rdname meta
#' @name meta
#' @exportMethod meta
#' @docType methods
setMethod("meta", "partition", function(object, sAttribute){
  if ("values" %in% names(object@metadata)) {
    ret <- object@metadata$values[[sAttribute]]
  } else {
    ret <- unique(cqi_struc2str(paste(object@corpus, '.', sAttribute, sep=''), object@strucs));
    Encoding(ret) <- object@encoding;  
  }
  ret
})
