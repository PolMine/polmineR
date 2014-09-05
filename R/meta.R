# documented with meta,partition-method
setGeneric("meta", function(object, ...){standardGeneric("meta")})


#' get values of metadata
#' 
#' Returns the values that an sAttribute takes within a partition.
#' 
#' @param object a partition object or a character vector (length 1) supplying the name of a corpus
#' @param sAttribute the s-attribute to be looked up
#' @return a character vector
#' @aliases meta meta-method meta,partition-method meta,character-method
#' @rdname meta
#' @name meta
#' @exportMethod meta
#' @include partition.R
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

setMethod("meta", "character", function(object, sAttribute){
  if (object %in% cqi_list_corpora()) {
    ret <- unique(cqi_struc2str(
      paste(object, '.', sAttribute, sep=''),
      1:cqi_attribute_size(paste(object, '.', sAttribute, sep=''))
        )
      )
    Encoding(ret) <- .getCorpusEncoding(object) 
  } else {
    warning("corpus name provided not available")
    ret <- NULL
  }
  return(ret)
})