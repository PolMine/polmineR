#' @include partition-class.R 
NULL

setGeneric("sAttributes", function(object,...){standardGeneric("sAttributes")})


#' @rdname sAttributes-method
setMethod("sAttributes", "character", function(object, sAttribute=NULL){
  if (is.null(sAttribute)){
    ret <- cqi_attributes(object, "s")
    ret
  } else {
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
  }
  ret
})

#' Print S-Attributes in a partition or corpus
#' 
#' Convencience function - just to access the s-sttributes in a partition
#' quickly.
#'
#' @param object either a partition or a character vector specifying a CWB corpus
#' @param sAttribute bla
#' @return the S-Attributes are immediately printed
#' @exportMethod sAttributes
#' @docType methods
#' @aliases sAttributes sAttributes,character-method sAttributes,partition-method
#' @rdname sAttributes-method
setMethod(
  "sAttributes", "partition",
  function (object, sAttribute=NULL) {
    if (is.null(sAttribute)){
    ret <- cqi_attributes(object@corpus, "s")
    } else {
      if ("values" %in% names(object@metadata)) {
        ret <- object@metadata$values[[sAttribute]]
      } else {
        ret <- unique(cqi_struc2str(paste(object@corpus, '.', sAttribute, sep=''), object@strucs));
        Encoding(ret) <- object@encoding;  
      }   
    }
    ret
  }
)
