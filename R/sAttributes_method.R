#' @include partition_class.R 
NULL

setGeneric("sAttributes", function(object,...){standardGeneric("sAttributes")})


#' @param unique logical, whether to return unique values only
#' @param regex apply a regex
#' @rdname sAttributes-method
setMethod("sAttributes", "character", function(object, sAttribute=NULL, unique=TRUE, regex=NULL){
  if (is.null(sAttribute)){
    ret <- cqi_attributes(object, "s")
    ret
  } else {
    if (object %in% cqi_list_corpora()) {
      ret <- cqi_struc2str(
        paste(object, '.', sAttribute, sep=''),
        c(0:(cqi_attribute_size(paste(object, '.', sAttribute, sep=''))-1))
        )
      if (!is.null(regex)) {
        ret <- grep(regex, ret, value=TRUE)
      }
      if (unique == TRUE) ret <- unique(ret)
      Encoding(ret) <- getEncoding(object) 
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
      ret <- unique(cqi_struc2str(paste(object@corpus, '.', sAttribute, sep=''), object@strucs));
      Encoding(ret) <- object@encoding;  
    }
    ret
  }
)

#' @docType methods
#' @rdname partitionBundle-class
setMethod("sAttributes", "partitionBundle", function(object, sAttribute){
  lapply(
    object@objects,
    function(x) sAttributes(x, sAttribute)
    )
})
