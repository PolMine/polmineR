#' @include partition-class.R 
NULL

setGeneric("sAttributes", function(object,...){standardGeneric("sAttributes")})


#' @docType methods
#' @noRd
setMethod("sAttributes", "character", function(object){
  sAttributes <- cqi_attributes(object, "s")
  sAttributes
  
})

#' Print S-Attributes in a partition or corpus
#' 
#' Convencience function - just to access the s-sttributes in a partition
#' quickly.
#'
#' @param object either a partition or a character vector specifying a CWB corpus
#' @return the S-Attributes are immediately printed
#' @exportMethod sAttributes
#' @docType methods
#' @aliases sAttributes sAttributes,character-method sAttributes,partition-method
#' @rdname sAttributes-method
setMethod(
  "sAttributes", "partition",
  function (object) {
    sAttributes <- cqi_attributes(object@corpus, "s")
    sAttributes
  }
)
