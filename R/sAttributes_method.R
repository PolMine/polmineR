#' @include partition_class.R 
NULL

setGeneric("sAttributes", function(.Object, ...){standardGeneric("sAttributes")})


#' @param unique logical, whether to return unique values only
#' @param regex filter return value by applying a regex
#' @rdname sAttributes-method
setMethod("sAttributes", "character", function(.Object, sAttribute = NULL, unique = TRUE, regex = NULL){
  if (is.null(sAttribute)){
    return(CQI$attributes(.Object, "s"))
  } else {
    if (.Object %in% CQI$list_corpora()) {
      ret <- CQI$struc2str(
        .Object, sAttribute,
        c(0:(CQI$attribute_size(.Object, sAttribute)-1))
        )
      if (!is.null(regex)) ret <- grep(regex, ret, value = TRUE)
      if (unique == TRUE) ret <- unique(ret)
      Encoding(ret) <- getEncoding(.Object)
      return(ret)
    } else {
      warning("corpus name provided not available")
      return(NULL)
    }
  }
})

#' Get s-attributes.
#' 
#' Structural annotations (s-attributes) of a corpus provide metainformation for
#' ranges of text. Importing XML into the Corpus Workbench (CWB) turns elements and element
#' attributes into s-attributes. There are two uses of the sAttributes-method: If the 
#' \code{sAttribute} parameter is NULL (default), the return value is a character vector
#' with all s-attributes. If sAttribute is the name of a specific s-attribute (i.e. a 
#' length 1 character vector), the respective s-attributes available in the corpus are 
#' returned.
#'
#' @param .Object either a \code{partition} or a character vector specifying a CWB corpus
#' @param sAttribute name of a specific s-attribute
#' @return a character vector
#' @exportMethod sAttributes
#' @docType methods
#' @aliases sAttributes sAttributes,character-method sAttributes,partition-method
#' @rdname sAttributes-method
#' @examples 
#' \dontrun{
#'   use("polmineR.sampleCorpus")
#'   
#'   sAttributes("PLPRBTTXT")
#'   sAttributes("PLPRBTTXT", "text_date") # dates of plenary meetings
#'   
#'   P <- partition("PLPRBTTXT", text_date = "2009-11-10")
#'   sAttributes(P)
#'   sAttributes(P, "text_name") # get names of speakers
#' }
setMethod(
  "sAttributes", "partition",
  function (.Object, sAttribute = NULL) {
    if (is.null(sAttribute)){
      ret <- CQI$attributes(.Object@corpus, "s")
    } else {
      ret <- unique(CQI$struc2str(.Object@corpus, sAttribute, .Object@strucs));
      Encoding(ret) <- .Object@encoding;  
    }
    ret
  }
)

#' @docType methods
#' @rdname partitionBundle-class
setMethod("sAttributes", "partitionBundle", function(.Object, sAttribute){
  lapply(
    .Object@objects,
    function(x) sAttributes(x, sAttribute)
    )
})
