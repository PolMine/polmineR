#' @include partition_class.R 
NULL

setGeneric("sAttributes", function(.Object, ...) standardGeneric("sAttributes"))


#' @param unique logical, whether to return unique values only
#' @param regex filter return value by applying a regex
#' @rdname sAttributes-method
setMethod("sAttributes", "character", function(.Object, sAttribute = NULL, unique = TRUE, regex = NULL){
  if (!.Object %in% CQI$list_corpora()) stop("corpus name provided not available")
  
  if (is.null(sAttribute)){
    return( CQI$attributes(.Object, "s") )
  } else {
    if (length(sAttribute) == 1){
      ret <- CQI$struc2str(
        .Object, sAttribute,
        0:(CQI$attribute_size(.Object, sAttribute, type = "s") - 1)
      )
      if (!is.null(regex)) ret <- grep(regex, ret, value = TRUE)
      if (unique) ret <- unique(ret)
      Encoding(ret) <- getEncoding(.Object)
      ret <- as.nativeEnc(ret, from = getEncoding(.Object))
      return(ret)
    } else if (length(sAttribute) > 1){
      corpusEncoding <- registry_get_encoding(.Object)
      metaInformation <- lapply(
        sAttribute,
        function(x) {
          retval <- CQI$struc2str(.Object, x, 0:(CQI$attribute_size(.Object, x, "s") - 1))
          Encoding(retval) <- corpusEncoding
          as.nativeEnc(retval, from = corpusEncoding)
        })
      names(metaInformation) <- sAttribute
      return( as.data.table(metaInformation) )
    }
  }
})

#' Get s-attributes.
#' 
#' Structural annotations (s-attributes) of a corpus provide metainformation for
#' regions of tokens. Gain access to the s-attributes available for a corpus or partition,
#' or the values of s-attributes in a corpus/partition with the \code{sAttributes}-method.
#' 
#' Importing XML into the Corpus Workbench (CWB) turns elements and element
#' attributes into so-called s-attributes. There are two uses of the sAttributes-method: If the 
#' \code{sAttribute} parameter is NULL (default), the return value is a character vector
#' with all s-attributes present in a corpus.
#' 
#' If sAttribute is the name of a specific s-attribute (a length 1 character vector), the
#' values of the s-attributes available in the corpus/partition are returned.
#' 
#' If a character vector of s-attributes is provided, the method will return a \code{data.table}.
#'
#' @param .Object either a \code{partition} object or a character vector specifying a CWB corpus
#' @param sAttribute name of a specific s-attribute
#' @return a character vector
#' @exportMethod sAttributes
#' @docType methods
#' @aliases sAttributes sAttributes,character-method sAttributes,partition-method
#' @rdname sAttributes-method
#' @examples 
#'   use("polmineR")
#'   
#'   sAttributes("GERMAPARLMINI")
#'   sAttributes("GERMAPARLMINI", "date") # dates of plenary meetings
#'   
#'   
#'   P <- partition("GERMAPARLMINI", date = "2009-11-10")
#'   sAttributes(P)
#'   sAttributes(P, "speaker") # get names of speakers
setMethod(
  "sAttributes", "partition",
  function (.Object, sAttribute = NULL, unique = TRUE) {
    if (is.null(sAttribute)){
      return( CQI$attributes(.Object@corpus, "s") )
    } else {
      if (length(sAttribute) == 1){
        if (.Object@xml == "flat" || .Object@sAttributeStrucs == sAttribute){
          len1 <- CQI$attribute_size(.Object@corpus, .Object@sAttributeStrucs)
          len2 <- CQI$attribute_size(.Object@corpus, sAttribute)
          if (len1 != len2){
            stop("XML is stated to be flat, but sAttributeStrucs hat length ", len1, " and sAttribute length ", len2)
          }
          retval <- CQI$struc2str(.Object@corpus, sAttribute, .Object@strucs)
          if (unique) retval <- unique(retval)
        } else {
          cposVector <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
          strucs <- CQI$cpos2struc(.Object@corpus, sAttribute, cposVector)
          retval <- CQI$struc2str(.Object@corpus, sAttribute, strucs)
          if (unique) retval <- unique(retval)
        }
        Encoding(retval) <- .Object@encoding
        retval <- as.nativeEnc(retval, from = .Object@encoding)
        return(retval)
      } else if (length(sAttribute) > 1){
        if (.Object@xml == "flat") {
          tab <- data.frame(
            lapply(
              sAttribute,
              # USE.NAMES = TRUE,
              function(x) { 
                tmp <- CQI$struc2str(.Object@corpus, x, .Object@strucs)
                Encoding(tmp) <- .Object@encoding
                as.nativeEnc(tmp, from = .Object@encoding)
              }
            ),
            stringsAsFactors = FALSE
          )
          colnames(tab) <- sAttribute
        } else if (.Object@xml == "nested") {
          tab <- data.frame(
            sapply(
              sAttribute,
              USE.NAMES = TRUE,
              function(x) {
                tmp <- CQI$struc2str(.Object@corpus, x, CQI$cpos2struc(.Object@corpus, x, .Object@cpos[,1]))
                Encoding(tmp) <- .Object@encoding
                as.nativeEnc(tmp, from = .Object@encoding)
              }
            )
          )
          colnames(tab) <- sAttribute
        }
        return( as.data.table(tab) )
        
      }
    }
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
