#' @include partition.R S4classes.R
NULL

setGeneric("s_attributes", function(.Object, ...) standardGeneric("s_attributes"))


#' @param unique logical, whether to return unique values only
#' @param regex filter return value by applying a regex
#' @param ... to maintain backward compatibility, of argument \code{sAttribute} is used
#' @rdname s_attributes-method
setMethod("s_attributes", "character", function(.Object, s_attribute = NULL, unique = TRUE, regex = NULL, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  if (!.Object %in% CQI$list_corpora()) stop("corpus name provided not available")
  
  if (is.null(s_attribute)){
    return( CQI$attributes(.Object, "s") )
  } else {
    if (length(s_attribute) == 1){
      ret <- CQI$struc2str(
        .Object, s_attribute,
        0L:(CQI$attribute_size(.Object, s_attribute, type = "s") - 1L)
      )
      if (!is.null(regex)) ret <- grep(regex, ret, value = TRUE)
      if (unique) ret <- unique(ret)
      Encoding(ret) <- registry_get_encoding(.Object)
      ret <- as.nativeEnc(ret, from = registry_get_encoding(.Object))
      return(ret)
    } else if (length(s_attribute) > 1){
      corpusEncoding <- registry_get_encoding(.Object)
      metaInformation <- lapply(
        s_attribute,
        function(x) {
          retval <- CQI$struc2str(.Object, x, 0:(CQI$attribute_size(.Object, x, "s") - 1))
          Encoding(retval) <- corpusEncoding
          as.nativeEnc(retval, from = corpusEncoding)
        })
      names(metaInformation) <- s_attribute
      return( as.data.table(metaInformation) )
    }
  }
})

#' Get s-attributes.
#' 
#' Structural annotations (s-attributes) of a corpus provide metainformation for
#' regions of tokens. Gain access to the s-attributes available for a corpus or partition,
#' or the values of s-attributes in a corpus/partition with the \code{s_attributes}-method.
#' 
#' Importing XML into the Corpus Workbench (CWB) turns elements and element
#' attributes into so-called s-attributes. There are two uses of the s_attributes-method: If the 
#' \code{s_attribute} parameter is NULL (default), the return value is a character vector
#' with all s-attributes present in a corpus.
#' 
#' If s_attribute is the name of a specific s-attribute (a length 1 character vector), the
#' values of the s-attributes available in the corpus/partition are returned.
#' 
#' If a character vector of s-attributes is provided, the method will return a \code{data.table}.
#'
#' @param .Object either a \code{partition} object or a character vector specifying a CWB corpus
#' @param s_attribute name of a specific s-attribute
#' @return a character vector
#' @exportMethod s_attributes
#' @docType methods
#' @aliases s_attributes s_attributes,character-method s_attributes,partition-method
#' @rdname s_attributes-method
#' @examples 
#' use("polmineR")
#'   
#' s_attributes("GERMAPARLMINI")
#' s_attributes("GERMAPARLMINI", "date") # dates of plenary meetings
#'   
#' P <- partition("GERMAPARLMINI", date = "2009-11-10")
#' s_attributes(P)
#' s_attributes(P, "speaker") # get names of speakers
setMethod(
  "s_attributes", "partition",
  function (.Object, s_attribute = NULL, unique = TRUE, ...) {
    if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
    if (is.null(s_attribute)){
      return( CQI$attributes(.Object@corpus, "s") )
    } else {
      if (length(s_attribute) == 1){
        if (.Object@xml == "flat" || .Object@s_attribute_strucs == s_attribute){
          len1 <- CQI$attribute_size(.Object@corpus, .Object@s_attribute_strucs)
          len2 <- CQI$attribute_size(.Object@corpus, s_attribute)
          if (len1 != len2){
            stop("XML is stated to be flat, but s_attribute_strucs hat length ", len1, " and s_attribute length ", len2)
          }
          retval <- CQI$struc2str(.Object@corpus, s_attribute, .Object@strucs)
          if (unique) retval <- unique(retval)
        } else {
          cposVector <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
          strucs <- CQI$cpos2struc(.Object@corpus, s_attribute, cposVector)
          # filtering out negative struc values is necessary, because RcppCWB
          # will complain about negative values
          strucs <- strucs[which(strucs > 0)]
          retval <- CQI$struc2str(.Object@corpus, s_attribute, strucs)
          if (unique) retval <- unique(retval)
        }
        Encoding(retval) <- .Object@encoding
        retval <- as.nativeEnc(retval, from = .Object@encoding)
        return(retval)
      } else if (length(s_attribute) > 1){
        if (.Object@xml == "flat") {
          tab <- data.frame(
            lapply(
              s_attribute,
              # USE.NAMES = TRUE,
              function(x) { 
                tmp <- CQI$struc2str(.Object@corpus, x, .Object@strucs)
                Encoding(tmp) <- .Object@encoding
                as.nativeEnc(tmp, from = .Object@encoding)
              }
            ),
            stringsAsFactors = FALSE
          )
          colnames(tab) <- s_attribute
        } else if (.Object@xml == "nested") {
          tab <- data.frame(
            sapply(
              s_attribute,
              USE.NAMES = TRUE,
              function(x) {
                tmp <- CQI$struc2str(.Object@corpus, x, CQI$cpos2struc(.Object@corpus, x, .Object@cpos[,1]))
                Encoding(tmp) <- .Object@encoding
                as.nativeEnc(tmp, from = .Object@encoding)
              }
            )
          )
          colnames(tab) <- s_attribute
        }
        return( as.data.table(tab) )
        
      }
    }
  }
)

#' @docType methods
#' @rdname partition_bundle-class
setMethod("s_attributes", "partition_bundle", function(.Object, s_attribute, ...){
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  lapply(.Object@objects, function(x) s_attributes(x, s_attribute))
})
