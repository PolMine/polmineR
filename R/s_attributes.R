#' @include partition.R S4classes.R
NULL

#' @rdname s_attributes-method
setGeneric("s_attributes", function(.Object, ...) standardGeneric("s_attributes"))


#' @param unique Logical, whether to return unique values.
#' @param regex A regular expression passed into \code{grep} to filter return
#'   value by applying a regex.
#' @param ... To maintain backward compatibility, if argument \code{sAttribute}
#'   (deprecated) is used.
#' @rdname s_attributes-method
#' @name s_attributes
#' @aliases s_attributes,character-method
setMethod("s_attributes", "character", function(.Object, s_attribute = NULL, unique = TRUE, regex = NULL, ...){
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  s_attributes(.Object = corpus(.Object), s_attribute = s_attribute, unique = unique, regex = regex, ...)
})


#' @examples
#' s_attributes(corpus("GERMAPARLMINI"))
setMethod("s_attributes", "corpus", function(.Object, s_attribute = NULL, unique = TRUE, regex = NULL, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  if (!.Object@corpus %in% CQI$list_corpora()) stop("corpus name provided not available")
  
  if (is.null(s_attribute)){
    return( CQI$attributes(.Object@corpus, "s") )
  } else {
    if (length(s_attribute) == 1){
      y <- CQI$struc2str(
        .Object@corpus, s_attribute,
        0L:(CQI$attribute_size(.Object@corpus, s_attribute, type = "s") - 1L)
      )
      if (!is.null(regex)) y <- grep(regex, y, value = TRUE)
      if (unique) y <- unique(y)
      Encoding(y) <- .Object@encoding
      if (.Object@encoding != localeToCharset()[1]) y <- as.nativeEnc(y, from = .Object@encoding)
      return(y)
    } else if (length(s_attribute) > 1){
      y <- lapply(
        s_attribute,
        function(x) {
          retval <- CQI$struc2str(.Object@corpus, x, 0L:(CQI$attribute_size(.Object@corpus, x, "s") - 1L))
          Encoding(retval) <- .Object@encoding
          as.nativeEnc(retval, from = .Object@encoding)
        })
      names(y) <- s_attribute
      return( data.table::as.data.table(y) )
    }
  }
})


#' Get s-attributes.
#' 
#' Structural annotations (s-attributes) of a corpus capture metainformation for
#' regions of tokens. The \code{s_attributes}-method offers high-level access to
#' the s-attributes present in a corpus or subcorpus, or the values of
#' s-attributes in a corpus/partition.
#' 
#' Importing XML into the Corpus Workbench (CWB) turns elements and element
#' attributes into so-called s-attributes. There are two uses of the
#' s_attributes-method: If the \code{s_attribute} parameter is NULL (default),
#' the return value is a \code{character} vector with all s-attributes present in a
#' corpus.
#' 
#' If \code{s_attribute} is the name of a specific s-attribute (a length one
#' character vector), the values of the s-attributes available in the
#' \code{corpus}/\code{partition} are returned.
#' 
#' If a character vector of s-attributes is provided, the method will return a \code{data.table}.
#'
#' @param .Object either a \code{partition} object or a character vector specifying a CWB corpus
#' @param s_attribute name of a specific s-attribute
#' @return a character vector
#' @exportMethod s_attributes
#' @docType methods
#' @rdname s_attributes-method
#' @examples 
#' use("polmineR")
#'   
#' s_attributes("GERMAPARLMINI")
#' s_attributes("GERMAPARLMINI", "date") # dates of plenary meetings
#' s_attributes("GERMAPARLMINI", s_attribute = c("date", "party"))  
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
      if (length(s_attribute) == 1L){
        # Checking whether the xml is flat / whether s_attribute is in .Object@s_attribute_strucs 
        # is necessary because there are scenarios when these slots are not defined.
        xml_is_flat <- if (length(.Object@xml) > 0) if (.Object@xml == "flat") TRUE else FALSE else FALSE
        s_attr_strucs <- if (length(.Object@s_attribute_strucs) > 0) if (.Object@s_attribute_strucs == s_attribute) TRUE else FALSE else FALSE
        if (xml_is_flat && s_attr_strucs){
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
        Encoding(retval) <- localeToCharset()[1]
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
                tmp <- as.nativeEnc(tmp, from = .Object@encoding)
                Encoding(tmp) <- localeToCharset()[1]
                tmp
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
                Encoding(tmp) <- localeToCharset()[1]
                tmp
              }
            )
          )
          colnames(tab) <- s_attribute
        }
        return( data.table::as.data.table(tab) )
        
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
