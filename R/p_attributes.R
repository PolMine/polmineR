#' @include S4classes.R
NULL

#' Get p-attributes.
#'
#' In a CWB corpus, every token has positional attributes. While s-attributes
#' cover a range of tokens, every single token in the token stream of a corpus
#' will have a set of positional attributes (such as part-of-speech, or lemma).
#' The available p-attributes are returned by the p_attributes-method.
#' 
#' @param .Object a character vector (length 1) or partition object
#' @param ... further arguments
#' @param p_attribute p-attribute to decode
#' @exportMethod p_attributes
#' @rdname p_attributes
#' @name p_attributes
#' @examples 
#' use("polmineR")
#' p_attributes("GERMAPARLMINI")
#' @references Stefan Evert & The OCWB Development Team, CQP Query Language Tutorial, http://cwb.sourceforge.net/files/CQP_Tutorial.pdf.
setGeneric("p_attributes", function(.Object, ...) standardGeneric("p_attributes"))

#' @rdname p_attributes
setMethod("p_attributes", "character", function(.Object, p_attribute = NULL, ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  pAttrs <- registry_get_p_attributes(.Object)
  if (is.null(p_attribute)){
    return( pAttrs )
  } else {
    if (p_attribute %in% pAttrs){
      tokens <- CQI$id2str(.Object, p_attribute, c(0:(CQI$lexicon_size(.Object, p_attribute) - 1)))
      tokens <- as.nativeEnc(tokens, from = registry_get_encoding(.Object))
      return(tokens)
    } else {
      stop("p_attribute provided is not available")
    }
  }
})

#' @details The \code{p_attributes}-method returns the p-attributes defined for the
#' corpus the partition is derived from, if argument \code{p_attribute} is \code{NULL}
#' (the default). If \code{p_attribute} is defined, the unique values for the p-attribute
#' are returned.
#' @rdname partition_class
setMethod("p_attributes", "partition", function(.Object, p_attribute = NULL, ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  pAttrs <- registry_get_p_attributes(.Object@corpus)
  if (is.null(p_attribute)){
    return( pAttrs )
  } else {
    if (p_attribute %in% pAttrs){
      if (p_attribute %in% .Object@p_attribute && length(p_attribute) == 1){
        return(.Object@stat[[p_attribute]])
      } else {
        return(unique(get_token_stream(.Object)))
      }
    } else {
      stop("p-attribute provided is not available")
    }
  }
})

#' @rdname context-class
setMethod("p_attributes", "context", function(.Object) .Object@p_attribute)