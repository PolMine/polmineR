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
#' @param pAttribute p-attribute to decode
#' @exportMethod p_attributes
#' @rdname p_attributes
#' @name p_attributes
#' @examples 
#' use("polmineR")
#' p_attributes("GERMAPARLMINI")
#' @references Stefan Evert & The OCWB Development Team, CQP Query Language Tutorial, http://cwb.sourceforge.net/files/CQP_Tutorial.pdf.
setGeneric("p_attributes", function(.Object, ...) standardGeneric("p_attributes"))

#' @rdname p_attributes
setMethod("p_attributes", "character", function(.Object, pAttribute = NULL){
  pAttrs <- registry_get_p_attributes(.Object)
  if (is.null(pAttribute)){
    return( pAttrs )
  } else {
    if (pAttribute %in% pAttrs){
      tokens <- CQI$id2str(.Object, pAttribute, c(0:(CQI$lexicon_size(.Object, pAttribute) - 1)))
      tokens <- as.nativeEnc(tokens, from = registry_get_encoding(.Object))
      return(tokens)
    } else {
      stop("pAttribute provided is not available")
    }
  }
})

#' @rdname partition_class
setMethod("p_attributes", "partition", function(.Object, pAttribute = NULL){
  pAttrs <- registry_get_p_attributes(.Object@corpus)
  if (is.null(pAttribute)){
    return( pAttrs )
  } else {
    if (pAttribute %in% pAttrs){
      if (pAttribute %in% .Object@pAttribute && length(pAttribute) == 1){
        return(.Object@stat[[pAttribute]])
      } else {
        return(unique(get_token_stream(.Object)))
      }
    } else {
      stop("pAttribute provided is not available")
    }
  }
})

#' @rdname context-class
setMethod("p_attributes", "context", function(.Object) .Object@pAttribute)