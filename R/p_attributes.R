#' @include S4classes.R
NULL

#' Get p-attributes.
#'
#' In a CWB corpus, every token has positional attributes. While s-attributes
#' cover a range of tokens, every single token in the token stream of a corpus
#' will have a set of positional attributes (such as part-of-speech, or lemma).
#' The available p-attributes are returned by the \code{p_attributes}-method.
#' 
#' @param .Object A length-one \code{character} vector, or a \code{partition} object.
#' @param ... further arguments
#' @param p_attribute A p-attribute to decode, provided by a length-one
#'   \code{character} vector.
#' @exportMethod p_attributes
#' @rdname p_attributes
#' @name p_attributes
#' @examples 
#' use("polmineR")
#' p_attributes("GERMAPARLMINI")
#' p_attributes("REUTERS")
#' p_attributes("REUTERS", p_attribute = "word")
#' @references Stefan Evert & The OCWB Development Team, CQP Query Language Tutorial, http://cwb.sourceforge.net/files/CQP_Tutorial.pdf.
setGeneric("p_attributes", function(.Object, ...) standardGeneric("p_attributes"))

#' @rdname p_attributes
setMethod("p_attributes", "character", function(.Object, p_attribute = NULL, ...){
  p_attributes(.Object = corpus(.Object), p_attribute = p_attribute, ...)
})


#' @rdname p_attributes
setMethod("p_attributes", "corpus", function(.Object, p_attribute = NULL, ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  p_attrs <- registry_get_p_attributes(corpus = .Object@corpus)
  if (is.null(p_attribute)){
    return( p_attrs )
  } else {
    if (p_attribute %in% p_attrs){
      tokens <- get_token_stream(.Object = .Object, p_attribute = p_attribute, ...)
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
setMethod("p_attributes", "slice", function(.Object, p_attribute = NULL, ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  p_attrs <- registry_get_p_attributes(.Object@corpus)
  if (is.null(p_attribute)){
    return( p_attrs )
  } else {
    if (p_attribute %in% p_attrs){
      if (p_attribute %in% .Object@p_attribute && length(p_attribute) == 1L){
        return(.Object@stat[[p_attribute]])
      } else {
        return(unique(get_token_stream(.Object)))
      }
    } else {
      stop("p-attribute provided is not available")
    }
  }
})

#' @rdname partition_class
setMethod("p_attributes", "partition", function(.Object, p_attribute = NULL, ...) callNextMethod())

#' @rdname partition_class
setMethod("p_attributes", "subcorpus", function(.Object, p_attribute = NULL, ...) callNextMethod())


#' @rdname context-class
setMethod("p_attributes", "context", function(.Object) .Object@p_attribute)