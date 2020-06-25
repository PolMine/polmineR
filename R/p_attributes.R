#' @include S4classes.R
NULL

#' Get p-attributes.
#'
#' In a CWB corpus, every token has positional attributes. While s-attributes
#' cover a range of tokens, every single token in the token stream of a corpus
#' will have a set of positional attributes (such as part-of-speech, or lemma).
#' The available p-attributes are returned by the \code{p_attributes}-method.
#' 
#' The \code{p_attributes}-method returns the p-attributes defined for the
#' corpus the partition is derived from, if argument \code{p_attribute} is
#' \code{NULL} (the default). If \code{p_attribute} is defined, the unique
#' values for the p-attribute are returned.
#' 
#' @param .Object A length-one \code{character} vector, or a \code{partition}
#'   object.
#' @param ... Arguments passed to \code{get_token_stream}.
#' @param p_attribute A p-attribute to decode, provided by a length-one
#'   \code{character} vector.
#' @param decode A length-one \code{logical} value. Whether to return decoded
#'   p-attributes or unique token ids.
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
setMethod("p_attributes", "character", function(.Object, p_attribute = NULL){
  p_attributes(.Object = corpus(.Object), p_attribute = p_attribute)
})


#' @rdname p_attributes
setMethod("p_attributes", "corpus", function(.Object, p_attribute = NULL){
  p_attrs <- registry_get_p_attributes(corpus = .Object@corpus)
  if (is.null(p_attribute)){
    return(p_attrs)
  } else {
    if (!p_attribute %in% p_attrs){
      stop(sprintf("The p-attribute '' is not available in corpus ''.", p_attribute, .Object@corpus))
    }
    lexfile <- file.path(.Object@data_dir, sprintf("%s.lexicon", p_attribute), fsep = "/")
    lexicon <- readBin(con = lexfile, what = character(), n = file.info(lexfile)$size)
    if (.Object@encoding != localeToCharset()[1]){
      lexicon <- stringi::stri_encode(lexicon, from = .Object@encoding, to = localeToCharset()[1])
    }
    return(lexicon)
  }
})


#' @rdname p_attributes
setMethod("p_attributes", "slice", function(.Object, p_attribute = NULL, decode = TRUE){
  p_attrs <- registry_get_p_attributes(.Object@corpus)
  if (is.null(p_attribute)){
    return( p_attrs )
  } else {
    if (!p_attribute %in% p_attrs){
      stop(sprintf("The p-attribute '' is not available in corpus ''.", p_attribute, .Object@corpus))
    }
    ids <- RcppCWB::cl_cpos2id(
      corpus = .Object@corpus,
      p_attribute = p_attribute,
      registry = registry,
      cpos = cpos(.Object@cpos)
    )
    ids_unique <- unique(ids)
    ids_unique <- ids_unique[order(ids_unique)]
    str <- cl_id2str(corpus = .Object@corpus, p_attribute = p_attribute, registry = registry(), id = ids_unique)
    if (corpus@encoding != localeToCharset()[1]){
      str <- stringi::stri_encode(str, from = corpus@encoding, to = localeToCharset()[1])
    }
    return(str)
  }
})

#' @rdname p_attributes
setMethod("p_attributes", "partition_bundle", function(.Object, p_attribute = NULL, decode = TRUE){
  corpus_id <- unique(sapply(.Object@objects, slot, "corpus"))
  if (length(corpus_id) > 1L) stop("Getting p-attributes for a corpus requires that objects ",
                                   "are derived from the same corpus.")
  p_attrs <- registry_get_p_attributes(.Object@corpus)
  if (is.null(p_attribute)){
    return(p_attrs)
  } else {
    y_pre <- get_token_stream(.Object, p_attribute = p_attribute, decode = decode)
    y <- lapply(y_pre, unique)
    return(y)
  }
})

#' @rdname partition_class
setMethod("p_attributes", "partition", function(.Object, p_attribute = NULL, decode = TRUE) callNextMethod())

#' @rdname partition_class
setMethod("p_attributes", "subcorpus", function(.Object, p_attribute = NULL, decode = TRUE) callNextMethod())


#' @rdname context-class
setMethod("p_attributes", "context", function(.Object) .Object@p_attribute)