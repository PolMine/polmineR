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
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' 
#' p_attributes("REUTERS")
#' p_attributes("REUTERS", p_attribute = "word")
#' @references Stefan Evert & The OCWB Development Team, CQP Query Language
#'   Tutorial, https://cwb.sourceforge.io/files/CQP_Tutorial.pdf.
setGeneric("p_attributes", function(.Object, ...) standardGeneric("p_attributes"))

#' @rdname p_attributes
setMethod("p_attributes", "character", function(.Object, p_attribute = NULL){
  p_attributes(.Object = corpus(.Object), p_attribute = p_attribute)
})


#' @rdname p_attributes
setMethod("p_attributes", "corpus", function(.Object, p_attribute = NULL){
  p_attrs <- corpus_p_attributes(
    corpus = .Object@corpus,
    registry = .Object@registry_dir
  )
  
  if (is.null(p_attribute)){
    return(p_attrs)
  } else {
    if (!p_attribute %in% p_attrs){
      stop(sprintf("The p-attribute '' is not available in corpus ''.", p_attribute, .Object@corpus))
    }
    lexfile <- fs::path(.Object@data_dir, sprintf("%s.lexicon", p_attribute))
    lexicon <- readBin(con = lexfile, what = character(), n = file.info(lexfile)$size)
    if (.Object@encoding != encoding()){
      lexicon <- stringi::stri_encode(lexicon, from = .Object@encoding, to = encoding())
    }
    return(lexicon)
  }
})


#' @examples
#' merkel <- partition("GERMAPARLMINI", speaker = "Merkel", regex = TRUE)
#' merkel_words <- p_attributes(merkel, "word")
#' @rdname p_attributes
setMethod("p_attributes", "slice", function(.Object, p_attribute = NULL, decode = TRUE){
  p_attrs <- corpus_p_attributes(
    .Object@corpus,
    registry = .Object@registry_dir
  )
  if (is.null(p_attribute)){
    return( p_attrs )
  } else {
    if (!p_attribute %in% p_attrs){
      stop(
        sprintf(
          "The p-attribute '' is not available in corpus ''.",
          p_attribute, .Object@corpus
        )
      )
    }
    ids <- cpos2id(.Object, p_attribute = p_attribute, cpos = cpos(.Object@cpos))
    ids_unique <- unique(ids)
    ids_unique <- ids_unique[order(ids_unique)]
    str <- cl_id2str(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      p_attribute = p_attribute, id = ids_unique
    )
    if (.Object@encoding != encoding()){
      str <- stringi::stri_encode(str, from = .Object@encoding, to = encoding())
    }
    return(str)
  }
})

#' @rdname p_attributes
setMethod("p_attributes", "partition_bundle", function(.Object, p_attribute = NULL, decode = TRUE){
  corpus_id <- unique(sapply(.Object@objects, slot, "corpus"))
  if (length(corpus_id) > 1L){
    stop(
      "Getting p-attributes for a corpus requires that objects ",
      "are derived from the same corpus."
    )
  }
  
  if (is.null(p_attribute)){
    p_attrs <- corpus_p_attributes(
      .Object@corpus,
      registry = .Object@registry_dir
    )
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


#' @rdname p_attributes
setMethod("p_attributes", "remote_corpus", function(.Object, ...){
  ocpu_exec(fn = "p_attributes", corpus = .Object@corpus, server = .Object@server, restricted = .Object@restricted, .Object = as(.Object, "corpus"), ...)
})


#' @rdname p_attributes
setMethod("p_attributes", "remote_partition", function(.Object, ...){
  ocpu_exec(fn = "p_attributes", corpus = .Object@corpus, server = .Object@server, restricted = .Object@restricted, .Object = as(.Object, "partition"), ...)
})

