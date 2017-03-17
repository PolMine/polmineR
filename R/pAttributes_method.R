#' Get p-attributes.
#'
#' In a CWB corpus, every token has positional attributes. While s-attributes
#' cover a range of tokens, every single token in the token stream of a corpus
#' will have a set of positional attributes (such as part-of-speech, or lemma).
#' The available p-attributes are returned by the pAttributes-method.
#' 
#' @param .Object a character vector (length 1) or partition object
#' @param ... further arguments
#' @param pAttribute p-attribute to decode
#' @exportMethod pAttributes
#' @rdname pAttributes
#' @name pAttributes
#' @examples 
#' \dontrun{
#'    use("polmineR.sampleCorpus")
#'    pAttributes("PLPRBTTXT")
#' }
#' @references Stefan Evert & The OCWB Development Team, CQP Query Language Tutorial, http://cwb.sourceforge.net/files/CQP_Tutorial.pdf.
setGeneric("pAttributes", function(.Object, ...) standardGeneric("pAttributes"))

#' @rdname pAttributes
setMethod("pAttributes", "character", function(.Object, pAttribute = NULL){
  pAttrs <- RegistryFile$new(.Object)$getPAttributes()
  if (is.null(pAttribute)){
    return( pAttrs )
  } else {
    if (pAttribute %in% pAttrs){
      tokens <- CQI$id2str(.Object, pAttribute, c(0:(CQI$lexicon_size(.Object, pAttribute) - 1)))
      tokens <- as.utf8(tokens)
      return(tokens)
    } else {
      stop("pAttribute provided is not available")
    }
  }
})

#' @rdname partition-class
setMethod("pAttributes", "partition", function(.Object, pAttribute = NULL){
  pAttrs <- RegistryFile$new(.Object@corpus)$getPAttributes()
  if (is.null(pAttribute)){
    return( pAttrs )
  } else {
    if (pAttribute %in% pAttrs){
      if (pAttribute %in% .Object@pAttribute && length(pAttribute) == 1){
        return(.Object@stat[[pAttribute]])
      } else {
        return(unique(getTokenStream(.Object)))
      }
    } else {
      stop("pAttribute provided is not available")
    }
  }
})
