#' Get p-attributes.
#'
#' In a CWB corpus, every token has positional attributes. While s-attributes
#' cover a range of tokens, every single token in the token stream of a corpus
#' will have a set of positional attributes (such as part-of-speech, or lemma).
#' The available p-attributes are returned by the pAttributes-method.
#' 
#' @param .Object a character vector (length 1) or partition object
#' @exportMethod pAttributes
#' @rdname pAttributes
#' @name pAttributes
#' @examples 
#' \dontrun{
#'    use("polmineR.sampleCorpus")
#'    pAttributes("PLPRBTTXT")
#' }
#' @references Stefan Evert & The OCWB Development Team, CQP Query Language Tutorial, http://cwb.sourceforge.net/files/CQP_Tutorial.pdf.
setGeneric("pAttributes", function(.Object) standardGeneric("pAttributes"))

#' @rdname pAttributes
setMethod("pAttributes", "character", function(.Object){
  parseRegistry(.Object)$pAttributes
})

#' @rdname partition-class
setMethod("pAttributes", "partition", function(.Object){
  parseRegistry(.Object@corpus)$pAttributes
})
