#' get availables pAttributes
#'
#' @param .Object a character string or partition object
#' @exportMethod pAttributes
#' @rdname pAttributes
#' @name pAttributes
setGeneric("pAttributes", function(.Object) standardGeneric("pAttributes"))

#' @rdname pAttributes
setMethod("pAttributes", "character", function(.Object){
  parseRegistry(.Object)$pAttributes
})

#' @rdname partition-class
setMethod("pAttributes", "partition", function(.Object){
  parseRegistry(.Object@corpus)$pAttributes
})
