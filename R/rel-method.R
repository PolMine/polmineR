#' @exportMethod rel
setGeneric("rel", function(x) standardGeneric("rel"))

#' @rdname dispersion-class
setMethod("rel", "dispersion", function(x) x@rel)
