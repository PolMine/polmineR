#' @exportMethod freq
setGeneric("freq", function(.Object) standardGeneric("freq"))

#' @rdname dispersion-class
setMethod("freq", "dispersion", function(.Object) .Object@freq)

#' @rdname partition-class
setMethod("freq", "partition", function(.Object){
  if (length(.Object@pAttribute) == 0) stop("no counts available")
  # DT <- copy(.Object@stat)
  .Object@stat <- .Object@stat[, freq := .Object@stat[["count"]] / .Object@size]
  .Object
})
