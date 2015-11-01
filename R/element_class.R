#' virtual superclass for elementary classes
#' 
#' @exportClass element
#' @rdname element-class
#' @name element
setClassUnion(
  "element",
  c("partition", "keyness", "context", "cooccurrences")
)

#' @rdname element-class
#' @exportMethod as.bundle
setGeneric("as.bundle", function(object, ...) standardGeneric("as.bundle"))

setMethod("as.bundle", "element", function(object){
  new(
    paste(is(object)[1], "Bundle", sep=""),
    objects=setNames(list(object), object@name),
    corpus=object@corpus,
    encoding=object@encoding,
    explanation=c("derived from a partition object")
  )
})

#' @exportMethod +
#' @docType methods
#' @rdname element-class
setMethod("+", signature(e1="element", e2="element"), function(e1, e2){
  if (e1@corpus != e2@corpus) warning("Please be careful - partition is from a different CWB corpus")
  newBundle <- as.bundle(e1)
  newBundle@objects[[length(newBundle@objects)+1]] <- e2
  names(newBundle@objects)[length(newBundle@objects)] <- e2@name
  newBundle
})
