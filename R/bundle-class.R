#' bundle class
#' 
#' A class to bundle several objects (partition, context, keyness, collocations objects)
#' in one S4 object.
#' 
#' @slot objects Object of class \code{"list"}
#' 
#' @param x a bundle object
#' @param i index to address an object
#'   
#' @rdname bundle-class
#' @name bundle-class
#' @exportClass bundle
#' @docType class
#' @author Andreas Blaette
setClass("bundle",
         representation(
           objects="list"
           )
)

#' @rdname bundle-class
#' @exportMethod [[
setMethod("[[", "bundle", function(x,i){
  return(x@objects[[i]])
})

#' @rdname bundle-class
setMethod("length", "bundle", function(x) length(x@objects))

#' @rdname bundle-class
setMethod("names", "bundle", function(x) names(x@objects))

#' @rdname bundle-class
setMethod("unique", "bundle", function(x){
  labels <- names(x)
  uniqueLabels <- unique(labels)
  uniquePos <- sapply(uniqueLabels, function(x) grep(x, labels)[1])
  objectsToDrop <- which(c(1:length(labels)) %in% uniquePos == FALSE)
  objectsToDrop <- objectsToDrop[order(objectsToDrop, decreasing=TRUE)]
  for (pos in objectsToDrop) x@objects[pos] <- NULL
  x
})

#' @rdname bundle-class
setGeneric("bapply", function(x, ...) standardGeneric("bapply"))

#' @param f a function that can be applied to each object contained in the bundle
#' @param ... further parameters
#' @rdname bundle-class
#' @exportMethod bapply
setMethod("bapply", "bundle", function(x, f, ...){
  x@objects <- lapply(X=x@objects, FUN=f, ...)
  x
})