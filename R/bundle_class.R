#' bundle class
#' 
#' A class to bundle several objects (partition, context, keyness, cooccurrences objects)
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
  partitionNames <- names(x)
  uniquePartitionNames <- unique(partitionNames)
  uniquePos <- sapply(uniqueuniquePartitionNames, function(x) grep(x, partitionNames)[1])
  objectsToDrop <- which(c(1:length(partitionNames)) %in% uniquePos == FALSE)
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


#' @exportMethod +
#' @rdname bundle-class
#' @param e1 object 1
#' @param e2 object 2
#' @docType methods
setMethod("+", signature(e1="bundle", e2="bundle"), function(e1, e2){
  newObjectClass <- unique(c(is(e1)[1], is(e2)[1]))
  if (length(newObjectClass) > 1) stop("the two objects do not have the same length")
  new(
    newObjectClass,
    objects=c(e1@objects, e2@objects),
    corpus=unique(e1@corpus, e2@corpus),
    encoding=unique(e1@encoding, e2@encoding)
    )
})


