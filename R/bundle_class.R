#' @include bundle_class.R textstat_class.R
NULL

setGeneric("as.bundle", function(object,...){standardGeneric("as.bundle")})


#' bundle class
#' 
#' A class to bundle several objects (partition, context, comp, cooccurrences objects)
#' in one S4 object.
#' 
#' @slot objects Object of class \code{"list"}
#' @slot pAttribute Object of class \code{"character"}
#' 
#' @param x a bundle object
#' @param i index to address an object
#' @param object a bundle object
#' @param col columsn of the table to be used
#' @param value ...
#' @rdname bundle-class
#' @name bundle-class
#' @exportClass bundle
#' @docType class
#' @author Andreas Blaette
setClass("bundle",
         representation(
           objects="list",
           pAttribute="character"
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
#' @exportMethod names<-
setReplaceMethod(
  "names",
  signature=c(x="bundle", value="character"),
  function(x, value) {
    if ( length(value) != length(x@objects) ) {
      warning("length of value provided does not match number of partitions")
      stop()
    }
    if ( !is.character(name(x)) ){
      warning("value needs to be a character vector")
      stop()
    }
    for (i in c(1:length(x@objects))){
      x@objects[[i]]@name <- value[i]
    }
    names(x@objects) <- value
    x
  }
)


#' @rdname bundle-class
setMethod("unique", "bundle", function(x){
  objectNames <- names(x)
  uniqueObjectNames <- unique(objectNames)
  uniquePos <- sapply(uniqueObjectNames, function(x) grep(x, objectNames)[1])
  objectsToDrop <- which(c(1:length(objectNames)) %in% uniquePos == FALSE)
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

#' @exportMethod +
#' @rdname bundle-class
setMethod("+", signature(e1="bundle", e2="textstat"), function(e1, e2){
  e1@objects[[length(e1@objects)+1]] <- e2
  names(e1@objects)[length(e1@objects)] <- e2@name
  e1
})



#' @exportMethod [[
#' @rdname bundle-class
setMethod('[[', 'bundle', function(x,i){
  return(x@objects[[i]])
}
)

#' @rdname bundle-class
setMethod("as.matrix", "bundle", function(x, col) {
  as.matrix(as.TermDocumentMatrix(x, col))
})
