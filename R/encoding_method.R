#' get/set encoding slot of an object
#' 
#' Method for partition, partitionBundle objects.
#' 
#' @param object the object with an 'encoding'-slot
#' @param value value to be assigned
#' @rdname encoding
#' @exportMethod encoding
setGeneric("encoding", function(object) standardGeneric("encoding"))

#' @rdname encoding
setGeneric("encoding<-", function(object, value) standardGeneric("encoding<-"))

#' @rdname encoding
setMethod("encoding", "partition", function(object) object@encoding)

#' @rdname encoding
setMethod("encoding", "partitionBundle", function(object) object@encoding)
