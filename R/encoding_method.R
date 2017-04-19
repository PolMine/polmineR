#' Get and set encoding.
#' 
#' Method for \code{textstat} objects and classes inheriting from \code{textstat}.
#' 
#' @param object the object with an 'encoding'-slot
#' @param value value to be assigned
#' @rdname encoding
#' @exportMethod encoding
setGeneric("encoding", function(object) standardGeneric("encoding"))

#' @rdname encoding
setGeneric("encoding<-", function(object, value) standardGeneric("encoding<-"))

#' @rdname encoding
setMethod("encoding", "textstat", function(object) object@encoding)

#' @rdname encoding
setMethod("encoding", "bundle", function(object) object@encoding)
