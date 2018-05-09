#' Get the encoding of a corpus.
#' 
#' Parse the registry file and get the encoding of a corpus.
#' 
#' @param .Object the corpus name
#' @rdname getEncoding-method
#' @name getEncoding
#' @exportMethod getEncoding
setGeneric("getEncoding", function(.Object) standardGeneric("getEncoding"))

#' @rdname getEncoding-method
setMethod("getEncoding", "character", function(.Object) registry_get_encoding(.Object))

