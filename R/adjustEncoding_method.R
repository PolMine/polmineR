#' adjust encoding
#' 
#' Utility method to help making sure that the encoding of a character string or the relevant parts of an object
#' conform to the encoding to the corpus. 
#' 
#' @param .Object the object (usually a character vector, nothing else is implemented so far)
#' @param corpusEncoding the encoding of the corpus (e.g. "latin1")
#' @param from encoding
#' @param ... further parameters
#' @rdname encodings
#' @name adjustEncoding
#' @exportMethod adjustEncoding
setGeneric("adjustEncoding", function(.Object, ...) standardGeneric("adjustEncoding"))

#' @rdname encodings
setMethod("adjustEncoding", "character", function(.Object, corpusEncoding){
  sapply(
    as.list(.Object),
    function(x) {
      enc <- Encoding(x)
      if ( enc != "unknown" && enc != corpusEncoding ) {
        x <- iconv(x, from=enc, to=corpusEncoding)
      }
      x
    }
  )
})

#' @exportMethod as.utf8
#' @rdname encodings
setGeneric("as.utf8", function(.Object, ...) standardGeneric("as.utf8"))

#' adjustEncoding-method
#' @rdname encodings
setMethod("as.utf8", "character", function(.Object, from="latin1"){
  Encoding(.Object) <- from
  retval <- enc2utf8(.Object)
  Encoding(retval) <- "unknown"
  retval
})