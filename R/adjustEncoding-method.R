#' adjust encoding
#' 
#' make sure that encoding is equal to partition encoding
#' 
#' @param .Object the object
#' @param corpusEncoding the encoding of the corpus
#' @rdname adjustEncoding-method
#' @name adjustEncoding
#' @exportMethod adjustEncoding
setGeneric("adjustEncoding", function(.Object, ...) standardGeneric("adjustEncoding"))

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