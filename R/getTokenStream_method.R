#' get token stream
#' 
#' @param .Object an object of classe \code{matrix} or \code{partition}
#' @param pAttribute the pAttribute to decode
#' @param encoding encoding to use
#' @param collapse character string length 1
#' @param corpus the CWB corpus
#' @param cpos logical, whether to return cpos as names of the tokens
#' @param ... further arguments
#' @exportMethod getTokenStream
#' @rdname getTokenStream-method
setGeneric("getTokenStream", function(.Object, ...) standardGeneric("getTokenStream"))

#' @rdname getTokenStream-method
setMethod("getTokenStream", "matrix", function(.Object, corpus, pAttribute, encoding, collapse=NULL, cpos=FALSE){
  cposVector <- as.vector(unlist(apply(.Object, 1, function(row) c(row[1]:row[2]))))
  pAttr <- paste(corpus, ".", pAttribute, sep="")
  tokens <- cqi_id2str(pAttr, cqi_cpos2id(pAttr, cposVector))
  tokens <- iconv(tokens, from=encoding, to="UTF-8")
  if (cpos == TRUE) names(tokens) <- cposVector
  if (!is.null(collapse)) tokens <- paste(tokens, collapse=collapse)  
  return(tokens)
})

#' @rdname getTokenStream-method
setMethod("getTokenStream", "partition", function(.Object, pAttribute, collapse=NULL, cpos=FALSE){
  getTokenStream(
    .Object=.Object@cpos, corpus=.Object@corpus, pAttribute=pAttribute,
    encoding=.Object@encoding, collapse=collapse, cpos=cpos
    )
})

