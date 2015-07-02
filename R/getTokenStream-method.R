#' get token stream
#' 
#' @param .Object an object of classe \code{matrix} or \code{partition}
#' @param pAttribute the pAttribute to decode
#' @param encoding encoding to use
#' @param collapse character string length 1
#' @param corpus the CWB corpus
#' @param ... further arguments
#' @exportMethod getTokenStream
#' @rdname getTokenStream-method
setGeneric("getTokenStream", function(.Object, ...) standardGeneric("getTokenStream"))

#' @rdname getTokenStream-method
setMethod("getTokenStream", "matrix", function(.Object, corpus, pAttribute, encoding, collapse=NULL){
  cpos <- as.vector(unlist(apply(.Object, 1, function(row) c(row[1]:row[2]))))
  pAttr <- paste(corpus, ".", pAttribute, sep="")
  tokens <- cqi_id2str(pAttr, cqi_cpos2id(pAttr, cpos))
  tokens <- iconv(tokens, from=encoding, to="UTF-8")
  if (!is.null(collapse)){
    paste(tokens, collapse=collapse)  
  }
  return(tokens)
})

#' @rdname getTokenStream-method
setMethod("getTokenStream", "partition", function(.Object, pAttribute, collapse=NULL){
  getTokenStream(
    .Object=.Object@cpos, corpus=.Object@corpus, pAttribute=pAttribute,
    encoding=.Object@encoding, collapse=collapse
    )
})

# 
# getTokenStream2 <- function(x){
#   cpos <- as.vector(unlist(apply(x@cpos, 1, function(row) c(row[1]:row[2]))))
#   pAttr <- paste(x@corpus, ".", pAttribute, sep="")
#   token <- cqi_id2str(pAttr, cqi_cpos2id(pAttr, cpos))
#   token <- iconv(token, from=x@encoding, to="UTF-8")
#   paste(token, collapse="\n")
# }
