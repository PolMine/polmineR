#' get token stream
#' 
#' @param .Object an object of classe \code{matrix} or \code{partition}
#' @param pAttribute the pAttribute to decode
#' @param encoding encoding to use
#' @param collapse character string length 1
#' @param corpus the CWB corpus
#' @param beautify logical, whether to correct whitespace before and after interpunctation
#' @param left left corpus position
#' @param right right corpus position
#' @param cpos logical, whether to return cpos as names of the tokens
#' @param ... further arguments
#' @exportMethod getTokenStream
#' @rdname getTokenStream-method
setGeneric("getTokenStream", function(.Object, ...) standardGeneric("getTokenStream"))

#' @rdname getTokenStream-method
setMethod("getTokenStream", "numeric", function(.Object, corpus, pAttribute, encoding=NULL, collapse=NULL, beautify=TRUE, cpos=FALSE){
  # pAttr <- paste(corpus, pAttribute, sep=".")
  tokens <- CQI$cpos2str(corpus, pAttribute, .Object)
  if (!is.null(encoding)){
    Encoding(tokens) <- encoding
    tokens <- iconv(tokens, from=encoding, to="UTF-8")
  }
  if (cpos == TRUE) names(tokens) <- .Object
  if (!is.null(collapse)) {
    if (beautify == TRUE){
      pos <- CQI$cpos2str(corpus, "pos", .Object)
      whitespace <- rep(collapse, times = length(.Object))
      whitespace[grep("\\$[\\.;,:!?]", pos, perl=T)] <- ""
      whitespace[grep("\\)", tokens, perl=T)] <- ""
      whitespace[grep("\\(", tokens, perl=T) + 1] <- ""
      whitespace[1] <- ""
      tokens <- paste(paste(whitespace, tokens, sep=""), collapse="")
    } else {
      tokens <- paste(tokens, collapse=collapse)  
    }
  }
  tokens
})

#' @rdname getTokenStream-method
setMethod("getTokenStream", "matrix", function(.Object, ...){
  cposVector <- as.vector(unlist(apply(.Object, 1, function(row) c(row[1]:row[2]))))
  getTokenStream(cposVector, ...)
})


#' @rdname getTokenStream-method
setMethod("getTokenStream", "character", function(.Object, left, right, ...){
  getTokenStream(c(left:right), corpus=.Object, ...)
})

#' @rdname getTokenStream-method
setMethod("getTokenStream", "partition", function(.Object, pAttribute, collapse=NULL, cpos=FALSE){
  getTokenStream(
    .Object=.Object@cpos, corpus=.Object@corpus, pAttribute=pAttribute,
    encoding=.Object@encoding, collapse=collapse, cpos=cpos
    )
})

