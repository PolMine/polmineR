#' @include regions_class.R
NULL

#' Get Token Stream Based on Corpus Positions.
#' 
#' Turn regions of a corpus defined by corpus positions into the original text.
#' 
#' @param .Object an object of class \code{matrix} or \code{partition}
#' @param pAttribute the pAttribute to decode
#' @param encoding encoding to use
#' @param collapse character string length 1
#' @param corpus the CWB corpus
#' @param beautify logical, whether to adjust whitespace before and after interpunctation
#' @param left left corpus position
#' @param right right corpus position
#' @param cpos logical, whether to return cpos as names of the tokens
#' @param cutoff maximum number of tokens to be reconstructed
#' @param ... further arguments
#' @exportMethod getTokenStream
#' @rdname getTokenStream-method
setGeneric("getTokenStream", function(.Object, ...) standardGeneric("getTokenStream"))

#' @rdname getTokenStream-method
setMethod("getTokenStream", "numeric", function(.Object, corpus, pAttribute, encoding = NULL, collapse = NULL, beautify = TRUE, cpos = FALSE, cutoff = NULL){
  if (!is.null(cutoff)) .Object <- .Object[1:cutoff]
  tokens <- CQI$cpos2str(corpus, pAttribute, .Object)
  if (!is.null(encoding)){
    Encoding(tokens) <- encoding
    tokens <- as.nativeEnc(tokens, from = encoding)
  }
  if (cpos) names(tokens) <- .Object
  if (!is.null(collapse)) {
    if (beautify){
      pos <- CQI$cpos2str(corpus, "pos", .Object)
      whitespace <- rep(collapse, times = length(.Object))
      whitespace[grep("\\$[\\.;,:!?]", pos, perl = T)] <- ""
      whitespace[grep("\\)", tokens, perl = T)] <- ""
      whitespace[grep("\\(", tokens, perl = T) + 1] <- ""
      whitespace[1] <- ""
      tokens <- paste(paste(whitespace, tokens, sep=""), collapse="")
    } else {
      tokens <- paste(tokens, collapse = collapse)  
    }
  }
  tokens
})

#' @rdname getTokenStream-method
setMethod("getTokenStream", "matrix", function(.Object, ...){
  cposVector <- as.vector(unlist(apply(.Object, 1, function(row) row[1]:row[2])))
  getTokenStream(cposVector, ...)
})


#' @rdname getTokenStream-method
setMethod("getTokenStream", "character", function(.Object, left = NULL, right = NULL, ...){
  if (is.null(left)) left <- 0
  if (is.null(right)) right <- size(.Object) - 1
  getTokenStream(left:right, corpus = .Object, ...)
})

#' @rdname getTokenStream-method
setMethod("getTokenStream", "partition", function(.Object, pAttribute, collapse = NULL, cpos = FALSE, ...){
  getTokenStream(
    .Object=.Object@cpos, corpus=.Object@corpus, pAttribute = pAttribute,
    encoding=.Object@encoding, collapse = collapse, cpos = cpos, ...
    )
})

#' @rdname getTokenStream-method
setMethod("getTokenStream", "regions", function(.Object, pAttribute = "word", ...){
  .getText <- function(.BY){
    list(text = getTokenStream(
      .BY[[1]]:.BY[[2]],
      corpus = .Object@corpus, encoding = .Object@encoding,
      pAttribute = "word",
      collapse = " ",
      ...
    ))
  }
  .Object@cpos[, .getText(.BY), by = c("cpos_left", "cpos_right"), with = TRUE]
})

