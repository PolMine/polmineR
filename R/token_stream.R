#' @include regions.R S4classes.R
NULL

#' Get Token Stream Based on Corpus Positions.
#' 
#' Turn regions of a corpus defined by corpus positions into the original text.
#' 
#' @param .Object an object of class \code{matrix} or \code{partition}
#' @param p_attribute the p-attribute to decode
#' @param encoding encoding to use
#' @param collapse character string length 1
#' @param corpus the CWB corpus
#' @param beautify logical, whether to adjust whitespace before and after interpunctation
#' @param left left corpus position
#' @param right right corpus position
#' @param cpos logical, whether to return cpos as names of the tokens
#' @param cutoff maximum number of tokens to be reconstructed
#' @param ... further arguments
#' @exportMethod get_token_stream
#' @rdname get_token_stream-method
#' @examples 
#' get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word")
#' get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word", collapse = " ")
#' fulltext <- get_token_stream("GERMAPARLMINI", p_attribute = "word")
setGeneric("get_token_stream", function(.Object, ...) standardGeneric("get_token_stream"))

#' @rdname get_token_stream-method
setMethod("get_token_stream", "numeric", function(.Object, corpus, p_attribute, encoding = NULL, collapse = NULL, beautify = TRUE, cpos = FALSE, cutoff = NULL, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  # apply cutoff if length of cpos exceeds maximum number of tokens specified by cutoff
  if (!is.null(cutoff)) if (cutoff < length(.Object)) .Object <- .Object[1L:cutoff]
  tokens <- CQI$cpos2str(corpus, p_attribute, .Object)
  if (!is.null(encoding)){
    Encoding(tokens) <- encoding
    tokens <- as.nativeEnc(tokens, from = encoding)
  }
  if (cpos) names(tokens) <- .Object
  if (!is.null(collapse)) {
    if (beautify){
      pos <- CQI$cpos2str(corpus, "pos", .Object)
      whitespace <- rep(collapse, times = length(.Object))
      whitespace[grep("\\$[\\.;,:!?]", pos, perl = TRUE)] <- ""
      whitespace[grep("\\)", tokens, perl = TRUE)] <- ""
      whitespace[grep("\\(", tokens, perl = TRUE) + 1L] <- ""
      whitespace[1] <- ""
      tokens <- paste(paste(whitespace, tokens, sep = ""), collapse="")
    } else {
      tokens <- paste(tokens, collapse = collapse)  
    }
  }
  tokens
})

#' @rdname get_token_stream-method
setMethod("get_token_stream", "matrix", function(.Object, ...){
  cpos_vector <- as.vector(unlist(apply(.Object, 1L, function(row) row[1L]:row[2L])))
  get_token_stream(cpos_vector, ...)
})


#' @rdname get_token_stream-method
setMethod("get_token_stream", "character", function(.Object, left = NULL, right = NULL, ...){
  if (is.null(left)) left <- 0L
  if (is.null(right)) right <- size(.Object) - 1L
  get_token_stream(left:right, corpus = .Object, ...)
})

#' @rdname get_token_stream-method
setMethod("get_token_stream", "partition", function(.Object, p_attribute, collapse = NULL, cpos = FALSE, ...){
  get_token_stream(
    .Object = .Object@cpos, corpus = .Object@corpus, p_attribute = p_attribute,
    encoding = .Object@encoding, collapse = collapse, cpos = cpos,
    ...
    )
})

#' @rdname get_token_stream-method
setMethod("get_token_stream", "regions", function(.Object, p_attribute = "word", ...){
  
  .getText <- function(.BY){
    list(text = get_token_stream(
      .BY[[1]]:.BY[[2]],
      corpus = .Object@corpus, encoding = .Object@encoding,
      p_attribute = p_attribute,
      collapse = " ",
      ...
    ))
  }
  .Object@cpos[, .getText(.BY), by = c("cpos_left", "cpos_right"), with = TRUE]
})

