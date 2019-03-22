#' @include regions.R S4classes.R
NULL

#' Get Token Stream Based on Corpus Positions.
#' 
#' Turn regions of a corpus defined by corpus positions into the original text.
#' 
#' @param .Object An object of class \code{matrix} or \code{partition}
#' @param p_attribute The p-attribute to decode.
#' @param encoding Encoding to use.
#' @param collapse Length-one character string.
#' @param corpus The CWB corpus.
#' @param beautify Logical, whether to adjust whitespace before and after interpunctation.
#' @param decode Logical, whether to decode token ids to character strings.
#' @param left Left corpus position.
#' @param right Right corpus position.
#' @param cpos Logical, whether to return cpos as names of the tokens.
#' @param cutoff Maximum number of tokens to be reconstructed.
#' @param ... Further arguments.
#' @exportMethod get_token_stream
#' @rdname get_token_stream-method
#' @examples 
#' get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word")
#' get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word", collapse = " ")
#' fulltext <- get_token_stream("GERMAPARLMINI", p_attribute = "word")
setGeneric("get_token_stream", function(.Object, ...) standardGeneric("get_token_stream"))

#' @rdname get_token_stream-method
setMethod("get_token_stream", "numeric", function(.Object, corpus, p_attribute, encoding = NULL, collapse = NULL, beautify = TRUE, cpos = FALSE, cutoff = NULL, decode = TRUE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  # apply cutoff if length of cpos exceeds maximum number of tokens specified by cutoff
  if (!is.null(cutoff)) if (cutoff < length(.Object)) .Object <- .Object[1L:cutoff]
  
  if (decode){
    tokens <- cl_cpos2str(corpus = corpus, p_attribute = p_attribute, cpos = .Object, registry = registry())
  } else {
    return( cl_cpos2id(corpus = corpus, p_attribute = p_attribute, cpos = .Object, registry = registry()) )
  }
  
  if (!is.null(encoding)){
    Encoding(tokens) <- encoding
    tokens <- as.nativeEnc(tokens, from = encoding)
  }
  if (cpos) names(tokens) <- .Object
  if (!is.null(collapse)) {
    if (beautify){
      pos <- cl_cpos2str(corpus = corpus, p_attribute = "pos", cpos = .Object, registry = registry())
      whitespace <- rep(collapse, times = length(.Object))
      whitespace[grep("\\$[\\.;,:!?]", pos, perl = TRUE)] <- ""
      whitespace[grep("\\)", tokens, perl = TRUE)] <- ""
      whitespace[grep("\\(", tokens, perl = TRUE) + 1L] <- ""
      whitespace[1] <- ""
      tokens <- paste(paste(whitespace, tokens, sep = ""), collapse = "")
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
setMethod("get_token_stream", "corpus", function(.Object, left = NULL, right = NULL, ...){
  if (is.null(left)) left <- 0L
  if (is.null(right)) right <- size(.Object) - 1L
  get_token_stream(left:right, corpus = .Object@corpus, encoding = encoding(.Object), ...)
})


#' @rdname get_token_stream-method
setMethod("get_token_stream", "character", function(.Object, left = NULL, right = NULL, ...){
  get_token_stream(corpus(.Object), left = left, right = right, ...)
})


#' @rdname get_token_stream-method
setMethod("get_token_stream", "slice", function(.Object, p_attribute, collapse = NULL, cpos = FALSE, ...){
  get_token_stream(
    .Object = .Object@cpos, corpus = .Object@corpus, p_attribute = p_attribute,
    encoding = .Object@encoding, collapse = collapse, cpos = cpos,
    ...
    )
})

#' @rdname get_token_stream-method
setMethod("get_token_stream", "partition", function(.Object, p_attribute, collapse = NULL, cpos = FALSE, ...)
  callNextMethod()
)


#' @rdname get_token_stream-method
setMethod("get_token_stream", "subcorpus", function(.Object, p_attribute, collapse = NULL, cpos = FALSE, ...)
  callNextMethod()
)


#' @rdname get_token_stream-method
setMethod("get_token_stream", "regions", function(.Object, p_attribute = "word", collapse = NULL, cpos = FALSE, ...){
  get_token_stream(
    .Object = .Object@cpos, corpus = .Object@corpus, p_attribute = p_attribute,
    encoding = .Object@encoding, collapse = collapse, cpos = cpos,
    ...
  )
  # 
  # .getText <- function(.BY){
  #   list(text = get_token_stream(
  #     .BY[[1]]:.BY[[2]],
  #     corpus = .Object@corpus, encoding = .Object@encoding,
  #     p_attribute = p_attribute,
  #     collapse = " ",
  #     ...
  #   ))
  # }
  # .Object@cpos[, .getText(.BY), by = c("cpos_left", "cpos_right"), with = TRUE]
})


setOldClass("String")

#' Decode as String.
#'
#' @examples
#' use("polmineR")
#' p <- partition("GERMAPARLMINI", date = "2009-11-10", speaker = "Angela Dorothea Merkel")
#' s <- as(p, "String")
#' @name partition_to_string
setAs(from = "slice", to = "String", def = function(from){
  word <- get_token_stream(from, p_attribute = "word")
  whitespace_after <- c(ifelse(word %in% c(".", ",", ":", "!", "?"), FALSE, TRUE)[2L:length(word)], FALSE)
  word_with_whitespace <- paste(word, ifelse(whitespace_after, " ", ""), sep = "")
  y <- paste(word_with_whitespace, collapse = "")
  # to avoid importing the NLP packgage (with its rJava dependency), the following 
  # lines are adapted from the .String_from_string() auxiliary function of the NLP package.
  y <- enc2utf8(y)
  class(y) <- "String"
  y
})

