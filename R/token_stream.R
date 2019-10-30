#' @include regions.R S4classes.R
NULL

#' Get Token Stream.
#' 
#' Auxiliary method to get the fulltext of a corpus, subcorpora etc. Can be used
#' to export corpus data to other tools.
#' 
#' @param .Object Input object.
#' @param p_attribute A length-one \code{character} vector, the p-attribute to decode.
#' @param encoding If not \code{NULL} (default) a length-one \code{character}
#'   vector stating an encoding that will be assigned to the (decoded) token
#'   stream.
#' @param collapse If not \code{NULL} (default), a length-one \code{character}
#'   string passed into \code{paste} to collapse character vector into a single
#'   string.
#' @param corpus A CWB indexed corpus.
#' @param beautify A (length-one) \code{logical} value, whether to adjust
#'   whitespace before and after interpunctation.
#' @param decode A (length-one) \code{logical} value, whether to decode token
#'   ids to character strings. Defaults to \code{TRUE}, if \code{FALSE}, an
#'   integer vector with token ids is returned.
#' @param left Left corpus position.
#' @param right Right corpus position.
#' @param cpos A \code{logical} value, whether to return corpus positions as names of the tokens.
#' @param cutoff Maximum number of tokens to be reconstructed.
#' @param progress A length-one \code{logical} value, whether to show progress bar.
#' @param mc Number of cores to use. If \code{FALSE} (default), only one thread
#'   will be used.
#' @param ... Arguments that will be be passed into the
#'   \code{get_token_stream}-method for a \code{numeric} vector, the real
#'   worker.
#' @exportMethod get_token_stream
#' @rdname get_token_stream-method
#' @details CWB indexed corpora have a fixed order of tokens which is called the
#'   \emph{token stream}. Every token is assigned to a unique \emph{corpus
#'   position}, Subsets of the (entire) token stream defined by a left and a
#'   right corpus position are called \emph{regions}. The
#'   \code{get_token_stream}-method will extract the tokens (for regions) from a
#'   corpus.
#' @details The primary usage of this method is to return the token
#'   stream of a (sub-)corpus as defined by a \code{corpus}, \code{subcorpus} or
#'   \code{partition} object. The methods defined for a \code{numeric} vector or
#'   a (two-column) \code{matrix} defining regions (i.e. left and right corpus
#'   positions in the first and second column) are the actual workers for this
#'   operation.
#' @details The \code{get_token_stream} has been introduced so serve as a worker by
#'   higher level methods such as \code{read}, \code{html}, and
#'   \code{as.markdown}. It may however be useful for decoding a corpus so that 
#'   it can be exported to other tools.
#'   
#' @examples 
#' # Decode first words of GERMAPARLMINI corpus (first sentence)
#' get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word")
#' 
#' # Decode first sentence and collapse tokens into single string
#' get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word", collapse = " ")
#' 
#' # Decode regions defined by two-column matrix
#' region_matrix <- matrix(c(0,9,10,25), ncol = 2, byrow = TRUE)
#' get_token_stream(region_matrix, corpus = "GERMAPARLMINI", p_attribute = "word", encoding = "latin1")
#' 
#' # Use argument 'beautify' to remove surplus whitespace
#' get_token_stream(
#'   region_matrix,
#'   corpus = "GERMAPARLMINI",
#'   p_attribute = "word",
#'   encoding = "latin1",
#'   collapse = " ", beautify = TRUE
#' )
#' 
#' # Decode entire corpus (corpus object / specified by corpus ID)
#' fulltext <- get_token_stream("GERMAPARLMINI", p_attribute = "word")
#' corpus("GERMAPARLMINI") %>%
#'   get_token_stream(p_attribute = "word") %>%
#'   head()
#' 
#' # Decode subcorpus
#' corpus("REUTERS") %>%
#'   subset(id == "127") %>%
#'   get_token_stream(p_attribute = "word") %>%
#'   head()
#' 
#' # Decode partition_bundle
#' pb_tokstr <- corpus("REUTERS") %>%
#'   split(s_attribute = "id") %>%
#'   get_token_stream(p_attribute = "word")
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
    
    if (typeof(collapse) != "character")
      warning("Argument 'collapse' of the get_token_stream()-method is expected to be a character vector, but it isn't.")
    if (length(collapse) != 1L)
      stop("Argument 'collapse' of the get_token_stream()-method is expected to be a length-one vector, but length is > 1.")
    
    if (beautify){
      whitespace <- rep(collapse, times = length(.Object))
      if ("pos" %in% p_attributes(corpus)){
        pos <- cl_cpos2str(corpus = corpus, p_attribute = "pos", cpos = .Object, registry = registry())
        whitespace[grep("\\$[\\.;,:!?]", pos, perl = TRUE)] <- ""
      } else {
        whitespace[grep("^[\\.;,:!?]$", tokens, perl = TRUE)] <- ""
      }
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
})

#' @rdname get_token_stream-method
#' @examples 
#' 
#' # get token stream for partition_bundle
#' pb <- partition_bundle("REUTERS", s_attribute = "id")
#' ts_list <- get_token_stream(pb, progress = FALSE)
setMethod("get_token_stream", "partition_bundle", function(.Object, p_attribute = "word", collapse = NULL, cpos = FALSE, progress = FALSE, mc = FALSE, ...){
  .fn <- function (x) get_token_stream(x, p_attribute = p_attribute, collapse = collapse, cpos = cpos, ...)
  if (mc == FALSE) mc <- 1L
  if (progress)
    pblapply(.Object@objects, .fn)
  else 
    if (mc == 1L) lapply(.Object@objects, .fn) else mclapply(.Object@objects, .fn, mc.cores = mc)
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
  whitespace_after <- c(ifelse(word %in% c(".", ",", ":", "!", "?", ";"), FALSE, TRUE)[2L:length(word)], FALSE)
  word_with_whitespace <- paste(word, ifelse(whitespace_after, " ", ""), sep = "")
  y <- paste(word_with_whitespace, collapse = "")
  # to avoid importing the NLP packgage (with its rJava dependency), the following 
  # lines are adapted from the .String_from_string() auxiliary function of the NLP package.
  y <- enc2utf8(y)
  class(y) <- "String"
  y
})

