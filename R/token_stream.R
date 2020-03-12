#' @include regions.R S4classes.R
NULL

#' Get Token Stream.
#'
#' Auxiliary method to get the fulltext of a corpus, subcorpora etc. Can be used
#' to export corpus data to other tools.
#'
#' @param .Object Input object.
#' @param p_attribute A length-one \code{character} vector, the p-attribute to
#'   decode.
#' @param phrases A \code{phrases} object. Defined phrases will be concatenated.
#' @param subset An expression applied on p-attributes, using non-standard
#'   evaluation. Note that symbols used in the expression may not be used
#'   internally (e.g. 'stopwords').
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
#' @param cpos A \code{logical} value, whether to return corpus positions as
#'   names of the tokens.
#' @param cutoff Maximum number of tokens to be reconstructed.
#' @param progress A length-one \code{logical} value, whether to show progress
#'   bar.
#' @param verbose A length-one \code{logical} value, whether to show messages.
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
#' @details The primary usage of this method is to return the token stream of a
#'   (sub-)corpus as defined by a \code{corpus}, \code{subcorpus} or
#'   \code{partition} object. The methods defined for a \code{numeric} vector or
#'   a (two-column) \code{matrix} defining regions (i.e. left and right corpus
#'   positions in the first and second column) are the actual workers for this
#'   operation.
#' @details The \code{get_token_stream} has been introduced so serve as a worker
#'   by higher level methods such as \code{read}, \code{html}, and
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

#' @inheritParams decode
#' @rdname get_token_stream-method
setMethod("get_token_stream", "numeric", function(.Object, corpus, p_attribute, subset = NULL, boost = NULL, encoding = NULL, collapse = NULL, beautify = TRUE, cpos = FALSE, cutoff = NULL, decode = TRUE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  # apply cutoff if length of cpos exceeds maximum number of tokens specified by cutoff
  if (!is.null(cutoff)) if (cutoff < length(.Object)) .Object <- .Object[1L:cutoff]
  
  ids <- cl_cpos2id(corpus = corpus, p_attribute = p_attribute, cpos = .Object, registry = registry())
  if (isTRUE(decode)){
    tokens <- decode(.Object = ids, corpus = corpus, p_attributes = p_attribute, boost = boost)
    rm(ids)
  } else if (isFALSE(decode)){
    return(ids)
  }
  
  expr <- substitute(subset)
  if (!is.null(expr)){
    assign(p_attribute, tokens)
    assign(p_attribute, get(p_attribute)[eval(expr)])
    tokens <- get(p_attribute)
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
  get_token_stream(cpos(.Object), ...)
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
#' # Get token stream for partition_bundle
#' pb <- partition_bundle("REUTERS", s_attribute = "id")
#' ts_list <- get_token_stream(pb)
#' 
#' # Workflow to filter decoded subcorpus_bundle
#' \dontrun{
#' sp <- corpus("GERMAPARLMINI") %>% as.speeches(s_attribute_name = "speaker", progress = FALSE)
#' queries <- c('"freiheitliche" "Grundordnung"', '"Bundesrepublik" "Deutschland"' )
#' phr <- corpus("GERMAPARLMINI") %>% cpos(query = queries) %>% as.phrases(corpus = "GERMAPARLMINI")
#' 
#' kill <- tm::stopwords("de")
#'
#' ts_phr <- get_token_stream(
#'   sp,
#'   p_attribute = c("word", "pos"),
#'   subset = {!word %in% kill  & !grepl("(\\$.$|ART)", pos)},
#'   phrases = phr,
#'   progress = FALSE,
#'   verbose = FALSE
#' )
#' }
setMethod("get_token_stream", "partition_bundle", function(.Object, p_attribute = "word", phrases = NULL, subset = NULL, collapse = NULL, cpos = FALSE, decode = TRUE, verbose = TRUE, progress = FALSE, mc = FALSE, ...){

  corpus_id <- get_corpus(.Object)
  if (length(corpus_id) > 1L) stop("Objects in bundle not derived from the same corpus.")
  
  if (verbose) message("... creating vector of document ids")
  sizes <- sapply(.Object@objects, slot, "size")
  id_list <- lapply(seq_along(.Object), function(i) rep(x = i, times = sizes[[i]]))
  obj_id <- do.call(c, id_list)
  rm(id_list)
  
  if (verbose) message("... creating vector of corpus positions")
  region_matrix_list <- lapply(.Object@objects, slot, "cpos")
  region_matrix <- do.call(rbind, region_matrix_list)
  cpos_vec <- cpos(region_matrix)
  rm(region_matrix, region_matrix_list)

  if (is.null(phrases)){
    if (verbose) message("... decoding character vectors")
    p_attr <- get_token_stream(cpos_vec, corpus = corpus_id, encoding = encoding(.Object), p_attribute = p_attribute, decode = decode)
    if (cpos) names(p_attr) <- cpos_vec
    if (verbose) message("... generating list of character vectors")
    if (!is.null(subset)) stop("using subset is not yet implemented")
    y <- split(x = p_attr, f = obj_id)
    names(y) <- names(.Object@objects)[unique(obj_id)] # subsetting may have removed objs
  } else {
    
    if (isTRUE(cpos)) stop("Argument 'cpos' is TRUE, but assigning corpus positions nonsensical when concatenating phrases.")

    dt <- data.table(obj_id = obj_id, cpos = cpos_vec)
    
    for (p_attr in p_attribute){
      if (verbose) message("... decoding token stream for p-attribute ", p_attr)
      dt[, (p_attr) := get_token_stream(cpos_vec, corpus = corpus_id, encoding = encoding(.Object), p_attribute = p_attr)]
    }

    if (verbose) message("... concatenate phrases")
    dt_phr <- concatenate_phrases(dt = dt, phrases = phrases, col = p_attribute[[1]])
    
    expr <- substitute(subset)
    if (!is.null(expr)){
      if (verbose) message("... applying argument subset")
      dt_phr <- dt_phr[eval(expr, envir = dt_phr, enclos = .GlobalEnv),]
    }

    if (length(p_attribute) > 1L){
      if (verbose) message("... concatenate multiple p-attributes")
      dt_phr[, (p_attribute[[1]]) := do.call(paste, c(lapply(p_attribute, function(p_attr) dt_phr[[p_attr]]), sep = "//"))]
    }
    
    if (verbose) message("... generating list of character vectors")
    y <- split(x = dt_phr[[p_attribute[[1]]]], f = dt_phr[["obj_id"]])
    names(y) <- names(.Object@objects)[unique(dt_phr[["obj_id"]])] # subsetting may have removed objs
  }
  if (!is.null(collapse)) y <- lapply(y, function(x) paste(x, collapse = collapse))
  y
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

