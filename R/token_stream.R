#' @include regions.R S4classes.R
NULL

#' Get Token Stream.
#'
#' Auxiliary method to get the fulltext of a corpus, subcorpora etc. Can be used
#' to export corpus data to other tools.
#'
#' @param .Object Input object.
#' @param p_attribute A `character` vector, the p-attribute(s) to decode.
#' @param phrases A `phrases` object. Defined phrases will be concatenated.
#' @param subset An expression applied on p-attributes, using non-standard
#'   evaluation. Note that symbols used in the expression may not be used
#'   internally (e.g. 'stopwords').
#' @param encoding If not `NULL` (default) a length-one `character` vector
#'   stating an encoding that will be assigned to the (decoded) token stream.
#' @param collapse If not `NULL` (default), a length-one `character` string
#'   passed into `paste` to collapse character vector into a single string.
#' @param corpus A CWB indexed corpus.
#' @param beautify A (length-one) `logical` value, whether to adjust whitespace
#'   before and after interpunctation.
#' @param decode A (length-one) `logical` value, whether to decode token ids to
#'   character strings. Defaults to `TRUE`, if `FALSE`, an integer vector with
#'   token ids is returned.
#' @param split A `logical` value, whether to return a `character` vector (when
#'   `split` is `FALSE`, default) or a `list` of `character` vectors; each of
#'   these vectors will then represent the tokens of a region defined by a row
#'   in a regions matrix.
#' @param left Left corpus position.
#' @param right Right corpus position.
#' @param cpos A `logical` value, whether to return corpus positions as names of
#'   the tokens.
#' @param cutoff Maximum number of tokens to be reconstructed.
#' @param progress A length-one `logical` value, whether to show progress bar.
#' @param verbose A length-one `logical` value, whether to show messages.
#' @param mc Number of cores to use. If `FALSE` (default), only one thread will
#'   be used.
#' @param ... Arguments that will be be passed into the
#'   `get_token_stream`-method for a `numeric` vector, the real worker.
#' @exportMethod get_token_stream
#' @rdname get_token_stream-method
#' @details CWB indexed corpora have a fixed order of tokens which is called the
#'   \emph{token stream}. Every token is assigned to a unique \emph{corpus
#'   position}, Subsets of the (entire) token stream defined by a left and a
#'   right corpus position are called \emph{regions}. The
#'   `get_token_stream`-method will extract the tokens (for regions) from a
#'   corpus.
#' @details The primary usage of this method is to return the token stream of a
#'   (sub-)corpus as defined by a `corpus`, `subcorpus` or `partition` object.
#'   The methods defined for a `numeric` vector or a (two-column) `matrix`
#'   defining regions (i.e. left and right corpus positions in the first and
#'   second column) are the actual workers for this operation.
#' @details The `get_token_stream` has been introduced so serve as a worker by
#'   higher level methods such as `read`, `html`, and `as.markdown`. It may
#'   however be useful for decoding a corpus so that it can be exported to other
#'   tools.
#'
#' @examples
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' 
#' # Decode first words of GERMAPARLMINI corpus (first sentence)
#' get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word")
#'
#' # Decode first sentence and collapse tokens into single string
#' get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word", collapse = " ")
#'
#' # Decode regions defined by two-column integer matrix
#' region_matrix <- matrix(c(0L,9L,10L,25L), ncol = 2, byrow = TRUE)
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
  
  ids <- cl_cpos2id(
    corpus = corpus, registry = corpus_registry_dir(corpus),
    p_attribute = p_attribute, cpos = .Object
  )
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
      whitespace[1] <- ""
      whitespace[grep("^[\\.;,:!?\\)]$", tokens, perl = TRUE)] <- ""
      whitespace[grep("\\(", tokens, perl = TRUE) + 1L] <- ""
      tokens <- paste(paste(whitespace, tokens, sep = ""), collapse = "")
    } else {
      tokens <- paste(tokens, collapse = collapse)  
    }
  }
  tokens
})

#' @rdname get_token_stream-method
setMethod("get_token_stream", "matrix", function(.Object, split = FALSE, ...){
  ts_vec <- get_token_stream(ranges_to_cpos(.Object), ...)
  
  if (isFALSE(is.logical(split))) stop("'split' needs to be a logical value.")
  if (isFALSE(split)){
    return(ts_vec)
  }
  if (isTRUE(split)){
    breakpoints <- c(0L, cumsum(.Object[,2] - .Object[,1] + 1L))
    breaks_factor <- cut(x = 1L:length(ts_vec), breaks = breakpoints, include.lowest = TRUE)
    ts_list <- split(x = ts_vec, f = breaks_factor)
    return(ts_list)
  }
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
setMethod("get_token_stream", "regions", function(.Object, p_attribute = "word", collapse = NULL, cpos = FALSE, split = FALSE, ...){
  get_token_stream(
    .Object = .Object@cpos, corpus = .Object@corpus, p_attribute = p_attribute,
    encoding = .Object@encoding, collapse = collapse, cpos = cpos, split = split,
    ...
  )
})

#' @param min_length If not `NULL` (default), an `integer` value with minimum
#'   length of documents required to keep them in the `list` object that is 
#'   returned.
#' @rdname get_token_stream-method
#' @importFrom stringi stri_c
#' @importFrom RcppCWB region_matrix_to_ids
#' @examples 
#' \donttest{
#' # Get token stream for partition_bundle
#' pb <- partition_bundle("REUTERS", s_attribute = "id")
#' ts_list <- get_token_stream(pb)
#' 
#' # Use two p-attributes
#' sp <- corpus("GERMAPARLMINI") %>%
#'   as.speeches(s_attribute_name = "speaker", s_attribute_date = "date", progress = FALSE)
#' p2 <- get_token_stream(sp, p_attribute = c("word", "pos"), verbose = FALSE)
#' 
#' # Apply filter
#' p_sub <- get_token_stream(
#'   sp, p_attribute = c("word", "pos"),
#'   subset = {!grepl("(\\$.$|ART)", pos)}
#' )
#' 
#' # Concatenate phrases and apply filter
#' queries <- c('"freiheitliche" "Grundordnung"', '"Bundesrepublik" "Deutschland"' )
#' phr <- corpus("GERMAPARLMINI") %>%
#'   cpos(query = queries) %>%
#'   as.phrases(corpus = "GERMAPARLMINI")
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
setMethod("get_token_stream", "partition_bundle", function(.Object, p_attribute = "word", phrases = NULL, subset = NULL, min_length = NULL, collapse = NULL, cpos = FALSE, decode = TRUE, beautify = FALSE, verbose = TRUE, progress = FALSE, mc = FALSE, ...){
  
  if (verbose) cli_progress_step("creating vector of document ids")
  sizes <- sapply(.Object@objects, slot, "size")
  id_list <- mapply(rep, seq_along(.Object), sizes, SIMPLIFY = FALSE)
  dt <- data.table(obj_id = do.call(c, id_list))
  rm(id_list); gc()
  
  if (verbose) cli_progress_step("get region matrices and corpus positions")
  region_matrix <- do.call(rbind, lapply(.Object@objects, slot, "cpos"))
  if (!is.null(phrases)) dt[, "cpos" := ranges_to_cpos(region_matrix)]

  for (p_attr in p_attribute){
    if (verbose) cli_progress_step("decoding token stream for p-attribute {.val {p_attr}}")
    ids <- region_matrix_to_ids(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      p_attribute = p_attr, matrix = region_matrix
    )
    if (isTRUE(decode)){
      tokens <- id2str(x = .Object, p_attribute = p_attr, id = ids)
      rm(ids); gc()
      tokens <- iconv(x = tokens, from = .Object@encoding, to = encoding())
      
      if (beautify){
        whitespace <- rep(" ", times = length(tokens))
        whitespace[1] <- ""
        whitespace[cumsum(sizes[1:(length(sizes) - 1L)]) + 1L] <- ""
        whitespace[grep("^[\\.;,:!?\\)]$", tokens, perl = TRUE)] <- ""
        whitespace[grep("\\(", tokens, perl = TRUE) + 1L] <- ""
        tokens <- paste(whitespace, tokens, sep = "")
      }

      dt[, (p_attr) := tokens]
      rm(tokens); gc()
    } else {
      dt[, (p_attr) := ids]
      rm(ids); gc()
    }
  }
  
  if (!is.null(phrases)){
    
    if (isFALSE(decode))
      stop("concatenating phrases not possible when decode = FALSE")
    
    if (isTRUE(cpos))
      stop(
        "Argument 'cpos' is TRUE, but cannot assigning corpus positions ",
        "AND concatenate phrases."
      )
    if (verbose) cli_progress_step("concatenate phrases")
    dt <- concatenate_phrases(dt = dt, phrases = phrases, col = p_attribute[[1]])
  }

  expr <- substitute(subset)
  if (!is.null(expr)){
    if (verbose) cli_progress_step("applying argument subset")
    dt <- dt[eval(expr, envir = dt, enclos = .GlobalEnv),]
  }
  
  if (!is.null(min_length)){
    if (verbose) cli_progress_step("drop documents shorter than `min_length`")
    stopifnot(is.numeric(min_length), length(min_length) == 1L)
    dt_cnt <- dt[, .N, by = "obj_id"]
    ids_to_keep <- dt_cnt[dt_cnt[["N"]] >= min_length][["obj_id"]]
    dt <- dt[dt[["obj_id"]] %in% ids_to_keep]
  }
  
  if (length(p_attribute) > 1L){
    
    if (isFALSE(decode))
      stop("cannot concatenate multiple p-attributes when decode = FALSE")
    
    if (verbose) cli_progress_step("concatenate multiple p-attributes")
    merger <- do.call(
      paste,
      c(lapply(p_attribute, function(p_attr) dt[[p_attr]]), sep = "//")
    )
    dt[, (p_attribute[[1]]) := merger]
    rm(merger); gc()
  }
  
  if (verbose) cli_progress_step("generating list of character vectors")
  
  p_attr <- dt[[ p_attribute[[1]] ]]
  dt[, (p_attribute[[1]]) := NULL]
  gc()
  ids <- dt[["obj_id"]]
  rm(dt); gc()
  
  y <- split(x = p_attr, f = ids)
  rm(p_attr); gc()
  ids_unique <- unique(ids) # subsetting may have removed objs
  names(y) <- names(.Object@objects)[ids_unique]
  rm(ids_unique); gc()

  if (!is.null(collapse)){
    
    if (isFALSE(decode))
      stop("cannot collapse tokens when decode = FALSE")
    
    if (isTRUE(beautify)) collapse <- ""
    
    y <- if (progress){
      pblapply(y, function(x) stringi::stri_c(x, collapse = collapse), cl = mc)
    } else {
      lapply(y, function(x) stringi::stri_c(x, collapse = collapse))
    }
  }

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

