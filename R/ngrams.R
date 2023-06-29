#' @include count.R S4classes.R
NULL



#' Get N-Grams
#' 
#' Count n-grams, either of words, or of characters.
#' 
#' @param .Object object of class `partition`
#' @param n Number of tokens (if `char` is `NULL`) or characters otherwise.
#' @param p_attribute the p-attribute to use (can be > 1)
#' @param char If `NULL`, tokens will be counted, else characters, keeping
#'   only those provided by a character vector
#' @param mc A `logical` value, whether to use multicore, passed into call
#'   to `blapply()`.
#' @inheritParams get_token_stream
#' @param progress A `logical` value.
#' @param ... Further arguments.
#' @exportMethod ngrams
#' @rdname ngrams
#' @name ngrams
#' @examples 
#' use("polmineR")
#' P <- partition("GERMAPARLMINI", date = "2009-10-27")
#' ngrm <- ngrams(P, n = 2, p_attribute = "word", char = NULL)
#' 
#' # a more complex scenario: get most frequent ADJA/NN-combinations
#' ngrm <- ngrams(P, n = 2, p_attribute = c("word", "pos"), char = NULL)
#' ngrm2 <- subset(
#'  ngrm,
#'  ngrm[["1_pos"]] == "ADJA"  & ngrm[["2_pos"]] == "NN"
#'  )
#' ngrm2@@stat[, "1_pos" := NULL][, "2_pos" := NULL]
#' ngrm3 <- sort(ngrm2, by = "count")
#' head(ngrm3)
setGeneric("ngrams", function(.Object, ...) standardGeneric("ngrams"))

#' @rdname ngrams
setMethod("ngrams", "partition", function(.Object, ...) callNextMethod(.Object, ...))

#' @rdname ngrams
setMethod("ngrams", "character", function(.Object, ...) callNextMethod(.Object, ...))

#' @rdname ngrams
setMethod("ngrams", "partition", function(.Object, n = 2, p_attribute = "word", char = NULL, progress = FALSE, mc = 1L, ...){
  ngrams(.Object = as(.Object, "subcorpus"), n = n, p_attribute = p_attribute, char = char, progress = progress, mc = mc, ...)
})

#' @rdname ngrams
setMethod("ngrams", "subcorpus", function(.Object, n = 2, p_attribute = "word", char = NULL, progress = FALSE, mc = 1L, ...){
  callNextMethod()
})

#' @rdname ngrams
setMethod("ngrams", "character", function(.Object, n = 2, p_attribute = "word", char = NULL, progress = FALSE, mc = 1L, ...){
  ngrams(.Object = corpus(.Object), n = n, p_attribute = p_attribute, char = char, progress = progress, mc = mc, ...)
})


#' @rdname ngrams
#' @examples 
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' dt <- decode("REUTERS", p_attribute = "word", s_attribute = character(), to = "data.table")
#' y <- ngrams(dt, n = 3L, p_attribute = "word")
setMethod("ngrams", "data.table", function(.Object, n = 2L, p_attribute = "word"){
  
  obj <- .Object[1L:(nrow(.Object) - n + 1L), p_attribute, with = FALSE]
  colnames(obj) <- paste(colnames(obj), "1", sep = "_")
  
  for (i in 2L:n){
    for (p_attr in p_attribute){
      new_col <- .Object[[p_attribute]][i:(nrow(.Object) - n + i)]
      obj[, paste(p_attr, i, sep = "_") := new_col]
    }  
  }

  cnt <- obj[, .N, by = c(eval(colnames(obj))), with = TRUE]
  setnames(cnt, "N", "count")
  
  setcolorder(cnt, neworder = c(colnames(cnt)[!colnames(cnt) %in% "count"], "count"))
  setorderv(cnt, cols = "count", order = -1L)
  new(
    "ngrams",
    n = as.integer(n),
    stat = cnt,
    p_attribute = p_attribute,
    corpus = NA_character_, encoding = NA_character_, size = -1L,
    name = NA_character_, annotation_cols = NA_character_
  )
})


#' @rdname ngrams
#' @importFrom pbapply pbsapply
setMethod("ngrams", "corpus", function(.Object, n = 2, p_attribute = "word", char = NULL, progress = FALSE, mc = 1L, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  if (is.null(char)){
    
    id_list_base <- lapply(
      setNames(p_attribute, p_attribute),
      function(p_attr) get_token_stream(.Object, p_attribute = p_attr, decode = FALSE)
    )
    
    id_list <- list()
    j <- 0L
    for (i in 1L:n){
      for (p_attr in p_attribute){
        j <- j + 1L
        id_list[[j]] <- id_list_base[[p_attr]][i:(length(id_list_base[[p_attr]]) - (n - i))]
        names(id_list)[j] <- paste("id", i, p_attr, sep = "_")
      }
    }
    
    DT <- data.table::as.data.table(id_list)
    TF <- DT[, .N, by = c(eval(colnames(DT))), with = TRUE]
    setnames(TF, "N", "count")
    p_attrs_cols <- rep(p_attribute, times = n)
    token_no <- unlist(lapply(1L:n, function(x) rep(x, times = length(p_attribute))))
    
    # convert ids to strings
    dummy <- lapply(
      1L:(n * length(p_attribute)),
      function(i){
        str_raw <- cl_id2str(
          corpus = .Object@corpus, registry = .Object@registry_dir,
          p_attribute = p_attrs_cols[i], id = TF[[i]]
        )
        Encoding(str_raw) <- encoding(.Object)
        str <- stringi::stri_enc_tonative(str_raw)
        # str <- as.nativeEnc(str_raw, from = encoding(.Object))
        TF[, eval(paste(p_attrs_cols[i], token_no[i], sep = "_")) := str , with = TRUE] 
      })
    
    # remove columns with ids
    lapply(
      grep("id_", colnames(TF), value = TRUE),
      function(x) TF[, eval(x) := NULL, with = TRUE]
    )
    setcolorder(TF, neworder = c(colnames(TF)[!colnames(TF) %in% "count"], "count"))
  } else {
    char_soup_base <- get_token_stream(.Object, p_attribute = p_attribute[1], collapse = "")
    TF <- ngrams(.Object = list(char_soup_base), n = n, char = char)[[1]]
    # TF <- .character_ngrams(x = char_soup_base, n = n, char = char)
  }
  
  y <- as(as(.Object, "corpus"), "ngrams")
  y@n = as.integer(n)
  y@size = as.integer(size(.Object))
  y@stat = TF
  y@name = name(.Object)
  y@p_attribute = if (is.null(char)) p_attribute else "ngram"
  y
})


#' @importFrom stats na.omit
#' @importFrom stringi stri_sub
#' @rdname ngrams
setMethod("ngrams", "list", function(.Object, n = 2, char = NULL, mc = FALSE, verbose = FALSE, progress = FALSE, ...){
  
  .character_ngrams <- function(x, n, char){
    # had tried stringi::stri_extract_all() - not faster
    if (char[1] != ""){
      splitted <- strsplit(x, "")[[1]]
      splitted_min <- ifelse(splitted %in% char, splitted, NA)
      x <- paste(na.omit(splitted_min), collapse = "")
    }
    ngrams <- stringi::stri_sub(x, from = 1L:(nchar(x) - n + 1L), to = n:nchar(x))
    dt <- data.table(ngram = ngrams)[, .N, by = "ngram"]
    setnames(dt, old = "N", new = "count")
    dt
  }
  
  if (progress){
    if (mc){
      dts <- pblapply(.Object, .character_ngrams, n = n, char = char, cl = mc)
    } else {
      dts <- pblapply(.Object, .character_ngrams, n = n, char = char, cl = NULL)
    }
  } else {
    if (verbose) cli_progress_step("generate ngrams")
    if (mc){
      dts <- mclapply(.Object, .character_ngrams, n = n, char = char, mc.cores = mc)
    } else {
      dts <- lapply(.Object, .character_ngrams, n = n, char = char)
    }
    if (verbose) cli_progress_done()
  }
  
  dts
})


#' @rdname ngrams
setMethod("ngrams", "partition_bundle", function(.Object, n = 2, char = NULL, vocab = NULL, p_attribute = "word", mc = FALSE, verbose = FALSE, progress = FALSE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  retval <- as(as(.Object, "corpus"), "bundle")
  
  if (is.null(char)){
    retval@objects <- blapply(
      .Object@objects, f = ngrams,
      n = n, p_attribute = p_attribute, char = char, mc = mc, progress = progress
    )
    retval@p_attribute <- unique(unlist(lapply(retval@objects, function(x) x@p_attribute)))
  } else {
    retval@p_attribute <- p_attribute
    if (verbose) cli_progress_step("decoding token stream")
    li <- get_token_stream(
      .Object,
      p_attribute = p_attribute[1],
      collapse = "",
      vocab = vocab,
      verbose = FALSE
    )
    if (verbose) cli_progress_done()

    dts <- ngrams(
      li, n = n, char = char,
      mc = mc, verbose = verbose, progress = progress
    )
    
    if (verbose) cli_progress_step("generate return value")
    proto <- as(as(.Object, "corpus"), "ngrams")
    proto@n = as.integer(n)
    proto@p_attribute = if (is.null(char)) p_attribute else "ngram"
    
    retval@objects <- lapply(
      1L:length(dts),
      function(i){
        proto@stat <- dts[[i]]
        proto@size <- as.integer(size(.Object[[i]]))
        proto@name <- names(.Object)[[i]]
        proto
      }
    )
    if (verbose) cli_progress_done()
  }
  
  names(retval@objects) <- names(.Object)
  
  retval
})