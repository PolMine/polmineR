#' @include count.R S4classes.R
NULL


#' Get N-Grams
#' 
#' Count n-grams, either of words, or of characters.
#' 
#' @param .Object object of class \code{partition}
#' @param n number of tokens/characters
#' @param p_attribute the p-attribute to use (can be > 1)
#' @param char if NULL, tokens will be counted, else characters, keeping only those provided by a character vector
#' @param mc logical, whether to use multicore, passed into call to \code{blapply} (see respective documentation)
#' @param progress logical
#' @param ... further parameters
#' @exportMethod ngrams
#' @rdname ngrams
#' @name ngrams
#' @examples 
#' use("polmineR")
#' P <- partition("GERMAPARLMINI", date = "2009-10-27")
#' ngramObject <- ngrams(P, n = 2, p_attribute = "word", char = NULL)
#' 
#' # a more complex scenario: get most frequent ADJA/NN-combinations
#' ngramObject <- ngrams(P, n = 2, p_attribute = c("word", "pos"), char = NULL)
#' ngramObject2 <- subset(
#'  ngramObject,
#'  ngramObject[["1_pos"]] == "ADJA"  & ngramObject[["2_pos"]] == "NN"
#'  )
#' ngramObject2@@stat[, "1_pos" := NULL][, "2_pos" := NULL]
#' ngramObject3 <- sort(ngramObject2, by = "count")
#' head(ngramObject3)
setGeneric("ngrams", function(.Object, ...) standardGeneric("ngrams"))

#' @rdname ngrams
setMethod("ngrams", "partition", function(.Object, ...) callNextMethod(.Object, ...))

#' @rdname ngrams
setMethod("ngrams", "character", function(.Object, ...) callNextMethod(.Object, ...))


#' @rdname ngrams
setMethod("ngrams", "CorpusOrSubcorpus", function(.Object, n = 2, p_attribute = "word", char = NULL, progress = FALSE, ...){
  
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
        str <- as.nativeEnc(CQI$id2str(corpus(.Object), p_attrs_cols[i], TF[[i]]), from = encoding(.Object))
        TF[, eval(paste(token_no[i], p_attrs_cols[i], sep = "_")) := str , with = TRUE] 
      })
    
    # remove columns with ids
    lapply(
      grep("id_", colnames(TF), value = TRUE),
      function(x) TF[, eval(x) := NULL, with = TRUE]
    )
    setcolorder(TF, neworder = c(colnames(TF)[!colnames(TF) %in% "count"], "count"))
  } else {
    char_soup_base <- get_token_stream(.Object, p_attribute = p_attribute[1], collapse = "")
    char_soup <- unlist(strsplit(char_soup_base, ""))
    if (char[1] != ""){
      char_soup <- unname(unlist(sapply(char_soup, function(x) ifelse(x %in% char, x, NA))))
      if (any(is.na(char_soup))) char_soup[-which(is.na(char_soup))]
    }
    char_soup <- paste(char_soup[which(!is.na(char_soup))], sep = "", collapse = "")
    char_soup_total <- nchar(char_soup)
    ngrams <- sapply(
      1L:(char_soup_total - n + 1L),
      function(x) {
        if (progress) .progressBar(x, char_soup_total)
        substr(char_soup, x, x + n - 1L)
        })
    tabled_ngrams <- table(ngrams)
    TF <- data.table(
      ngram = names(tabled_ngrams),
      count = unname(as.vector(tabled_ngrams))
      )
  }
  new(
    "ngrams",
    n = as.integer(n), corpus = corpus(.Object), encoding = encoding(.Object),
    size = as.integer(size(.Object)), stat = TF, name = name(.Object),
    p_attribute = if (is.null(char)) p_attribute else "ngram"
    )
})

#' @rdname ngrams
setMethod("ngrams", "partition_bundle", function(.Object, n = 2, char = NULL, p_attribute = "word", mc = FALSE, progress = FALSE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  retval <- new("bundle")
  retval@objects <- blapply(
    .Object@objects, f = ngrams,
    n = n, p_attribute = p_attribute, char = char, mc = mc, progress = progress
    )
  retval@p_attribute <- unique(unlist(lapply(retval@objects, function(x) x@p_attribute)))
  names(retval@objects) <- names(.Object)
  retval
})