#' @include partition.R S4classes.R
NULL

#' Get terms in \code{partition} or corpus.
#' 
#' @param x an atomic \code{character} vector with a corpus id or \code{partition} object
#' @param p_attribute the p-attribute to be analyzed
#' @param regex regular expression(s) to filter results
#' @param robust logical, whether to check for potential failures
#' @param ... for backward compatibility
#' @exportMethod terms
#' @docType methods
#' @name terms
#' @docType methods
#' @importFrom RcppCWB region_matrix_to_ids
#' @examples
#' use("polmineR")
#' session <- partition("GERMAPARLMINI", date = "2009-10-27")
#' words <- terms(session, "word")
#' terms(session, p_attribute = "word", regex = "^Arbeit.*")
#' terms(session, p_attribute = "word", regex = c("Arbeit.*", ".*arbeit"))
#' 
#' terms("GERMAPARLMINI", p_attribute = "word")
#' terms("GERMAPARLMINI", p_attribute = "word", regex = "^Arbeit.*")
#' @rdname terms
#' @aliases terms,slice-method terms,partition-method
setMethod("terms", "slice", function(x, p_attribute, regex = NULL, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  # ensure that input is correct
  stopifnot(is.character(p_attribute))
  if (length(p_attribute) > 1) stop("cannot process more than one p-attribute")
  if (!is.null(regex)) stopifnot(is.character(regex))
  
  # if count has been performed for partition use stat table
  if (identical(p_attribute, x@p_attribute)){
    y <- x@stat[[p_attribute]]
  } else {
    ids <- region_matrix_to_ids(corpus = x@corpus, p_attribute = p_attribute, matrix = x@cpos)
    ids_unique <- unique(ids)
    y <- cl_id2str(corpus = x@corpus, p_attribute = p_attribute, id = ids_unique, registry = registry())
    Encoding(y) <- x@encoding
  }
  y <- enc2utf8(y)
  
  if (!is.null(regex)) {
    y <- unlist(lapply(regex, function(r) grep(r, y, value = TRUE)))
  }
  y
})

#' @rdname terms
setMethod("terms", "partition", function(x, p_attribute, regex = NULL, ...) callNextMethod() )

#' @rdname terms
setMethod("terms", "subcorpus", function(x, p_attribute, regex = NULL, ...) callNextMethod() )


#' @rdname terms
setMethod("terms", "character", function(x, p_attribute, regex = NULL, robust = FALSE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  stopifnot(
    length(x) == 1,
    is.character(p_attribute),
    length(p_attribute) == 1,
    is.logical(robust)
    )
  if (!is.null(regex)) stopifnot(is.character(regex))
  
  terms_total <- cl_lexicon_size(corpus = x, p_attribute = p_attribute, registry = registry())
  ids <- 0L:(terms_total - 1L)
  str <- cl_id2str(corpus = x, p_attribute = p_attribute, id = ids, registry = registry())
  corpus_enc <- registry_get_encoding(x)
  Encoding(str) <- corpus_enc
  y <- as.nativeEnc(str, from = corpus_enc)
  
  if (robust != FALSE){
    if (robust){
      if (length(y) != length(unique(y))){
        warning("there may be terms causing issues")
        strCount <- table(y)
        villainNames <- names(which(strCount > 1))
      }      
    } else if (is.character(robust)) {
      villainNames <- robust
    }
    for (villainName in villainNames){
      warning("this is a villain: ", villainName)
      villainPos <- which(villainName == y)
      for (i in 1L:length(villainPos)){
        if (i >= 2L) y[villainPos[i]] <- paste(villainName, i, sep = "_")
      }
    }
  }
  if (!is.null(regex)) {
    y <- unlist(lapply(regex, function(r) grep(r, y, value = TRUE)))
  }
  y
})

