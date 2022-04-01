#' @include partition.R S4classes.R
NULL

#' Get terms in `partition` or `corpus`.
#' 
#' @param x A `corpus`, `partition` or `subcorpus` object, or a length-one
#'   `character` with a corpus id.
#' @param p_attribute The p-attribute to for which to retrieve results
#'   (length-one `character` vector).
#' @param regex Regular expression(s) to filter results (`character` vector).
#' @param robust A `logical` value, whether to check for potential failures.
#' @exportMethod terms
#' @docType methods
#' @name terms
#' @docType methods
#' @importFrom RcppCWB region_matrix_to_ids
#' @examples
#' use("polmineR")
#' r <- partition("REUTERS", id = "144")
#' words <- terms(r, "word")
#' terms(r, p_attribute = "word", regex = ".*il.*")
#' @rdname terms
#' @aliases terms,slice-method terms,partition-method
setMethod("terms", "slice", function(x, p_attribute, regex = NULL){
  
  # ensure that input is correct
  stopifnot(is.character(p_attribute))
  if (length(p_attribute) > 1) stop("cannot process more than one p-attribute")
  if (!is.null(regex)) stopifnot(is.character(regex))
  
  # if count has been performed for partition use stat table
  if (identical(p_attribute, x@p_attribute)){
    y <- x@stat[[p_attribute]]
  } else {
    ids <- region_matrix_to_ids(
      corpus = x@corpus, registry = x@registry_dir,
      p_attribute = p_attribute,
      matrix = x@cpos
    )
    ids_unique <- unique(ids)
    y <- cl_id2str(
      corpus = x@corpus,
      p_attribute = p_attribute,
      id = ids_unique,
      registry = x@registry_dir
    )
    Encoding(y) <- x@encoding
  }
  y <- enc2utf8(y)
  
  if (!is.null(regex)) {
    y <- unlist(lapply(regex, function(r) grep(r, y, value = TRUE)))
  }
  y
})

#' @rdname terms
setMethod("terms", "partition", function(x, p_attribute, regex = NULL) callNextMethod() )

#' @rdname terms
setMethod("terms", "subcorpus", function(x, p_attribute, regex = NULL) callNextMethod() )


#' @rdname terms
setMethod("terms", "corpus", function(x, p_attribute, regex = NULL, robust = FALSE){
  
  stopifnot(
    length(x) == 1L,
    is.character(p_attribute),
    length(p_attribute) == 1L,
    is.logical(robust)
  )
  if (!is.null(regex)) stopifnot(is.character(regex))
  
  terms_total <- cl_lexicon_size(
    corpus = x@corpus,
    p_attribute = p_attribute,
    registry = x@registry_dir
  )
  ids <- 0L:(terms_total - 1L)
  str <- cl_id2str(
    corpus = x@corpus,
    p_attribute = p_attribute,
    id = ids,
    registry = x@registry_dir
  )
  Encoding(str) <- x@encoding
  y <- as.nativeEnc(str, from = x@encoding)
  
  if (robust != FALSE){
    if (robust){
      if (length(y) != length(unique(y))){
        warning("there may be terms causing issues")
        strCount <- table(y)
        villains <- names(which(strCount > 1))
      }      
    } else if (is.character(robust)) {
      villains <- robust
    }
    for (villain in villains){
      warning("this is a villain: ", villain)
      villain_pos <- which(villain == y)
      for (i in 1L:length(villain_pos)){
        if (i >= 2L) y[villain_pos[i]] <- paste(villain, i, sep = "_")
      }
    }
  }
  if (!is.null(regex)) {
    y <- unlist(lapply(regex, function(r) grep(r, y, value = TRUE)))
  }
  y
})


#' @rdname terms
setMethod("terms", "character", function(x, p_attribute, regex = NULL, robust = FALSE){
  terms(
    x = corpus(x),
    p_attribute = p_attribute,
    regex = regex,
    robust = robust
  )
})

