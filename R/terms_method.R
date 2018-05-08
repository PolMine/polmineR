#' @include partition_class.R
NULL

#' Get Terms Occurring in Partition or Corpus.
#' 
#' @param x an atomic \code{character} vector with a corpus id or \code{partition} object
#' @param pAttribute the p-attribute to be analyzed
#' @param regex regular expression(s) to filter results
#' @param robust logical, whether to check for potential failures
#' @exportMethod terms
#' @docType methods
#' @name terms
#' @docType methods
#' @importFrom RcppCWB region_matrix_to_ids
#' @examples
#' use("polmineR")
#' session <- partition("GERMAPARLMINI", date = "2009-10-27")
#' words <- terms(session, "word")
#' terms(session, pAttribute = "word", regex = "^Arbeit.*")
#' terms(session, pAttribute = "word", regex = c("Arbeit.*", ".*arbeit"))
#' 
#' terms("GERMAPARLMINI", pAttribute = "word")
#' terms("GERMAPARLMINI", pAttribute = "word", regex = "^Arbeit.*")
#' @rdname terms
#' @aliases terms,partition-method
setMethod("terms", "partition", function(x, pAttribute, regex = NULL){
  # ensure that input is correct
  stopifnot(is.character(pAttribute))
  if (length(pAttribute) > 1) stop("cannot process more than one p-attribute")
  if (!is.null(regex)) stopifnot(is.character(regex))
  
  # if count has been performed for partition use stat table
  if (identical(pAttribute, x@pAttribute)){
    y <- x@stat[[pAttribute]]
  } else {
    ids <- region_matrix_to_ids(corpus = x@corpus, p_attribute = pAttribute, matrix = x@cpos)
    ids_unique <- unique(ids)
    y <- CQI$id2str(corpus = x@corpus, pAttribute = pAttribute, id = ids_unique)
    Encoding(y) <- x@encoding
  }
  y <- enc2utf8(y)
  
  if (!is.null(regex)) {
    y <- unlist(lapply(regex, function(r) grep(r, y, value = TRUE)))
  }
  y
})

#' @rdname terms
setMethod("terms", "character", function(x, pAttribute, regex = NULL, robust = FALSE){
  stopifnot(length(x) == 1, is.character(pAttribute), length(pAttribute) == 1, is.logical(robust))
  if (!is.null(regex)) stopifnot(is.character(regex))
  
  corpusEncoding <- getEncoding(x)
  totalNoTerms <- CQI$lexicon_size(x, pAttribute)
  ids <- 0L:(totalNoTerms - 1L)
  
  y <- CQI$id2str(x, pAttribute, ids)
  Encoding(y) <- getEncoding(x)
  y <- as.nativeEnc(y, from = corpusEncoding)
  if (robust != FALSE){
    if (robust == TRUE){
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
      for (i in 1:length(villainPos)){
        if (i >= 2) y[villainPos[i]] <- paste(villainName, i, sep = "_")
      }
    }
  }
  if (!is.null(regex)) {
    y <- unlist(lapply(regex, function(r) grep(r, y, value = TRUE)))
  }
  y
})

