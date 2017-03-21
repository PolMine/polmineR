#' @rdname decode
setGeneric("decode", function(.Object, ...) standardGeneric("decode"))


#' Decode corpus.
#' 
#' An encoded CWB corpus is turned into a tibble.
#' 
#' @param .Object the corpus to decode (character vector)
#' @param verbose logical
#' @param sAttribute the s-attribute to decode
#' @param ... further parameters
#' @rdname decode
#' @examples
#' \dontrun{
#' PLPRBTTXT <- Corpus$new("PLPRBTTXT")
#' }
#' @exportMethod decode
setMethod("decode", "Corpus", function(.Object, verbose = TRUE){
  if (require(package = "tibble", quietly = TRUE)){
    maxCpos <- CQI$attribute_size(.Object$corpus, "word", type = "p") - 1
    pAttributeList <- lapply(
      pAttributes(.Object$corpus),
      function(x){
        if (verbose) message("... decoding pAttribute ", x)
        getTokenStream(.Object$corpus, pAttribute = x)
      }
    )
    names(pAttributeList) <- pAttributes(.Object$corpus)
    
    sAttributeList <- lapply(
      sAttributes(.Object$corpus),
      function(x){
        if (verbose) message("... decoding sAttribute ", x)
        struc <- CQI$cpos2struc(.Object$corpus, x, 0:maxCpos)
        str <- CQI$struc2str(.Object$corpus, x, struc)
        as.factor(str)
      }
    )
    names(sAttributeList) <- sAttributes(.Object$corpus)
    
    
    if (verbose) message("... assembling tibble")
    combinedList <- c(
      list(cpos = 0:maxCpos),
      pAttributeList,
      sAttributeList
    )
    
    return(tibble::as_tibble(combinedList))
  } else {
    warning("package 'tibble' required but not available")
  }
})

#' @rdname decode
setMethod("decode", "character", function(.Object, sAttribute, verbose = TRUE){
  cmd <- c(
    "cwb-s-decode",
    "-r", Sys.getenv("CORPUS_REGISTRY"), .Object,
    "-S", sAttribute
  )
  if (verbose) message(cmd)
  raw <- system(paste(cmd, collapse = " ", sep = " "), intern = TRUE)
  y <- as.data.table(do.call(rbind, strsplit(raw, "\\t")), stringsAsFactors = FALSE)
  y[[1]] <- as.integer(y[[1]])
  y[[2]] <- as.integer(y[[2]])
  colnames(y) <- c("cpos_left", "cpos_right", sAttribute)
  y
})