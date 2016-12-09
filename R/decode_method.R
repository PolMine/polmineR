setGeneric("decode", function(.Object, ...) standardGeneric("decode"))


#' Decode corpus.
#' 
#' An encoded CWB corpus is turned into a tibble.
#' 
#' @examples 
#' PLPRBTTXT <- Corpus$new("PLPRBTTXT")
setMethod("decode", "Corpus", function(.Object, verbose = TRUE){
  maxCpos <- CQI$attribute_size(.Object$corpus, "word") - 1
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
  
  as_tibble(combinedList)
})