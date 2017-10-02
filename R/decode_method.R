#' @rdname decode
setGeneric("decode", function(.Object, ...) standardGeneric("decode"))


#' Decode corpus or s-attribute.
#' 
#' Decode either a corpus, or a s-attribute.
#' 
#' @param .Object the corpus to decode (character vector)
#' @param verbose logical
#' @param sAttribute the s-attribute to decode
#' @param ... further parameters
#' @return a \code{data.table}
#' @rdname decode
#' @examples
#' \dontrun{
#' use("polmineR.sampleCorpus")
#' 
#' # Scenario 1: Decode one s-attribute
#' dt <- decode("PLPRBTTXT", sAttribute = "text_date")
#' 
#' # Scenario 2: Decode corpus entirely
#' dt <- decode("PLPRBTTXT")
#' 
#' }
#' @exportMethod decode
#' @seealso \code{\link{encode}}
setMethod("decode", "character", function(.Object, sAttribute = NULL, verbose = TRUE){
  
  stopifnot(.Object %in% CQI$list_corpora()) # check that corpus is available
  
  if (!is.null(sAttribute)){
    
    stopifnot(sAttribute %in% sAttributes(.Object)) # check that s-attribute is available
    
    if (getOption("polmineR.cwb-s-decode")){
      
      cmd <- c(
        "cwb-s-decode",
        "-r", Sys.getenv("CORPUS_REGISTRY"), .Object,
        "-S", sAttribute
      )
      cmd <- paste(cmd, collapse = " ", sep = " ")
      if (verbose) message(cmd)
      raw <- system(cmd, intern = TRUE)
      Encoding(raw) <- getEncoding(.Object)
      raw2 <- as.nativeEnc(x = raw, from = getEncoding(.Object))
      y <- as.data.table(do.call(rbind, strsplit(raw2, "\\t")), stringsAsFactors = FALSE)
      y[[1]] <- as.integer(y[[1]])
      y[[2]] <- as.integer(y[[2]])
      colnames(y) <- c("cpos_left", "cpos_right", sAttribute)
      
    } else {
      stop("cwb-s-decode utility required to be present for decoding a s-attribute")
    }
    
  } else {
    
    maxCpos <- CQI$attribute_size(.Object, "word", type = "p") - 1
    pAttributeList <- lapply(
      pAttributes(.Object),
      function(x){
        if (verbose) message("... decoding pAttribute ", x)
        tokens <- getTokenStream(.Object, pAttribute = x)
        Encoding(tokens) <- getEncoding(.Object)
        as.nativeEnc(tokens, from = getEncoding(.Object))
      }
    )
    names(pAttributeList) <- pAttributes(.Object)
    
    sAttributeList <- lapply(
      sAttributes(.Object),
      function(x){
        if (verbose) message("... decoding sAttribute ", x)
        struc <- CQI$cpos2struc(.Object, x, 0:maxCpos)
        str <- CQI$struc2str(.Object, x, struc)
        Encoding(str) <- getEncoding(.Object)
        as.nativeEnc(str, from = getEncoding(.Object))
      }
    )
    names(sAttributeList) <- sAttributes(.Object)
    
    
    if (verbose) message("... assembling data.table")
    combinedList <- c(
      list(cpos = 0:maxCpos),
      pAttributeList,
      sAttributeList
    )
    y <- as.data.table(combinedList)
    
  }
  y
})