#' get terms available in a corpus or partition
#' 
#' @param .Object the object
#' @param pAttribute the pAttribute
#' @param mc logical, whether to use multicore
#' @param verbose logical, whether to be verbose
#' @param robust logical, whether to be robust
#' @param ... further parameters
#' @exportMethod getTerms
#' @rdname getTerms-method
#' @name getTerms
setGeneric("getTerms", function(.Object, ...) standardGeneric("getTerms"))

#' @rdname getTerms-method
setMethod("getTerms", "character", function(.Object, pAttribute, robust=FALSE, verbose=TRUE, mc=FALSE){
  # pAttr <- paste(.Object, ".", pAttribute, sep="")
  corpusEncoding <- getEncoding(.Object)
  totalNoTerms <- CQI$lexicon_size(.Object, pAttribute)
  ids <- c(0:(totalNoTerms-1))
  
  if (mc == FALSE){
    strs <- CQI$id2str(.Object, pAttribute, ids)
  } else if (mc == TRUE){
    .getTerms <- function(ids) CQI$id2str(.Object, pAttribute, ids)
    warning("... not yet implemented")
  }
  Encoding(strs) <- corpusEncoding
  strs <- iconv(strs, from=corpusEncoding, to="UTF-8")
  if (robust != FALSE){
    if (robust == TRUE){
      if (length(strs) != length(unique(strs))){
        if (verbose == TRUE) message("... counting to hunt down villains")
        strCount <- table(strs)
        villainNames <- names(which(strCount > 1))
      }      
    } else if (is.character(robust)) {
      villainNames <- robust
    }
    for (villainName in villainNames){
      if (verbose == TRUE) message("... this is a villain: ", villainName)
      villainPos <- which(villainName == strs)
      for (i in c(1:length(villainPos))){
        if (i >= 2) strs[villainPos[i]] <- paste(villainName, "_", i, sep="")
      }
    }
  }
  strs
})
