#' @rdname characterCount
setGeneric("characterCount", function(.Object, ...) standardGeneric("characterCount"))

#' count characters
#' 
#' @param .Object object
#' @param pAttribute the p-attribute
#' @param regexCharsToKeep if not NULL, a regex
#' @param toLower whether to lower tokens
#' @param mc logical
#' @param ... parameters that will be passed
#' @param decreasing logical, passed into order call 
#' @exportMethod characterCount
#' @rdname characterCount
setMethod("characterCount", "partition", function(.Object, pAttribute, regexCharsToKeep="[a-zA-Z]", toLower=TRUE, decreasing=TRUE){
  charSoup <- getTokenStream(.Object, pAttribute=pAttribute, collapse="")
  if ( toLower == TRUE ) charSoup <- tolower(charSoup)
  charCount <- table(unlist(strsplit(charSoup, "")))
  if(!is.null(regexCharsToKeep)){
    charCount <- charCount[grep(regexCharsToKeep, names(charCount))]
  }
  charCount[order(charCount, decreasing=decreasing)]
})

#' @rdname characterCount
setMethod("characterCount", "partitionBundle", function(.Object, decreasing=TRUE, mc=FALSE, ...){
  if (mc == FALSE){
    partitionCount <- lapply(.Object@objects, function(x) characterCount(x, ...))
  } else {
    partitionCount <- mclapply(
      .Object@objects,
      function(x) characterCount(x, ...),
      mc.cores=ifelse(mc == TRUE, slot(get("session", ".GlobalEnv"), "cores"))
      )
  }
  charCount <- tapply(
    unname(unlist(partitionCount)),
    INDEX=unlist(sapply(partitionCount, function(x) names(x))),
    FUN=sum
  )
  charCount[order(charCount, decreasing=decreasing)]
})