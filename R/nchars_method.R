#' Count the number of characters
#' 
#' @param .Object object
#' @param pAttribute the p-attribute
#' @param regexCharsToKeep if not NULL, a regex
#' @param toLower whether to lower tokens
#' @param mc logical
#' @param ... parameters that will be passed
#' @param decreasing logical, passed into order call 
#' @exportMethod nchars
#' @rdname nchars
setGeneric("nchars", function(x, ...) standardGeneric("nchars"))


#' count characters
#' 
#' @param .Object object
#' @param pAttribute the p-attribute
#' @param regexCharsToKeep if NULL, counts for all charactrs will be returned, else a regex indicating which characters to include in the counting
#' @param toLower whether to lower tokens
#' @param mc logical
#' @param ... parameters that will be passed
#' @param decreasing logical, passed into order call 
#' @exportMethod nchars
#' @rdname nchars
setMethod("nchars", "partition", function(x, pAttribute="word", regexCharsToKeep="[a-zA-Z]", toLower=TRUE, decreasing=TRUE){
  .Object <- x
  charSoup <- getTokenStream(.Object, pAttribute=pAttribute, collapse="")
  if ( toLower == TRUE ) charSoup <- tolower(charSoup)
  charCount <- table(unlist(strsplit(charSoup, "")))
  if(!is.null(regexCharsToKeep)){
    charCount <- charCount[grep(regexCharsToKeep, names(charCount))]
  }
  charCount[order(charCount, decreasing=decreasing)]
})

#' @rdname nchars
setMethod("nchars", "partitionBundle", function(x, decreasing=TRUE, mc=FALSE, ...){
  .Object <- x
  if (mc == FALSE){
    partitionCount <- lapply(.Object@objects, function(x) nchars(x, ...))
  } else {
    partitionCount <- mclapply(
      .Object@objects,
      function(x) nchars(x, ...),
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