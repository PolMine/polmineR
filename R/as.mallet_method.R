#' as.mallet
#' 
#' Save polmineR-object as mallet object
#' 
#' @param .Object partition- or partitionBundle-object
#' @param ... further parameters
#' @param pAttribute the pAttribute to use, typically "word" or "lemma"
#' @param stoplist a list of stopwords to be dropped from matrix, defaults to stopwords("de") from tm package
#' @param mc whether to use multicore
#' @param verbose logical, whether to be verbose
#' @param object the rJava-object
#' @param filename where to store the Java-object
#' @exportMethod as.mallet
#' @rdname as.mallet-method
setGeneric("as.mallet", function(.Object, ...) standardGeneric("as.mallet"))

#' @examples 
#' \dontrun{
#' clu <- partitionBundle("PLPRBTTXT", def=list(text_year="2009"), var=list(text_protocol_no=NULL), pAttribute=NULL)
#' foo <- as.mallet(clu)
#' }
#' @rdname as.mallet-method
setMethod("as.mallet", "partitionBundle", function(.Object, pAttribute="word", stoplist=stopwords("de"), mc=TRUE, verbose=TRUE){
  if (require("mallet", quietly=TRUE)){
    if (verbose == TRUE) message("... mallet-package loaded")
  } else {
    warning("malle package not available")
    stop()
  }
  if (mc == FALSE){
    if (verbose == TRUE) message("... reconstructing token stream (mc=FALSE)")
    tokenStream <- lapply(
      .Object@objects, function(x) getTokenStream(x, pAttribute=pAttribute, collapse="\n")
      )
  } else if (mc == TRUE){
    if (verbose == TRUE) message("... reconstructing token stream (mc=TRUE)")
    tokenStream <- mclapply(
      .Object@objects, function(x) getTokenStream(x, pAttribute=pAttribute, collapse="\n")
      )
  }
  tokenStreamVector <- unlist(tokenStream)
  tmpDir <- tempdir()
  stoplistFile <- file.path(tmpDir, "stoplists.txt")
  cat(paste(stoplist, collapse="\n"), file=stoplistFile)
  # malletFile <- file.path(tmpDir, "partitionBundle.mallet")
  if (verbose == TRUE) message("... make mallet object")
  malletObject <- mallet.import(
    id.array=names(.Object), text.array=tokenStreamVector,
    stoplist.file=stoplistFile, preserve.case=T, token.regexp="\n"
  )
  return(malletObject)
})
