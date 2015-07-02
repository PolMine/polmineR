#' as.mallet
#' 
#' Save polmineR-object as mallet object
#' @param .Object partition- or partitionCluster-object
#' @param ... further parameters
#' @exportMethod as.mallet
setGeneric("as.mallet", function(.Object, ...) standardGeneric("as.mallet"))

#' @examples 
#' \dontrun{
#' clu <- partitionCluster("ARENEN", def=list(text_newspaper="Süddeutsche Zeitung"), var=list(text_date=NULL), tf=NULL)
#' foo <- as.mallet(clu)
#' }
setMethod("as.mallet", "partitionCluster", function(.Object, pAttribute="word", stoplist=stopwords("de"), mc=TRUE, verbose=TRUE){
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
  # malletFile <- file.path(tmpDir, "partitionCluster.mallet")
  if (verbose == TRUE) message("... make mallet object")
  malletObject <- mallet.import(
    id.array=labels(.Object), text.array=tokenStreamVector,
    stoplist.file=stoplistFile, preserve.case=T, token.regexp="\n"
  )
  return(malletObject)
})
