#' @rdname duplicates-method
setGeneric("duplicates", function(.Object, ...) standardGeneric("duplicates"))

#' get duplicates
#' 
#' The method implements a procedure described by Fritz Kliche, Andre Blessing,
#' Urlich Heid and Jonathan Sonntag in the paper "The eIdentity Text
#' ExplorationWorkbench" presented at LREC 2014
#' (see \url{http://www.lrec-conf.org/proceedings/lrec2014/pdf/332_Paper.pdf}).
#' 
#' @param .Object a \code{"partitionBundle"} object
#' @param chars a regex providing the characters to keep
#' @param sAttribute the s-attribute providing the date
#' @param sample will be passed as param size into sample-method to achieve a subset of partitionBundle and make char count faster
#' @param n number of days before and after a document was published
#' @param threshold numeric (0 < x < 1), the minimum similarity to qualify two documents as duplicates
#' @param mc whether to use multicore
#' @param verbose logical, whether to be verbose
#' @param ... further parameters (verbose, progress, mc) to be passed into functions
#' @examples 
#' \dontrun{
#' foo <- partitionBundle(
#'   "KEYWORDS",
#'   def=list(text_newspaper="guardian"),
#'   var=list(text_id=sAttributes("KEYWORDS", "text_id")[1:500]),
#'   pAttribute=NULL
#'  )
#' doubled <- duplicates(foo)
#' }
#' @exportMethod duplicates
#' @rdname duplicates-method
setMethod("duplicates", "partitionBundle", function(.Object, chars="[a-zA-Z]", sAttribute="text_date", sample=100, n=2, threshold=0.9, mc=FALSE, verbose=TRUE){
  if (verbose == TRUE) message("... counting characters")
  bundleSample <- sample(.Object, size=sample)
  charCount <- characterCount(.Object, pAttribute, regexCharsToKeep=chars, toLower=TRUE, decreasing=FALSE, mc=FALSE)
  if (verbose == TRUE) message("... preparing ngram matrix")
  ngramBundle <- ngrams(.Object, n=4, char=names(charCount[1:10]))
  ngramDocumentMatrix <- as.TermDocumentMatrix(ngramBundle, col="count")
  ngramDocumentMatrixWeighed <- polmineR::weigh(ngramDocumentMatrix, method="tfidf")
  if (verbose == TRUE) message("... identifying comparables")
  whatToCompareMatrix <- comparables(.Object, date=sAttribute, n=n)
  if (verbose == TRUE) message("... calculating cosine similarity")
  similarityMatrix <- similarity(ngramDocumentMatrixWeighed, select=whatToCompareMatrix)
  if (verbose == TRUE) message("... preparing data.table")
  duplicates <- getDuplicates(.Object, similarityMatrix=similarityMatrix, threshold=threshold, date=sAttribute, progress=TRUE)
})


setGeneric("getDuplicates", function(.Object, ...) standardGeneric("getDuplicates"))

setMethod("getDuplicates", "partitionBundle", function(.Object, similarityMatrix, threshold=0.9, date, mc=FALSE, progress=TRUE, verbose=TRUE){
  if (verbose == TRUE) message("... applying threshold")
  if (mc == FALSE) mc <- 1
  dates <- unlist(lapply(
    setNames(.Object@objects, names(.Object)),
    function(x) sAttributes(x, date))
    )
  indexDuplicates <- which(similarityMatrix$v >= threshold)
  if (length(indexDuplicates > 0)){
    for (x in c("i", "j", "v")) similarityMatrix[[x]] <- similarityMatrix[[x]][indexDuplicates]  
    duplicateList <- lapply(
      c(1:length(similarityMatrix$i)),
      function(i){
        iName <- similarityMatrix$dimnames[[1]][similarityMatrix$i[i]]
        jName <- similarityMatrix$dimnames[[1]] [similarityMatrix$j[i]]
        iDate <- as.POSIXct(dates[[iName]])
        iLength <- .Object@objects[[iName]]@size
        jDate <- as.POSIXct(dates[[jName]])
        jLength <- .Object@objects[[jName]]@size
        value <- similarityMatrix$v[i]
        if(iDate == jDate){
          if (iLength >= jLength){
            return(c(name=iName, duplicateName=jName, value=value))
          } else {
            return(c(name=jName, duplicateName=iName, value=value))
          }
        } else if (iDate < jDate){
          return(c(name=iName, duplicateName=jName, value=value))
        } else if (iDate > jDate){
          return(c(name=jName, duplicateName=iName, value=value))
        }
      })
    duplicateDT <- data.table(do.call(rbind, duplicateList))
#     duplicateUnique <- unique(sapply(duplicateList, function(x) paste(x, collapse="\t")))
#     duplicateFinal <- as.data.frame(
#       do.call(rbind, strsplit(duplicateUnique, "\t")),
#       stringsAsFactor=FALSE
#     )
#     duplicateFinal[,1] <- as.character(as.vector(duplicateFinal[,1]))
#     duplicateFinal[,2] <- as.character(as.vector(duplicateFinal[,2]))
#     duplicateFinal[,3] <- as.numeric(as.vector(duplicateFinal[,3]))
#     colnames(duplicateFinal) <- c("name", "duplicateName", "value")
#     return(duplicateFinal)
    return(duplicateDT)
  } else {
    message("... no duplicates found")
    return(NULL)
  }
})

