#' @rdname duplicates-method
setGeneric("duplicates", function(.Object, ...) standardGeneric("duplicates"))

#' get duplicates
#' 
#' The method implements a procedure described by Fritz Kliche, Andre Blessing,
#' Urlich Heid and Jonathan Sonntag in the paper "The eIdentity Text
#' ExplorationWorkbench" presented at LREC 2014
#' (see \url{http://www.lrec-conf.org/proceedings/lrec2014/pdf/332_Paper.pdf}).
#' 
#' The method calls the (internal) \code{"getDuplicates"}-method that will make choices as follows:
#' (a) If two similar articles have been published on the same day, the shorter article will
#' be considered the duplicate; (b) if two similar articles were published on different days,
#' the article that appeared later will be considered the duplicate.
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
setMethod("duplicates", "partitionBundle", function(.Object, chars="[a-zA-Z]", pAttribute="word", sAttribute="text_date", sample=100, n=2, threshold=0.9, mc=FALSE, verbose=TRUE){
  if (verbose == TRUE) message("... counting characters")
  if (is.numeric(sample)){
    bundleSample <- sample(.Object, size=sample)
    charCount <- nchars(bundleSample, pAttribute=pAttribute, regexCharsToKeep=chars, toLower=TRUE, decreasing=FALSE, mc=FALSE)
    rm(bundleSample)
  } else {
    charCount <- nchars(.Object, pAttribute, regexCharsToKeep=chars, toLower=TRUE, decreasing=FALSE, mc=FALSE)  
  }
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
  duplicates
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
  if (length(indexDuplicates) > 0){
    # keep only those values in similarity matrix that are above the threshold
    for (x in c("i", "j", "v")) similarityMatrix[[x]] <- similarityMatrix[[x]][indexDuplicates]  
    duplicateList <- lapply(
      c(1:length(similarityMatrix$i)),
      function(i){
        iName <- similarityMatrix$dimnames[[1]][similarityMatrix$i[i]]
        jName <- similarityMatrix$dimnames[[1]] [similarityMatrix$j[i]]
        iDate <- as.POSIXct(dates[[iName]])
        iSize <- .Object@objects[[iName]]@size
        jDate <- as.POSIXct(dates[[jName]])
        jSize <- .Object@objects[[jName]]@size
        value <- similarityMatrix$v[i]
        if(iDate == jDate){
          if (iSize >= jSize){
            return(c(name=iName, date=as.character(iDate), size=iSize, duplicate_name=jName, duplicate_date=as.character(jDate), duplicate_size=jSize, similarity=value))
          } else {
            return(c(name=jName, date=as.character(jDate), size=jSize, duplicate_name=iName, duplicate_date=as.character(iDate), duplicate_size=iSize, similarity=value))
          }
        } else if (iDate < jDate){
          return(c(name=iName, date=as.character(iDate), size=iSize, duplicate_name=jName, duplicate_date=as.character(jDate), duplicate_size=jSize, similarity=value))
        } else if (iDate > jDate){
          return(c(name=jName, date=as.character(jDate), size=jSize, duplicate_name=iName, duplicate_date=as.character(iDate), duplicate_size=iSize, similarity=value))
        }
      })
    duplicateDT <- data.table(do.call(rbind, duplicateList))
    count <- function(x) return(x)
    DT <- duplicateDT[, count(.N), by=.(name, date, size, duplicate_name, duplicate_date, duplicate_size, similarity)][, V1 := NULL]
    DT[, size := as.numeric(size)][, duplicate_size := as.numeric(duplicate_size)][, similarity := as.numeric(similarity)]
    return(DT)
  } else {
    message("... no duplicates found")
    return(NULL)
  }
})

