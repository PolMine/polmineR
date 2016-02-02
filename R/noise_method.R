setGeneric("noise", function(.Object, ...) standardGeneric("noise"))

#' detect noise
#' 
#' @param .Object an .Object of class \code{"DocumentTermMatrix"}
#' @param minSum minimum colsum (for DocumentTermMatrix) to qualify a term as non-noise
#' @param minTfIdfMean minimum mean value for tf-idf to qualify a term as non-noise
#' @param sparse will be passed into \code{"removeSparseTerms"} from \code{"tm"}-package
#' @param stopwordsLanguage e.g. "german", to get stopwords defined in the tm package
#' @param minNchar min char length ti qualify a term as non-noise
#' @return a list 
#' @importFrom slam col_sums
#' @exportMethod noise
#' @rdname TermDocumentMatrix
setMethod(
  "noise", "DocumentTermMatrix",
  function(
    .Object, minTotal=2, minTfIdfMean = 0.005, sparse=0.995,
    stopwordsLanguage="german", minNchar=2, specialChars="^[a-zA-ZéäöüÄÖÜ-ß|-]+$",
    numbers="^[0-9\\.,]+$",
    verbose=TRUE
    ){
  noiseList <- list()
  # min count
  if (!is.null(minTotal)){
    if (verbose == TRUE) message("... minTotal ")
    tfTotal <- slam::col_sums(.Object)
    noiseList[["minTotal"]] <- names(tfTotal[which(tfTotal < minTotal)])
  }
  
  # min tf idf mean
  if (!is.null(minTfIdfMean)){
    if (verbose == TRUE) message("... minTfIdfMean ")
    dtmTfidf <- weigh(.Object, method="tfidf")
    tfidfMeans <- means(dtmTfidf, dim=1)
    noiseList[["minTfIdf"]] <- names(tfidfMeans[which(tfidfMeans < minTfIdfMean)])
    noiseList[["minTfIdf"]] <- noiseList[["minTfIdf"]][!is.na(noiseList[["minTfIdf"]])]
  }
  
  # reduce sparsity
  if (!is.null(sparse)){
    if (verbose == TRUE) message("... sparsity ")
    ncolOld <- ncol(.Object)
    dtmTmp <- tm::removeSparseTerms(.Object, sparse=sparse)
    noiseList[["sparse"]] <-  colnames(.Object)[which(!colnames(.Object) %in% colnames(dtmTmp))]
  }
  
  if (!is.null(stopwordsLanguage)){
    if (verbose == TRUE) message("... stopwords")
    noiseList[["stopwords"]] <- colnames(.Object)[which(colnames(.Object) %in% tm::stopwords(stopwordsLanguage))]
  }
  if (!is.null(specialChars)){
    if (verbose == TRUE) message("... specialCharsRegex")
    noiseList[["specialChars"]] <- colnames(.Object)[which(!c(1:length(colnames(.Object))) %in% grep(specialChars, colnames(.Object)))]  
  }
  if (!is.null(numbers)){
    if (verbose == TRUE) message("... numbers")
    noiseList[["numbers"]] <- grep(numbers, colnames(.Object), value=T)
  }
  if (!is.null(minNchar)){
    noiseList[["minNchar"]] <- colnames(.Object)[which(nchar(colnames(.Object)) <= minNchar)]  
  }
  # attr(.Object, "weighting") <- c("term frequency", "tf")
  noiseList
})

setMethod("noise", "TermDocumentMatrix", function(.Object,  ...){
  noise(t(.Object), ...)
})