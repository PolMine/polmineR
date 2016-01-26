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
setMethod("noise", "DocumentTermMatrix", function(.Object, minSum=2, minTfIdfMean = 0.005, sparse=0.995, stopwordsLanguage="german", minNchar=2, verbose=TRUE){
  noiseList <- list()
  # min count
  if (!is.null(minTotal)){
    if (verbose == TRUE) message("... minTotal ", appendLF = FALSE)
    tfTotal <- slam::col_sums(.Object)
    noiseList[["minTotal"]] <- names(tfTotal[which(tfTotal < minSum)])
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
  
  if (verbose == TRUE) message("... filtering vocabulary ", appendLF = FALSE)
  if (!is.null(stopwordsLanguage)){
    termsToDrop[["stopwords"]] <- which(colnames(.Object) %in% stopwords(stopwordsLanguage))
  }
  termsToDrop[["specialChars"]] <- colnames(.Object)[which(!c(1:length(terms)) %in% grep("^[a-zA-ZéäöüÄÖÜ-ß|-]+$", terms))]
  termsToDrop[["numbers"]] <- colnames(.Object)[grep("[0-9]", terms)]
  termsToDrop[["stubs"]] <- colnames(.Object)[which(nchar(colnames(.Object)) <= ncharTerm)]

  # attr(.Object, "weighting") <- c("term frequency", "tf")
  termsToDrop
})

setMethod("noise", "TermDocumentMatrix", function(.Object, ...){
  dtm <- as.DocumentTermMatrix(.Object)
  noise(dtm, ...)
})