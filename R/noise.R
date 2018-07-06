#' @include S4classes.R
NULL


#' @rdname noise
setGeneric("noise", function(.Object, ...) standardGeneric("noise"))

#' detect noise
#' 
#' @param .Object an .Object of class \code{"DocumentTermMatrix"}
#' @param minTotal minimum colsum (for DocumentTermMatrix) to qualify a term as non-noise
#' @param minTfIdfMean minimum mean value for tf-idf to qualify a term as non-noise
#' @param specialChars special characters to drop
#' @param numbers regex, to drop numbers
#' @param verbose logical
#' @param p_attribute relevant if applied to a textstat object
#' @param sparse will be passed into \code{"removeSparseTerms"} from \code{"tm"}-package
#' @param stopwordsLanguage e.g. "german", to get stopwords defined in the tm package
#' @param minNchar min char length ti qualify a term as non-noise
#' @param ... further parameters
#' @return a list 
#' @importFrom slam col_sums
#' @exportMethod noise
#' @rdname noise
setMethod(
  "noise", "DocumentTermMatrix",
  function(
    .Object, minTotal = 2, minTfIdfMean = 0.005, sparse = 0.995,
    stopwordsLanguage = "german", minNchar = 2,
    specialChars = getOption("polmineR.specialChars"),
    numbers = "^[0-9\\.,]+$",
    verbose = TRUE
    ){
  noiseList <- list()
  # min count
  if (!is.null(minTotal)){
    .message("minTotal ", verbose = verbose)
    tfTotal <- slam::col_sums(.Object)
    noiseList[["minTotal"]] <- names(tfTotal[which(tfTotal < minTotal)])
  }
  
  # min tf idf mean
  if (!is.null(minTfIdfMean)){
    .message("minTfIdfMean ", verbose = verbose)
    dtmTfidf <- weigh(.Object, method = "tfidf")
    tfidfMeans <- means(dtmTfidf, dim = 1)
    noiseList[["minTfIdf"]] <- names(tfidfMeans[which(tfidfMeans < minTfIdfMean)])
    noiseList[["minTfIdf"]] <- noiseList[["minTfIdf"]][!is.na(noiseList[["minTfIdf"]])]
  }
  
  # reduce sparsity
  if (!is.null(sparse)){
    .message("sparsity ", verbose = verbose)
    ncolOld <- ncol(.Object)
    dtmTmp <- tm::removeSparseTerms(.Object, sparse=sparse)
    noiseList[["sparse"]] <-  colnames(.Object)[which(!colnames(.Object) %in% colnames(dtmTmp))]
  }
  noiseList <- c(
    noiseList,
    noise(
      colnames(.Object),
      stopwordsLanguage=stopwordsLanguage, minNchar=minNchar,
      specialChars=specialChars, numbers=numbers
    )
  )
  noiseList
})

#' @rdname noise
setMethod("noise", "TermDocumentMatrix", function(.Object,  ...){
  noise(t(.Object), ...)
})

#' @rdname noise
setMethod(
  "noise", "character",
  function(
    .Object, 
    stopwordsLanguage = "german", minNchar = 2,
    specialChars = getOption("polmineR.specialChars"),
    numbers = "^[0-9\\.,]+$",
    verbose = TRUE
  ){
    noiseList <- list()
    if (!is.null(stopwordsLanguage)){
      .message("stopwords", verbose = verbose)
      noiseList[["stopwords"]] <- .Object[which(.Object %in% tm::stopwords(stopwordsLanguage))]
    }
    if (!is.null(specialChars)){
      .message("specialCharsRegex", verbose = verbose)
      noiseList[["specialChars"]] <- .Object[which(!c(1:length(.Object)) %in% grep(specialChars, .Object))]  
    }
    if (!is.null(numbers)){
      .message("numbers", verbose = verbose)
      noiseList[["numbers"]] <- grep(numbers, .Object, value=T)
    }
    if (!is.null(minNchar)){
      noiseList[["minNchar"]] <- .Object[which(nchar(.Object) <= minNchar)]  
    }
    noiseList
  })


#' @rdname noise
setMethod("noise", "textstat", function(.Object, p_attribute, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  noise(.Object@stat[[p_attribute]], ...)
})