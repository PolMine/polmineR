#' @noRd
setGeneric("comparables", function(.Object, ...) standardGeneric("comparables"))

#' @noRd
setMethod("comparables", "partitionBundle", function(.Object, dates=NULL, n=1, reduce=TRUE, verbose=FALSE, progress=TRUE, mc=FALSE){
  if (!is.null(dates)){
    if (requireNamespace("chron", quietly=TRUE)){
      message("... chron-package required and loaded")
    } else {
      stop("the 'chron'-package needs to be installed but is not available")
    }
    if (verbose == TRUE) message("... getting files to be compared")
    dates <- unlist(lapply(setNames(.Object@objects, names(.Object)), function(x) sAttributes(x, dates)))
    objectSplittedByDate <- split(c(1:length(.Object)), f=dates)
    .getWhatToCompare <- function(i){
      dateOfDoc <- as.POSIXct(unname(dates[i]))
      dateRange <- chron::seq.dates(
        from=strftime(dateOfDoc - 1 - (n-1) * 86400, format="%m/%d/%Y"),
        to=strftime(dateOfDoc + 1 + (n-1) * 86400, format="%m/%d/%Y"),
        by="days", format="%Y-%m-%d"
      )
      datesToGet <- sapply(c(1:length(dateRange)), function(i) {
        as.character(strftime(dateRange[i], format="%Y-%m-%d"))
        })
      unlist(lapply(datesToGet, function(x) objectSplittedByDate[[x]]))
    }
    if (mc == FALSE){
      docsToCompare <- lapply(c(1:length(.Object)), .getWhatToCompare)
    } else {
      docsToCompare <- mclapply(c(1:length(.Object)), .getWhatToCompare)
    }
    docsToCompareMatrix <- simple_triplet_matrix(
      i=unlist(docsToCompare),
      j=unlist(lapply(c(1:length(docsToCompare)), function(i) rep(i, times=length(docsToCompare[[i]])))),
      v=rep(NA, times=length(unlist(docsToCompare))),
      ncol=length(.Object),
      nrow=length(.Object),
      dimnames=list(rows=names(.Object), columns=names(.Object))
    )
    if (reduce == TRUE){
      keepOrDrop <- sapply(
        c(1:length(docsToCompareMatrix$i)),
        function(i) ifelse(docsToCompareMatrix$i[i] < docsToCompareMatrix$j[i], TRUE, FALSE)
      )
      for (x in c("i", "j", "v")) docsToCompareMatrix[[x]] <- docsToCompareMatrix[[x]][keepOrDrop]
    }
    return(docsToCompareMatrix)
  } else {
    stop("so far, getting comparables is only implemented based on dates")
  }
})
