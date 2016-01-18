setGeneric("similarity", function(.Object, ...) standardGeneric("similarity"))

#' get cosine similarity for a sparse matrix
#' 
#' @param .Object a simple_triplet_matrix
#' @param select a simple_triplet_matrix
#' @param method defaults to "cosine", no other method is implemented
#' @param weighting defaults to "tfidf"
#' @param progress logical
#' @param verbose logical
#' @param mc logical, or if numeric, providing number of cores
setMethod("similarity", "TermDocumentMatrix", function(.Object, select=NULL, method="cosine", weighting="tfidf", progress=TRUE, verbose=FALSE, mc=FALSE){
  # the sparse matrix needs to be inflated, the alternative is far too slow
  if (verbose == TRUE) message("... applying weighting algorithm")
  if (weighting %in% c("tfidf")) .Object <- polmineR::weigh(.Object, method=weighting)
  if (verbose == TRUE) message("... turning sparse matrix into ordinary matrix")
  xNonsparse <- as.matrix(.Object)
  if (verbose == TRUE) message("... calculating similarities")
  nValuesMatrix <- length(select$i)
  startTime <- Sys.time()
  if (is.null(select)){
    if (verbose == TRUE) message("... generating select matrix")
    iIndex <- unlist(lapply(c(1:ncol(.Object)), function(i) rep(i, times= i-1)))
    select <- simple_triplet_matrix(
      i=iIndex,
      j=unlist(lapply(c(2:ncol(.Object)), function(i) c(1:(i-1)))),
      v=rep(NA, times=length(iIndex)),
      ncol=ncol(.Object), nrow=ncol(.Object),
      dimnames=list(colnames(.Object), colnames(.Object))
    )
  }
  .compareDocs1 <- function(i){
    if (mc == FALSE) .progressBar(i, nValuesMatrix)
    a <- xNonsparse[,select$i[i]]
    b <- xNonsparse[,select$j[i]]
    cProd <- crossprod(a) * crossprod(b)
    cProdSqrt <- sqrt(cProd)
    crossprod(a, b)/cProdSqrt
  }
  if (mc == FALSE){
    cosineValues <- lapply(c(1:nValuesMatrix), .compareDocs1)
  } else if (mc == TRUE){
    if (progress == FALSE){
      cosineValues <- mclapply(c(1:nValuesMatrix), .compareDocs1)  
    } else if (progress == TRUE){
      .compareDocs2 <- function(i, verbose, param){
        if (mc == FALSE) .progressBar(i, param$nValuesMatrix)
        a <- param$xNonsparse[,param$select$i[i]]
        b <- param$xNonsparse[,param$select$j[i]]
        cProd <- crossprod(a) * crossprod(b)
        cProdSqrt <- sqrt(cProd)
        crossprod(a, b)/cProdSqrt
      }
      cosineValues <- papply(
        index=c(1:nValuesMatrix), f=.compareDocs2, verbose=TRUE, mc=3,
        param=list(nValuesMatrix=nValuesMatrix, startTime=startTime, select=select, xNonsparse=xNonsparse)
      )
    }
  }
  if (verbose == TRUE) message("... preparing matrix to be returned")
  similarityMatrix <- select
  similarityMatrix$v <- unlist(cosineValues)
  return(similarityMatrix)
})

