#' @include bundle_class.R partitionBundle_class.R
NULL

.rmBlank <- function(mat, verbose=TRUE){
  if (verbose==TRUE) message("... removing empty rows")
  matTmp <- as.sparseMatrix(mat)
  matTmp <- matTmp[which(rowSums(matTmp) > 0),]
  mat <- as.simple_triplet_matrix(matTmp)
  class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  mat
}


#' as.TermDocumentMatrix / as.DocumentTermMatrix
#' 
#' Method for type conversion, to generate the classes
#' \code{"TermDocumentMatrix"} or \code{"DocumentTermMatrix"} contained in the
#' \code{"tm"} package. The classes inherit from the
#' \code{"simple_triplet_matrix"}-class defined in the \code{"slam"}-package. A
#' \code{"DocumentTermMatrix"} is required as input by the \code{"topicmodels"}
#' package, for instance.
#'
#' The type conversion-method can be applied on object of the class
#' \code{"bundle"}, or classes inheriting from the \code{"bundle"} class. If
#' counts or some other measure is present in the \code{"stat"} slots of the
#' objects in the bundle, then the values in the column indicated by
#' \code{"col"} will be turned into the values of the sparse matrix that is
#' generated. A special case is the generation of the sparse matrix based on a
#' \code{"partitionBundle"} that does not yet include counts. In this case, a 
#' \code{"pAttribute"} needs to be provided, then counting will be performed,
#' too.
#' 
#' @param x some object
#' @param pAttribute the p-attribute
#' @param sAttribute the s-attribute
#' @param col the column to use of assembling the matrix
#' @param from bla
#' @param to bla
#' @param strucs bla
#' @param rmBlank bla
#' @param verbose bla
#' @param robust bla
#' @param mc logical
#' @param progress logical, whether to show progress bar
#' @param ... to make the check happy
#' @return a TermDocumentMatrix
#' @author Andreas Blaette
#' @exportMethod as.TermDocumentMatrix
#' @exportMethod as.DocumentTermMatrix
#' @docType methods
#' @rdname as.DocumentTermMatrix
#' @name as.TermDocumentMatrix
#' @aliases as.DocumentTermMatrix
#' @author me
setGeneric("as.TermDocumentMatrix", function(x, ...){UseMethod("as.TermDocumentMatrix")})
setGeneric("as.DocumentTermMatrix", function(x, ...){UseMethod("as.DocumentTermMatrix")})

#' @examples
#' if (require(polmineR.sampleCorpus) && require(rcqp)){
#'    use("polmineR.sampleCorpus")
#'    
#'    # do-it-yourself 
#'    p <- partition("PLPRBTTXT", text_date=".*", regex=TRUE)
#'    pB <- partitionBundle(p, sAttribute="text_date")
#'    pB <- enrich(pB, pAttribute="word")
#'    tdm <- as.TermDocumentMatrix(pB, col="count")
#'    
#'    # leave the counting to the as.TermDocumentMatrix-method
#'    pB2 <- partitionBundle(p, sAttribute="text_date")
#'    tdm <- as.TermDocumentMatrix(pB2, pAttribute="word")
#'    
#'    # diretissima
#'    pB3 <- as.TermDocumentMatrix("PLPRBTTXT", pAttribute="word", sAttribute="text_date")
#' }
#' @rdname as.DocumentTermMatrix
setMethod(
  "as.TermDocumentMatrix", "character",
  function (
    x, pAttribute, sAttribute, from=NULL, to=NULL, strucs=NULL,
    rmBlank=TRUE, verbose=TRUE, robust=FALSE, mc=FALSE, progress=TRUE
  ) {
    if (!is.null(strucs)){
      if (is.character(strucs)){
        sAttributeStrings <- sAttributes(x, sAttribute)
        strucs <- which(sAttributeStrings %in% strucs)
        sAttributeStrings <- sAttributeStrings[strucs]
      }
    } else {
      if (!is.null(from) || !is.null(to)) {
        sAttributeStrings <- sAttributes(x, sAttribute)
        fromStruc <- grep(from, sAttributeStrings)[1]
        toStruc <- grep(to, sAttributeStrings)[1]
        strucs <- c(fromStruc:toStruc)
        sAttributeStrings <- sAttributeStrings[strucs]
      } else {
        toStruc <- CQI$attribute_size(corpus = x, attribute = sAttribute) - 1
        fromStruc <- 0
        strucs <- c(0:toStruc) 
      }
    }
    .freqMatrix <- function(i, strucs, corpus, sAttribute, pAttribute, ...){
      struc <- strucs[i]
      cpos <- CQI$struc2cpos(corpus = x, sAttribute, struc)
      ids <- CQI$cpos2id(x, pAttribute, c(cpos[1]:cpos[2]))
      freqVector <- tabulate(ids + 1)
      noZeroCount <- which(freqVector != 0)
      freqMatrix <- matrix(
        c(noZeroCount, freqVector[noZeroCount]),
        ncol=2, byrow=FALSE
      )
      cbind(rep(i, times=nrow(freqMatrix)), freqMatrix)
    }
    if (verbose == TRUE) message("... getting counts")
    freqMatrixList <- blapply(
      as.list(c(1:length(strucs))), f=.freqMatrix,
      strucs=strucs, corpus=corpus, sAttribute=sAttribute, pAttribute=pAttribute,
      mc=mc, progress=progress
      )
    if (verbose == TRUE) message("... combining results")
    freqMatrixAgg <- do.call(rbind, freqMatrixList)
    lexiconSize <- CQI$lexicon_size(x, pAttribute)
    if (verbose == TRUE) message("... id2str for pAttribute")
    pAttributeStrings <- getTerms(x, pAttribute=pAttribute, robust=robust)
    if (!exists("sAttributeStrings")){
      if (verbose == TRUE) message("... id2str for sAttribute")
      sAttributeStrings <- CQI$struc2str(x, sAttribute, c(fromStruc:toStruc))  
    }
    mat <- simple_triplet_matrix(
      i=freqMatrixAgg[,2], j=freqMatrixAgg[,1], v=freqMatrixAgg[,3],
      ncol=length(strucs),
      nrow=lexiconSize,
      dimnames=list(Terms=pAttributeStrings, Docs=sAttributeStrings)
    )
    class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
    if (rmBlank == TRUE) mat <- .rmBlank(mat, verbose=verbose)
    mat
  })


#' @rdname as.DocumentTermMatrix
#' @importFrom slam simple_triplet_matrix
setMethod("as.TermDocumentMatrix", "bundle", function(x, col, pAttribute = NULL, verbose = TRUE){
  if (is.null(pAttribute)){
    pAttribute <- x@objects[[1]]@pAttribute
    if (verbose == TRUE) message("... using the pAttribute-slot of the first object in the bundle as pAttribute: ", pAttribute)
  }
  if (verbose == TRUE) message("... generating (temporary) key column")
  if (length(pAttribute) > 1){
    dummy <- lapply(
      c(1:length(x@objects)),
      function(i){
        keysRaw <- x@objects[[i]]@stat[, c(pAttribute), with=FALSE]
        keys <- apply(keys, 1, function(row) paste(row, collapse="//"))
        x@objects[[i]]@stat[, key := keys]
      })
    rm(dummy)
  } else {
    dummy <- lapply(c(1:length(x@objects)), function(i) setnames(x@objects[[i]]@stat, old = pAttribute, new="key"))
    rm(dummy)
  }
  if (verbose == TRUE) message("... generating cumulated data.table")
  DT <- data.table::rbindlist(lapply(x@objects, function(y) y@stat))
  j <- unlist(lapply(c(1:length(x@objects)), function(i) rep(i, times = nrow(x@objects[[i]]@stat))))
  DT[, "j" := j, with = FALSE]
  DT <- DT[which(DT[["key"]] != "")] # to avoid errors
  if (verbose == TRUE) message("... getting unique keys")
  uniqueKeys <- unique(DT[["key"]])
  keys <- setNames(c(1:length(uniqueKeys)), uniqueKeys)
  if (verbose == TRUE) message("... generating integer keys")
  i <- keys[ DT[["key"]] ]
  retval <- simple_triplet_matrix(
    i = unname(i), j = DT[["j"]], v = DT[[col]],
    nrow = length(names(keys)), ncol = length(names(x@objects)),
    dimnames = list(Terms=names(keys), Docs=names(x@objects))
  )
  class(retval) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  
  if (verbose == TRUE) message("... cleaning up temporary key columns")
  if (length(pAttribute) > 1){
    dummy <- lapply(c(1:length(x@objects)), function(i) x@objects[[i]]@stat[, key := NULL])
  } else {
    dummy <- lapply(c(1:length(x@objects)), function(i) setnames(x@objects[[i]]@stat, old="key", new=pAttribute))
  }
  attr(retval, "weighting") <- c("term frequency", "tf")
  retval
})

#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "bundle", function(x, col) {
  as.DocumentTermMatrix(as.TermDocumentMatrix(x=x, col=col))
})

#' @rdname as.DocumentTermMatrix
setMethod("as.TermDocumentMatrix", "partitionBundle", function(x, pAttribute=NULL, col=NULL, verbose=TRUE){
  if (!is.null(col)){
    callNextMethod()
  } else if (!is.null(pAttribute)){
    if (verbose == TRUE) message("... generating corpus positions")
    cposList <- lapply(
      c(1:length(x@objects)),
      function(i) cbind(i, cpos(x@objects[[i]]@cpos))
    )
    cposMatrix <- do.call(rbind, cposList)
    if (verbose == TRUE) message("... getting ids")
    id_vector <- CQI$cpos2id(x[[1]]@corpus, pAttribute, cposMatrix[,2])
    DT <- data.table(i=cposMatrix[,1], id=id_vector, key=c("i", "id"))
    if (verbose == TRUE) message("... performing count")
    # TF <- DT[,.N, by=.(i, id)]
    TF <- DT[,.N, by=c("i", "id"), with=TRUE]
    setnames(TF, old="N", new="count")
    # TF[, pAttribute := as.utf8(CQI$id2str(x[[1]]@corpus, pAttribute, id)), with=FALSE]
    TF[, pAttribute := as.utf8(CQI$id2str(x[[1]]@corpus, pAttribute, TF[["id"]])), with=FALSE]
    if (verbose == TRUE) message("... generating keys")
    uniqueTerms <- unique(TF[[pAttribute]])
    keys <- setNames(c(1:length(uniqueTerms)), uniqueTerms)
    if (verbose == TRUE) message("... generating simple triplet matrix")
    retval <- simple_triplet_matrix(
      i = keys[ TF[[pAttribute]] ], j = TF[["i"]], v = TF[["count"]],
      dimnames = list(Terms=names(keys), Docs=names(x@objects))
    )
    class(retval) <- c("TermDocumentMatrix", "simple_triplet_matrix")
    return(retval)
  } else {
    message("... doing nothing, as pAttribute and col is NULL")
  }
})

#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "partitionBundle", function(x, pAttribute=NULL, col=NULL, verbose=TRUE){
  as.DocumentTermMatrix(as.TermDocumentMatrix(x=x, pAttribute=pAttribute, col=col, verbose=verbose))
})
