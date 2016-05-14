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
#' @param from bla
#' @param to bla
#' @param strucs bla
#' @param rmBlank bla
#' @param verbose bla
#' @param robust bla
#' @param mc logical
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
#' \dontrun{
#' foo <- as.TermDocumentMatrix(
#'   x="ZEIT", pAttribute="word", sAttribute="text_id",
#'   from="1946_01_auf-einen-von-bomben-zerschlagenen-engel.html",
#'   to="1951_08_gespraeche-zum-interzonenhandel.html", robust="LESUNG", mc=TRUE, verbose=TRUE, rmBlank=TRUE
#' )
#' }
#' @rdname as.DocumentTermMatrix
setMethod(
  "as.TermDocumentMatrix", "character",
  function (
    x, pAttribute, sAttribute, from=NULL, to=NULL, strucs=NULL,
    rmBlank=TRUE, verbose=TRUE, robust=FALSE, mc=FALSE
  ) {
    sAttr <- paste(x, ".", sAttribute, sep="")
    pAttr <- paste(x, ".", pAttribute, sep="")
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
        toStruc <- cqi_attribute_size(sAttr) - 1
        fromStruc <- 0
        strucs <- c(0:toStruc) 
      }
    }
    .freqMatrix <- function(i){
      struc <- strucs[i]
      cpos <- cqi_struc2cpos(sAttr, struc)
      ids <- cqi_cpos2id(pAttr, c(cpos[1]:cpos[2]))
      freqVector <- tabulate(ids + 1)
      noZeroCount <- which(freqVector != 0)
      freqMatrix <- matrix(
        c(noZeroCount, freqVector[noZeroCount]),
        ncol=2, byrow=FALSE
      )
      cbind(rep(i, times=nrow(freqMatrix)), freqMatrix)
    }
    if (verbose == TRUE) message("... computing term frequencies")
    if (mc == FALSE){
      freqMatrixList <- lapply(
        c(1:length(strucs)), function(struc){
          if (verbose == TRUE) message("... processing struc", struc)
          .freqMatrix(struc)
        })
    } else if (mc == TRUE) {
      coresToUse <- getOption("polmineR.cores")
      if (verbose == TRUE) message("... using ", coresToUse, " cores")
      freqMatrixList <- mclapply(c(1:length(strucs)), .freqMatrix, mc.cores=coresToUse)
    }
    if (verbose == TRUE) message("... combining results")
    freqMatrixAgg <- do.call(rbind, freqMatrixList)
    lexiconSize <- cqi_lexicon_size(pAttr)
    if (verbose == TRUE) message("... id2str for pAttribute")
    # pAttributeStrings <- cqi_id2str(pAttr, c(0:lexiconSize)) # slow!
    pAttributeStrings <- getTerms(x, pAttribute=pAttribute, robust=robust)
    pAttributeStrings <- iconv(pAttributeStrings, from=getEncoding(x), to="UTF-8")                            
    if (!exists("sAttributeStrings")){
      if (verbose == TRUE) message("... id2str for sAttribute")
      sAttributeStrings <- cqi_struc2str(sAttr, c(fromStruc:toStruc))  
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
setMethod("as.TermDocumentMatrix", "bundle", function(x, col, pAttribute=NULL, verbose=TRUE){
  if (is.null(pAttribute)){
    pAttribute <- x@objects[[1]]@pAttribute
    if (verbose == TRUE) message("... using the pAttribute-slot of the first object in the bundle as pAttribute: ", pAttribute)
  }
  stopifnot(
    col %in% colnames(x@objects[[1]]),
    pAttribute %in% colnames(x@objects[[1]])
  )
  if (verbose == TRUE) message("... generating (temporary) key column")
  if (length(pAttribute) > 1){
    lapply(
      c(1:length(x@objects)),
      function(i){
        keys <- x@objects[[i]]@stat[, c(pAttribute), with=FALSE] %>% apply(1, function(row) paste(row, collapse="//"))
        x@objects[[i]]@stat[, key := keys]
      })
  } else {
    lapply(c(1:length(x@objects)), function(i) setnames(x@objects[[i]]@stat, old=pAttribute, new="key"))
  }
  if (verbose == TRUE) message("... generating cumulated data.table")
  DT <- rbindlist(lapply(x@objects, function(y) y@stat))
  
  if (verbose == TRUE) message("... getting unique keys")
  uniqueKeys <- unique(DT[["key"]])
  keys <- setNames(c(1:length(uniqueKeys)), uniqueKeys)
  if (verbose == TRUE) message("... generating integer keys")
  i <- keys[ DT[["key"]] ]
  j <- unlist(lapply(c(1:length(x@objects)), function(i) rep(i, times = nrow(x@objects[[i]]@stat))))
  retval <- simple_triplet_matrix(
    i = i, j = j, v = DT[[col]],
    dimnames = list(Terms=names(keys), Docs=names(x@objects))
  )
  class(retval) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  
  if (verbose == TRUE) message("... cleaning up temporary key columns")
  if (length(pAttribute) > 1){
    lapply(c(1:length(x@objects)), function(i) x@objects[[i]]@stat[, key := NULL])
  } else {
    lapply(c(1:length(x@objects)), function(i) setnames(x@objects[[i]]@stat, old="key", new=pAttribute))
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
  id_vector <- cqi_cpos2id(paste(x[[1]]@corpus, pAttribute, sep="."), cposMatrix[,2])
  DT <- data.table(i=cposMatrix[,1], id=id_vector, key=c("i", "id"))
  if (verbose == TRUE) message("... performing count")
  TF <- DT[,.N, by=.(i, id)]
  setnames(TF, old="N", new="count")
  TF[, pAttribute := as.utf8(cqi_id2str(paste(x[[1]]@corpus, pAttribute, sep="."), id)), with=FALSE]
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
