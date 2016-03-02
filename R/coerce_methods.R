#' @include polmineR_package.R partition_class.R partitionBundle_class.R context_class.R cooccurrences_class.R contextBundle_class.R
NULL


#' coerce object to matrix
#' 
#' Coerce an object, usually a bundle (or an object inheriting from bundle) to
#' a TermDocumentMatrix, DocumentTermMatrix, sparseMatrix etc.
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
#' @rdname coerce-methods
#' @name as.TermDocumentMatrix
#' @author me
setGeneric("as.TermDocumentMatrix", function(x, ...){UseMethod("as.TermDocumentMatrix")})
setGeneric("as.DocumentTermMatrix", function(x, ...){UseMethod("as.DocumentTermMatrix")})

#' @exportMethod as.sparseMatrix
setGeneric("as.sparseMatrix", function(x,...){standardGeneric("as.sparseMatrix")})


#' @docType methods
#' @importFrom Matrix sparseMatrix
#' @exportMethod as.sparseMatrix
setMethod("as.sparseMatrix", "TermDocumentMatrix", function(x){
  sparseMatrix(
    i=x$i, j=x$j, x=x$v,
    dims=c(x$nrow, x$ncol),
    dimnames = dimnames(x),
    giveCsparse = TRUE
    )
})




#' @examples
#' \dontrun{
#' foo <- as.TermDocumentMatrix(
#'   x="ZEIT", pAttribute="word", sAttribute="text_id",
#'   from="1946_01_auf-einen-von-bomben-zerschlagenen-engel.html",
#'   to="1951_08_gespraeche-zum-interzonenhandel.html", robust="LESUNG", mc=TRUE, verbose=TRUE, rmBlank=TRUE
#' )
#' }
#' @rdname coerce-methods
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
      coresToUse <- slot(get("session", ".GlobalEnv"), "cores")
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

setGeneric("as.partitionBundle", function(object, ...) standardGeneric("as.partitionBundle"))

#' @rdname partitionBundle-class
setMethod("as.partitionBundle", "list", function(object, ...){
  as(object, "bundle") # defined in bundle_class.R
})

#' @exportMethod as.partitionBundle
#' @rdname context-class
setMethod("as.partitionBundle", "context", function(object, mc=FALSE){
  newPartitionBundle <- new(
    "partitionBundle",
    corpus=object@corpus,
    encoding=object@encoding,
    explanation="this partitionBundle is derived from a context object"
    )
  .makeNewPartition <- function(cpos){
    newPartition <- new(
      "partition",
      corpus=object@corpus,
      encoding=object@encoding,
      cpos=matrix(c(cpos[["left"]][1], cpos[["right"]][length(cpos[["right"]])]), ncol=2)
    )
    newPartition <- enrich(newPartition, size=TRUE, pAttribute=object@pAttribute)
    newPartition@strucs <- c(
      cqi_cpos2struc(paste(object@corpus, ".", object@sAttribute, sep=""), newPartition@cpos[1,1])
      :
        cqi_cpos2struc(paste(object@corpus, ".", object@sAttribute, sep=""), newPartition@cpos[1,2])
    )
    newPartition
  }
  if (mc == FALSE){
    newPartitionBundle@objects <- lapply(object@cpos, FUN=.makeNewPartition)  
  } else {
    coresToUse <- slot(get("session", ".GlobalEnv"), "cores")
    newPartitionBundle@objects <- mclapply(object@cpos, FUN=.makeNewPartition, mc.cores=coresToUse)  
  }
  return(newPartitionBundle)
})


.rmBlank <- function(mat, verbose=TRUE){
  if (verbose==TRUE) message("... removing empty rows")
  matTmp <- as.sparseMatrix(mat)
  matTmp <- matTmp[which(rowSums(matTmp) > 0),]
  mat <- as.simple_triplet_matrix(matTmp)
  class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  mat
}


#' @rdname coerce-methods
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

#' @rdname bundle-class
setMethod("as.DocumentTermMatrix", "bundle", function(x, col) {
  retval <- as.DocumentTermMatrix(as.TermDocumentMatrix(x=x, col=col))
  retval
})

#' @docType methods
#' @exportMethod as.partitionBundle
#' @rdname bundle-class
setMethod("as.bundle", "textstat", function(object){
  newBundle <- new(
    paste(is(object)[1], "Bundle", sep=""),
    objects=list(object),
    corpus=object@corpus,
    encoding=object@encoding,
    explanation=c("derived from a partition object")
  )
  names(newBundle@objects)[1] <- object@name
  newBundle
})

#' @docType methods
#' @rdname bundle-class
setMethod("as.sparseMatrix", "bundle", function(x, col){
  message("... converting partitionBundle to TermDocumentMatrix")
  tdm_stm <- as.TermDocumentMatrix(x=x, col=col)
  message("... converting TermDocumentMatrix to Matrix")
  retval <-  as.sparseMatrix(tdm_stm)
  return(retval)
})
