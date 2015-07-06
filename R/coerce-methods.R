#' @include polmineR-package.R partition-class.R partitionCluster-class.R context-class.R collocations-class.R contextCluster-class.R
NULL


#' coerce object to TermDocumentMatrix/DocumentTermMatrix
#' 
#' A set of methods to transform objects into the TermDocumentMatrix-objects, or
#' DocumentTermMatrix-objects imported from the \code{tm} package.
#' 
#' @param x some object
#' @param pAttribute the p-attribute
#' @param sAttribute the s-attribute
#' @param from bla
#' @param to bla
#' @param strucs bla
#' @param rmBlank bla
#' @param weight whether to introduce a weight ("tfidf" and "rel" are implemented)
#' @param verbose bla
#' @param robust bla
#' @param mc logical
#' @param ... to make the check happy
#' @return a TermDocumentMatrix
#' @author Andreas Blaette
#' @exportMethod as.TermDocumentMatrix
#' @docType methods
#' @rdname coerce-methods
#' @name as.TermDocumentMatrix
#' @author me
setGeneric("as.TermDocumentMatrix", function(x, ...){UseMethod("as.TermDocumentMatrix")})
setGeneric("as.DocumentTermMatrix", function(x, ...){UseMethod("as.DocumentTermMatrix")})

#' @exportMethod as.sparseMatrix
setGeneric("as.sparseMatrix", function(x,...){standardGeneric("as.sparseMatrix")})
setGeneric("as.partitionCluster", function(object,...){standardGeneric("as.partitionCluster")})
setGeneric("as.TermContextMatrix", function(x, col, ...) {standardGeneric("as.TermContextMatrix")})

#' @param mat a TermDocumentMatrix
#' @param verbose logicla, whether to be talkative
#' @return a TermDocumentMatrix
#' @importFrom slam as.simple_triplet_matrix
#' 
#' @noRd
.rmBlank <- function(mat, verbose=TRUE){
  if (verbose==TRUE) message("... removing empty rows")
  matTmp <- as.sparseMatrix(mat)
  matTmp <- matTmp[which(rowSums(matTmp) > 0),]
  mat <- as.simple_triplet_matrix(matTmp)
  class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  mat
}


#' @docType methods
#' @importFrom Matrix sparseMatrix
setMethod("as.sparseMatrix", "TermDocumentMatrix", function(x){
  retval <-  sparseMatrix(i=x$i,
                          j=x$j,
                          x=x$v,
                          dims=c(x$nrow, x$ncol),
                          dimnames = dimnames(x),
                          giveCsparse = TRUE)
  return(retval)  
})

#' @method as.TermDocumentMatrix partitionCluster
#' @importFrom slam simple_triplet_matrix
#' @importFrom tm as.TermDocumentMatrix
#' @rdname coerce-methods
#' @docType methods
setMethod("as.TermDocumentMatrix", "partitionCluster", function (x, pAttribute=NULL, weight=NULL, rmBlank=TRUE, verbose=TRUE, ...) {
  encoding <- unique(unlist(lapply(x@objects, function(c) c@encoding)))
  if (is.null(pAttribute)){
    pAttributesAvailable <- unique(unlist(lapply(x@objects, function(x) names(x@tf))))
    if (length(pAttributesAvailable) == 1){
      pAttribute <- pAttributesAvailable
      if (verbose == TRUE) message("... preparing TermDocumentMatrix for pAttribute ", pAttribute)
    } else {
      warning("term frequencies are available for more than one tf, please provide pAttribute to be used explicitly")
    }
  }
  corpus <- unique(unlist(lapply(x@objects, function(c) c@corpus)))
  message("... putting together the matrix")
  i <- as.integer(unname(unlist(lapply(x@objects,
                                       function(c) {a <- c@tf[[pAttribute]][,1]
                                                    a <- a+1
                                                    a})
  )))
  j <- unlist(lapply(c(1:length(x@objects)),
                     function(m) {rep(
                       m,times=nrow(x@objects[[m]]@tf[[pAttribute]])
                     )
                     }
  ))
  v <- as.integer(unlist(lapply(x@objects, function(c) c@tf[[pAttribute]][,2])))
  attr <- paste(corpus, '.', pAttribute, sep='')
  lexicon.size <- cqi_lexicon_size(attr)
  mat <- simple_triplet_matrix(i=i, j=j, v=v,
                               ncol=length(x@objects),
                               nrow=lexicon.size+1,
                               dimnames=list(
                                 Terms=cqi_id2str(attr, c(0:lexicon.size)),
                                 Docs=names(x@objects))
  )
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")  
  class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  if (rmBlank == TRUE) mt <- .rmBlank(mat, verbose=verbose)
  if (!is.null(weight)){
    if (weight == "tfidf"){
      message("... applying tf/idf as a weight")
      mat <- weigh(mat, method="tfidf", corpusSizes=summary(x)$token)
    } else if (weight == "rel"){
      message("... computing relative frequencies")
      mat <- weigh(mat, method="rel", corpusSizes=summary(x)$token)
    }
  }
  mat
})


#' @docType methods
#' @noRd
setMethod("as.sparseMatrix", "partitionCluster", function(x, pAttribute, ...){
  message("... converting partitionCluster to TermDocumentMatrix")
  tdm_stm <- as.TermDocumentMatrix(x, pAttribute=pAttribute)
  message("... converting TermDocumentMatrix to Matrix")
  retval <-  as.sparseMatrix(tdm_stm)
  return(retval)
})


#' @rdname coerce-methods
setMethod("as.DocumentTermMatrix", "partitionCluster", function(x, pAttribute=NULL, weight=NULL, rmBlank=TRUE, ...) {
  retval <- as.DocumentTermMatrix(as.TermDocumentMatrix(x, pAttribute=NULL, weight=weight, rmBlank=rmBlank))
  retval
})






#' Transform a context cluster into a Term Context Matrix
#' 
#' Method based on the tm package, adds to as.TermDocumentMatrix
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a contextCluster object (S3 class)
#' @param col the col of the stat table to take
#' @param ... to make the check happy
#' @method as.TermContextMatrix contextCluster
#' @return a TermContextMatrix
#' @author Andreas Blaette
#' @docType method
#' @importFrom slam simple_triplet_matrix
#' @exportMethod as.TermContextMatrix
#' @noRd
setMethod("as.TermContextMatrix", "contextCluster", function (x, col, ...) {
  encoding <- unique(unlist(lapply(x@objects, function(c) c@encoding)))
  corpus <- unique(unlist(lapply(x@objects, function(c) c@corpus)))
  pAttribute <- unique(unlist(lapply(x@objects, function(c) c@pAttribute)))
  pAttr <- paste(corpus, '.', pAttribute, sep='')
  i <- unlist(lapply(x@objects, function(c) (cqi_str2id(pAttr, rownames(c@stat))+1)))
  j <- unlist(lapply(c(1:length(x@objects)), function(m) {rep(m,times=nrow(x[[m]]@stat))}))
  v <- unlist(lapply(x@objects, function(c) c@stat[,col]))
  lexiconSize <- cqi_lexicon_size(pAttr)
  mat <- simple_triplet_matrix(i=i, j=j, v=v,
                               ncol=length(x@objects),
                               nrow=lexiconSize+1,
                               dimnames=list(
                                 Terms=cqi_id2str(pAttr, c(0:lexiconSize)),
                                 Docs=names(x@objects))
  )
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")
  class(mat) <- c("TermContextMatrix", "TermDocumentMatrix", "simple_triplet_matrix")
  mat
})


#' Get statistics table from an object
#' 
#' @param x object with a statistics table
#' @param row.names defaults to NULL
#' @param optional see documentation of as.data.frame
#' @param ... any further arguments
#' @rdname asDataFrame-method
#' @name as.data.frame
#' @aliases as.data.frame,keyness-method as.data.frame,context-method
#' @exportMethod as.data.frame
#' @docType methods
setMethod("as.data.frame", "keyness", function(x, ...) x@stat )
setMethod("as.data.frame", "context", function(x, ...) x@stat )

#' @docType methods
#' @exportMethod as.partitionCluster
setMethod("as.partitionCluster", "partition", function(object){
  newCluster <- new("partitionCluster")
  newCluster@objects[[1]] <- object
  names(newCluster@objects)[1] <- object@label
  newCluster@corpus <- object@corpus
  newCluster@encoding <- object@encoding
  newCluster@explanation <- c("derived from a partition object")
  newCluster
})

setMethod("as.partitionCluster", "list", function(object, ...){
  if (!all(unlist(lapply(object, class))=="partition")) warning("all objects in list need to be partition objects")
  newCluster <- new("partitionCluster")
  newCluster@objects <- object
  newCluster@corpus <- unique(unlist(lapply(newCluster@objects, function(x) x@corpus)))
  newCluster@encoding <- unique(unlist(lapply(newCluster@objects, function(x) x@encoding)))
  names(newCluster@objects) <- vapply(newCluster@objects, function(x) x@label, FUN.VALUE="character")
  newCluster
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

#' @exportMethod as.partitionCluster
#' @rdname context-class
setMethod("as.partitionCluster", "context", function(object, mc=FALSE){
  newPartitionCluster <- new(
    "partitionCluster",
    corpus=object@corpus,
    encoding=object@encoding,
    explanation="this partitionCluster is derived from a context object"
    )
  .makeNewPartition <- function(cpos){
    newPartition <- new(
      "partition",
      corpus=object@corpus,
      encoding=object@encoding,
      cpos=matrix(c(cpos[["left"]][1], cpos[["right"]][length(cpos[["right"]])]), ncol=2)
    )
    newPartition <- enrich(newPartition, size=TRUE, tf=object@pAttribute)
    newPartition@strucs <- c(
      cqi_cpos2struc(paste(object@corpus, ".", object@sAttribute, sep=""), newPartition@cpos[1,1])
      :
        cqi_cpos2struc(paste(object@corpus, ".", object@sAttribute, sep=""), newPartition@cpos[1,2])
    )
    newPartition
  }
  if (mc == FALSE){
    newPartitionCluster@objects <- lapply(object@cpos, FUN=.makeNewPartition)  
  } else {
    coresToUse <- slot(get("session", ".GlobalEnv"), "cores")
    newPartitionCluster@objects <- mclapply(object@cpos, FUN=.makeNewPartition, mc.cores=coresToUse)  
  }
  return(newPartitionCluster)
})

