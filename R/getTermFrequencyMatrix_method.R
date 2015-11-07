setGeneric("getTermFrequencyMatrix", function(.Object, ...) standardGeneric("getTermFrequencyMatrix"))

.expandCposMatrix <- function(cposMatrix) unlist(apply(cposMatrix, 1, function(x) x[1]:x[2]))
.addRownames <- function(tf, corpus, pAttribute, encoding){
  rownames(tf) <- cqi_id2str(paste(corpus, '.', pAttribute, sep=''), tf[,"id"])
  Encoding(rownames(tf)) <- encoding
  rownames(tf) <- enc2utf8(rownames(tf))
  tf
}



#' get term frequencies
#' 
#' Get term frequencies based on corpus positions. Parallelization
#' is possible. (Performance gains decrease with the use
#' of additional cores, as all cores need to access the corpus on disk.)
#'
#' @param .Object a partition object
#' @param pAttribute length 1 or 2 character vector, first element is supposed to be "word" or "lemma", second (optionally) "pos"
#' @param id2str logical, whether to add rownames
#' @param mc logical, whether to use multicore
#' @rdname getTermFrequencyMatrix
#' @exportMethod getTermFrequencyMatrix
setMethod("getTermFrequencyMatrix", "partition", function(.Object, pAttribute, id2str=TRUE, mc=FALSE){
  stopifnot(length(pAttribute) <= 2)
  .cpos2ids <- function(corpus, pAttribute, cpos) cqi_cpos2id(paste(.Object@corpus, '.', pAttribute, sep=''), cpos)
  if (length(pAttribute) == 1){
    .cposMatrix2ids <- function(cposMatrix){
      cpos <- .expandCposMatrix(cposMatrix)
      ids <- cqi_cpos2id(paste(.Object@corpus, '.', pAttribute, sep=''), cpos)
    }
    if (mc == FALSE){
      ids <- .cposMatrix2ids(cposMatrix=.Object@cpos)
    } else {
      noCores <- ifelse(is.numeric(mc), mc, getOption("mc.cores", 2L))
      chunkList <- .splitMatrixIntoEquallySizedParts(x=.Object@cpos, n=noCores)
      idList <- mclapply(chunkList, .cposMatrix2ids, mc.cores=noCores)
      ids <- unlist(idList, recursive=FALSE, use.names=FALSE)
    }
    tf <- getTermFrequencyMatrix(ids)
    if (id2str == TRUE){   
      try(tf <- .addRownames(
        tf, corpus=.Object@corpus, pAttribute=pAttribute, encoding=.Object@encoding
        ), silent=FALSE)
    }
    return(tf)
  } else if(length(pAttribute == 2)){
    cpos <- .expandCposMatrix(.Object@cpos)
    pAttributeIds <- lapply(
      c(1,2),
      function(i) .cpos2ids(corpus=.Object@corpus, pAttribute=pAttribute[i], cpos=cpos)
#      , mc.cores=ifelse(mc==FALSE, 1, 2)
    )
    retval <- getTermFrequencyMatrix(
      .Object=pAttributeIds, pAttributes=pAttribute,
      corpus=.Object@corpus, encoding=.Object@encoding
      )
    return(retval)
  }
})


setMethod("getTermFrequencyMatrix", "list", function(.Object, pAttributes, corpus, encoding){
  pAttributeIds <- .Object
  chunks <- split(pAttributeIds[[1]], pAttributeIds[[2]])  
  chunksTabulated <- lapply(chunks, function(x) getTermFrequencyMatrix(x))
  chunksTabulatedWithRownames <- lapply(
    chunksTabulated,
    function(tf) .addRownames(tf, corpus=corpus, pAttribute=pAttributes[1], encoding=encoding)
#    , mc.cores=ifelse(mc==FALSE, 1, mc)
  )
  chunksTabulatedWithEnhancedRownames <- lapply(
    names(chunksTabulatedWithRownames),
    function(id){
      tf <- chunksTabulatedWithRownames[[id]]
      idAsStr <- cqi_id2str(paste(corpus, '.', pAttributes[2], sep=''), as.numeric(id))
      Encoding(idAsStr) <- encoding
      idAsStr <- enc2utf8(idAsStr)
      rownames(tf) <- paste(rownames(tf), "//", idAsStr, sep="")
      tf
    }
#    , mc.cores=ifelse(mc==FALSE, 1, mc)
    )
  retval <- do.call(rbind, chunksTabulatedWithEnhancedRownames)
})


setMethod("getTermFrequencyMatrix", "vector", function(.Object){
  ids <- .Object  
  tfRaw <- tabulate(ids)
  tf <- matrix(
    c(
      c(0:length(tfRaw)),
      c(length(ids[which(ids==0)]), tfRaw)
    ),
    ncol=2, dimnames=list(NULL, c("id", "tf"))
  )
  tf <- matrix(tf[which(tf[,"tf"] > 0),], ncol=2, dimnames=list(NULL, c("id", "tf")))
  return(tf)
})
