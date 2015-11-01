setGeneric("getTermFrequencyMatrix", function(.Object, ...) standardGeneric("getTermFrequencyMatrix"))

#' get term frequencies
#' 
#' Get term frequencies based on corpus positions. Parallelization
#' is possible. As it seems, performance gains decrease with the use
#' of additional cores, as all cores need to access the corpus on disk.
#'
#' @param .Object a partition object
#' @param pAttribute either 'word' or 'lemma'
#' @param id2str logical, whether to add rownames
#' @param mc logical, whether to use multicore
#' @noRd
setMethod("getTermFrequencyMatrix", "partition", function(.Object, pAttribute, id2str=TRUE, mc=FALSE){
  .cposMatrix2ids <- function(cposMatrix){
    cpos <- unlist(apply(cposMatrix, 1, function(x) x[1]:x[2]))
    ids <- cqi_cpos2id(paste(.Object@corpus, '.', pAttribute, sep=''), cpos)
  }
  if (mc == FALSE){
    ids <- .cposMatrix2ids(cposMatrix=.Object@cpos)
  } else {
    noCores <- ifelse(is.numeric(mc), mc, getOption("mc.cores", 2L))
    chunkFactor <- cut(
      c(1:nrow(.Object@cpos)),
      c(1, floor(c(1:(noCores-1))*(nrow(.Object@cpos)/noCores)), nrow(.Object@cpos)),
      include.lowest=TRUE
    )
    chunkList <- split(.Object@cpos, chunkFactor)
    idList <- mclapply(
      chunkList,
      function(chunk) .cposMatrix2ids(matrix(data=chunk, ncol=2)),
      mc.cores=noCores
      )
    ids <- unlist(idList, recursive=FALSE, use.names=FALSE)
  }
  tfRaw <- tabulate(ids)
  tf <- matrix(
    c(
      c(0:length(tfRaw)),
      c(length(ids[which(ids==0)]), tfRaw)
    ),
    ncol=2, dimnames=list(NULL, c("id", "tf"))
  )
  tf <- tf[which(tf[,"tf"] > 0),]
  if (id2str == TRUE){   
    .addRownames <- function(tf){
      rownames(tf) <- cqi_id2str(paste(.Object@corpus, '.', pAttribute, sep=''), tf[,"id"])
      Encoding(rownames(tf)) <- .Object@encoding
      tf
    }
    try(tf <- .addRownames(tf), silent=FALSE)
  }
  tf
})
