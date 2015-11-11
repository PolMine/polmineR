setGeneric("getTermFrequencies", function(.Object, ...) standardGeneric("getTermFrequencies"))

.expandCposMatrix <- function(cposMatrix) unlist(apply(cposMatrix, 1, function(x) x[1]:x[2]))

.id2str <- function(tf, corpus, pAttribute, encoding){
  token <- cqi_id2str(paste(corpus, '.', pAttribute, sep=''), tf[["ids"]])
  Encoding(token) <- encoding
  token <- enc2utf8(token)
  Encoding(token) <- "unknown"
  tf[["token"]] <- token
  setcolorder(tf, c("token", "ids", "tf"))
  setkey(tf, "token")
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
#' @rdname getTermFrequencies
#' @exportMethod getTermFrequencies
setMethod("getTermFrequencies", "partition", function(.Object, pAttribute, id2str=TRUE, mc=FALSE){
  stopifnot(length(pAttribute) <= 2)
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
    tf <- getTermFrequencies(ids)
    if (id2str == TRUE){   
      try(tf <- .id2str(
        tf, corpus=.Object@corpus, pAttribute=pAttribute, encoding=.Object@encoding
        ), silent=FALSE)
    }
    return(tf)
  } else if(length(pAttribute == 2)){
    .cpos2ids <- function(corpus, pAttribute, cpos) cqi_cpos2id(paste(.Object@corpus, '.', pAttribute, sep=''), cpos)
    cpos <- .expandCposMatrix(.Object@cpos)
    pAttributeIds <- lapply(
      c(1,2),
      function(i) .cpos2ids(corpus=.Object@corpus, pAttribute=pAttribute[i], cpos=cpos)
#      , mc.cores=ifelse(mc==FALSE, 1, 2)
    )
    retval <- getTermFrequencies(
      .Object=pAttributeIds, pAttributes=pAttribute,
      corpus=.Object@corpus, encoding=.Object@encoding
      )
    return(retval)
  }
})


setMethod("getTermFrequencies", "list", function(.Object, pAttributes, corpus, encoding){
  pAttributeIds <- .Object
  chunks <- split(pAttributeIds[[1]], pAttributeIds[[2]])  
  chunksTabulated <- lapply(chunks, function(x) getTermFrequencies(x))
  chunksTabulatedWithRownames <- lapply(
    chunksTabulated,
    function(tf) .id2str(tf, corpus=corpus, pAttribute=pAttributes[1], encoding=encoding)
#    , mc.cores=ifelse(mc==FALSE, 1, mc)
  )
  chunksTabulatedWithEnhancedRownames <- lapply(
    names(chunksTabulatedWithRownames),
    function(id){
      tf <- chunksTabulatedWithRownames[[id]]
      idAsStr <- cqi_id2str(paste(corpus, '.', pAttributes[2], sep=''), as.numeric(id))
      Encoding(idAsStr) <- encoding
      idAsStr <- enc2utf8(idAsStr)
      tf[["token"]] <- paste(tf[["token"]], "//", idAsStr, sep="")
      setcolorder(tf, c("token", "ids", "tf"))
      setkey(tf, "token")
      tf
    }
#    , mc.cores=ifelse(mc==FALSE, 1, mc)
    )
  retval <- rbindlist(chunksTabulatedWithEnhancedRownames)
  setkey(retval, "token")
  retval
})


setMethod("getTermFrequencies", "vector", function(.Object){
  count <- tabulate(.Object)
  stat <- data.table(
      ids=c(0:length(count)),
      tf=c(length(count[which(count==0)]), count)
  )
  setkey(stat, "ids")
  subset(stat, tf > 0)
})
