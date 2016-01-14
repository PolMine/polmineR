#' @rdname getTermFrequencies-method
setGeneric("getTermFrequencies", function(.Object, ...) standardGeneric("getTermFrequencies"))


#' get term frequencies
#' 
#' Get term frequencies based on corpus positions. Parallelization
#' is possible. (Performance gains decrease with the use
#' of additional cores, as all cores need to access the corpus on disk.)
#'
#' @param .Object a partition object
#' @param corpus the corpus to use
#' @param pAttribute length 1 or 2 character vector, first element is supposed to be "word" or "lemma", second (optionally) "pos"
#' @param id2str logical, whether to add rownames
#' @param mc logical, whether to use multicore
#' @param ... further parameters
#' @rdname getTermFrequencies-method
#' @exportMethod getTermFrequencies
setMethod("getTermFrequencies", "partition", function(.Object, pAttribute=c("word", "pos"), id2str=TRUE, mc=TRUE){
  cpos <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
  pAttr <- sapply(pAttribute, function(x) paste(.Object@corpus, ".", x, sep=""))
  pAttr_id <- paste(pAttribute, "_id", sep="")
  if (length(pAttribute) == 1){
    TF <- getTermFrequencies(cpos, .Object@corpus, pAttribute)
  } else {
    idList <- lapply(pAttr, function(p) cqi_cpos2id(p, cpos))
    names(idList) <- paste(pAttribute, "_id", sep="")
    ID <- as.data.table(idList)
    setkeyv(ID, cols=names(idList))
    count <- function(x) return(x)
    TF <- ID[, count(.N), by=c(eval(names(idList))), with=TRUE]
    setnames(TF, "V1", "count")
  }
  if (id2str == TRUE){
    dummy <- lapply(
      c(1:length(pAttribute)),
      function(i){
        str <- cqi_id2str(pAttr[i], TF[[pAttr_id[i]]]) %>% as.utf8(from=.Object@encoding)
        TF[, eval(pAttribute[i]) := str , with=TRUE] 
      })
    dummy <- lapply(pAttr_id, function(x) TF[, eval(x) := NULL, with=TRUE])
    setcolorder(TF, neworder=c(pAttribute, "count"))
  } else {
    setcolorder(TF, neworder=c(pAttr_id, "count"))
  }
  
  # setorderv(TF, "V1", order=-1)
  TF
})

#' @rdname getTermFrequencies-method
setMethod("getTermFrequencies", "vector", function(.Object, corpus, pAttribute){
  ids <- cqi_cpos2id(paste(corpus, ".", pAttribute, sep=""), .Object)
  count <- tabulate(ids)
  TF <- data.table(
    id=c(0:length(count)),
    count=c(length(which(ids == 0)), count)
  )
  setkey(TF, "id")
  setnames(TF, "id", paste(pAttribute, "_id", sep=""))
  TF[count > 0]
})



# #' @rdname getTermFrequencies-method
# setMethod("getTermFrequencies", "list", function(.Object, pAttributes, corpus, encoding){
#   pAttributeIds <- .Object
#   chunks <- split(pAttributeIds[[1]], pAttributeIds[[2]])  
#   chunksTabulated <- lapply(chunks, function(x) getTermFrequencies(x))
#   chunksTabulatedWithRownames <- lapply(
#     chunksTabulated,
#     function(count) .id2str(count, corpus=corpus, pAttribute=pAttributes[1], encoding=encoding)
# #    , mc.cores=ifelse(mc==FALSE, 1, mc)
#   )
#   chunksTabulatedWithEnhancedRownames <- lapply(
#     names(chunksTabulatedWithRownames),
#     function(id){
#       count <- chunksTabulatedWithRownames[[id]]
#       idAsStr <- cqi_id2str(paste(corpus, '.', pAttributes[2], sep=''), as.numeric(id))
#       Encoding(idAsStr) <- encoding
#       idAsStr <- enc2utf8(idAsStr)
#       count[["token"]] <- paste(count[["token"]], "//", idAsStr, sep="")
#       setcolorder(count, c("token", "ids", "count"))
#       setkey(count, "token")
#       count
#     }
# #    , mc.cores=ifelse(mc==FALSE, 1, mc)
#     )
#   retval <- rbindlist(chunksTabulatedWithEnhancedRownames)
#   setkey(retval, "token")
#   retval
# })
