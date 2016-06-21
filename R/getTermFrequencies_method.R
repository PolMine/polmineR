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
    idList <- lapply(pAttribute, function(p) CQI$cpos2id(.Object@corpus, p, cpos))
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
        str <- as.utf8(CQI$id2str(.Object@corpus, pAttribute[i], TF[[pAttr_id[i]]]), from=.Object@encoding)
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
  ids <- CQI$cpos2id(corpus, pAttribute, .Object)
  count <- tabulate(ids)
  TF <- data.table(
    id=c(0:length(count)),
    count=c(length(which(ids == 0)), count)
  )
  setkey(TF, "id")
  setnames(TF, "id", paste(pAttribute, "_id", sep=""))
  TF[count > 0]
})
