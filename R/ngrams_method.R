#' @exportClass ngrams
setClass(
  "ngrams",
  representation(
    n="integer", 
    corpus="character",
    encoding="character",
    size="integer",
    stat="data.table",
    pAttribute="character"
  ),
  contains=c("textstat")
)

#' get ngrams
#' 
#' @param .Object object
#' @param n number of token
#' @param pAttribute p-attribute
#' @exportMethod ngrams
#' @rdname ngrams
setGeneric("ngrams", function(.Object, ...) standardGeneric("ngrams"))

setMethod("ngrams", "partition", function(.Object, n=2, pAttribute="word"){
  cpos <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
  pAttr <- sapply(pAttribute, function(x) paste(.Object@corpus, ".", x, sep=""))
  ids <- cqi_cpos2id(pAttr, cpos)
  idList <- lapply(c(1:n), function(i) c(ids[c(i:(length(ids) - (n-i)))]))
  DT <- as.data.table(idList)
  setnames(DT, colnames(DT), paste("id_", c(1:n), sep=""))
  count <- function(x) return(x)
  TF <- DT[, count(.N), by=c(eval(colnames(DT))), with=TRUE]
  setnames(TF, "V1", "tf")
  lapply(
    c(1:n),
    function(i){
      str <- cqi_id2str(pAttr, TF[[i]]) %>% as.utf8(from="latin1")
      TF[, eval(paste("token_", i, sep="")) := str , with=TRUE] 
    })
  lapply(paste("id_", c(1:n), sep=""), function(x) TF[, eval(x) := NULL, with=TRUE])
  new(
    "ngrams",
    n=as.integer(n), corpus=.Object@corpus, encoding=.Object@encoding,
    size=as.integer(size(.Object)), stat=TF, pAttribute=pAttribute)
})

