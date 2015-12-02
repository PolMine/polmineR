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
#' @param .Object object of class \code{partition}
#' @param n number of tokens
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

setMethod("ngrams", "partition", function(.Object, n=2, pAttribute="word"){
  cpos <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
  pAttrs <- sapply(pAttribute, function(x) paste(.Object@corpus, ".", x, sep=""))
  idListBase <- lapply(pAttrs, function(pAttr) cqi_cpos2id(pAttr, cpos))
  idList <- list()
  j <- 0
  for (i in c(1:n)){
    for (pAttr in pAttribute){
      j <- j + 1
      idList[[j]] <- c(idListBase[[pAttr]][c(i:(length(idListBase[[pAttr]]) - (n-i)))])
      names(idList)[j] <- paste("id_", i, "_", pAttr, sep="")
    }
  }
  DT <- as.data.table(idList)
  count <- function(x) return(x)
  TF <- DT[, count(.N), by=c(eval(colnames(DT))), with=TRUE]
  setnames(TF, "V1", "tf")
  pAttrsCols <- rep(pAttrs, times=n)
  tokenNo <- unlist(lapply(c(1:n), function(x) rep(x, times=length(pAttribute))))
  # convert ids to strings
  dummy <- lapply(
    c(1:(n*length(pAttribute))),
    function(i){
      str <- cqi_id2str(pAttrsCols[i], TF[[i]]) %>% as.utf8(from="latin1")
      TF[, eval(paste("token_", tokenNo[i], "_", names(pAttrsCols)[i], sep="")) := str , with=TRUE] 
    })
  # remove columns with ids
  lapply(
    grep("id_", colnames(TF), value=TRUE),
    function(x) TF[, eval(x) := NULL, with=TRUE]
    )
  new(
    "ngrams",
    n=as.integer(n), corpus=.Object@corpus, encoding=.Object@encoding,
    size=as.integer(length(.Object)), stat=TF, pAttribute=pAttribute)
})
