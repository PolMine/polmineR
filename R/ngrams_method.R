#' @exportClass ngrams
#' @rdname ngrams
setClass(
  "ngrams",
  representation(n="integer", size="integer"),
  contains=c("textstat")
)

#' get ngrams
#' 
#' @param .Object object of class \code{partition}
#' @param n number of tokens
#' @param pAttribute p-attribute
#' @param char if NULL, tokens will be counted, else characters, keeping only those provided by a character vector
#' @param mc logical, whether to use multicore
#' @param progress logical
#' @param ... further parameters
#' @exportMethod ngrams
#' @rdname ngrams
setGeneric("ngrams", function(.Object, ...) standardGeneric("ngrams"))


#' @rdname ngrams
setMethod("ngrams", "partition", function(.Object, n=2, pAttribute="word", char=NULL, progress=FALSE){
  if (is.null(char)){
    cpos <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
    # pAttrs <- sapply(pAttribute, function(x) paste(.Object@corpus, ".", x, sep=""))
    idListBase <- lapply(pAttribute, function(pAttr) CQI$cpos2id(.Object@corpus, pAttribute, cpos))
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
    setnames(TF, "V1", "count")
    pAttrsCols <- rep(pAttribute, times=n)
    tokenNo <- unlist(lapply(c(1:n), function(x) rep(x, times=length(pAttribute))))
    # convert ids to strings
    dummy <- lapply(
      c(1:(n*length(pAttribute))),
      function(i){
        str <- as.utf8(CQI$id2str(strsplit(pAttrsCols[i], "\\.")[[1]][1], strsplit(pAttrsCols[i], "\\.")[[1]][1], TF[[i]]), from="latin1")
        TF[, eval(paste("token_", tokenNo[i], "_", names(pAttrsCols)[i], sep="")) := str , with=TRUE] 
      })
    # remove columns with ids
    lapply(
      grep("id_", colnames(TF), value=TRUE),
      function(x) TF[, eval(x) := NULL, with=TRUE]
    )
  } else {
    charSoupBase <- getTokenStream(.Object, pAttribute=pAttribute[1], collapse="")
    charSoup <- unlist(strsplit(charSoupBase, ""))
    if (char[1] != ""){
      charSoup <- unname(unlist(sapply(charSoup, function(x) ifelse(x %in% char, x, NA))))
      if (any(is.na(charSoup))) charSoup[-which(is.na(charSoup))]
    }
    charSoup <- paste(charSoup[which(!is.na(charSoup))], sep="", collapse="")
    charSoupTotal <- nchar(charSoup)
    ngrams <- sapply(
      c(1:(charSoupTotal-n+1)),
      function(x) {
        if (progress == TRUE) .progressBar(x, charSoupTotal)
        substr(charSoup, x, x+n-1)
        })
    tabledNgrams <- table(ngrams)
    TF <- data.table(
      ngram=names(tabledNgrams),
      count=unname(as.vector(tabledNgrams))
      )
  }
  new(
    "ngrams",
    n=as.integer(n), corpus=.Object@corpus, encoding=.Object@encoding,
    size=as.integer(length(.Object)), stat=TF, name=.Object@name,
    pAttribute=ifelse(is.null(char), pAttribute, "ngram")
    )
})

#' @rdname ngrams
setMethod("ngrams", "partitionBundle", function(.Object, char=NULL, mc=FALSE, progress=FALSE, ...){
  newBundle <- new("bundle")
  newBundle@objects <- lapply(
    c(1:length(.Object)),
    function(i){
      if (progress == TRUE) .progressBar(i, length(.Object))
      ngrams(.Object@objects[[i]], char=char, ...)
    }
  )
  newBundle@pAttribute <- unique(unlist(lapply(newBundle@objects, function(x) x@pAttribute)))
  names(newBundle@objects) <- names(.Object)
  newBundle
})