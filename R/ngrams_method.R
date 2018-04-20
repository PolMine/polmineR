#' @exportClass ngrams
#' @rdname ngrams
setClass(
  "ngrams",
  representation(n = "integer"),
  contains = c("count", "textstat")
)

#' Get N-Grams
#' 
#' Count n-grams, either of words, or of characters.
#' 
#' @param .Object object of class \code{partition}
#' @param n number of tokens/characters
#' @param pAttribute the p-attribute to use (can be > 1)
#' @param char if NULL, tokens will be counted, else characters, keeping only those provided by a character vector
#' @param mc logical, whether to use multicore, passed into call to \code{blapply} (see respective documentation)
#' @param progress logical
#' @param ... further parameters
#' @exportMethod ngrams
#' @rdname ngrams
#' @examples 
#'   use("polmineR")
#'   P <- partition("GERMAPARLMINI", date = "2009-10-27")
#'   ngramObject <- ngrams(P, n = 2, pAttribute = "word", char = NULL)
#'   # a more complex scenario: get most frequent ADJA/NN-combinations
#'   ngramObject <- ngrams(P, n = 2, pAttribute = c("word", "pos"), char = NULL)
#'   ngramObject2 <- subset(
#'     ngramObject,
#'     ngramObject[["1_pos"]] == "ADJA"  & ngramObject[["2_pos"]] == "NN"
#'     )
#'   ngramObject2@@stat[, "1_pos" := NULL, with = FALSE][, "2_pos" := NULL, with = FALSE]
#'   ngramObject3 <- sort(ngramObject2, by = "count")
#'   head(ngramObject3)
setGeneric("ngrams", function(.Object, ...) standardGeneric("ngrams"))


#' @rdname ngrams
setMethod("ngrams", "partition", function(.Object, n = 2, pAttribute = "word", char = NULL, progress = FALSE, ...){
  if (is.null(char)){
    
    cpos <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
    
    idListBase <- lapply(
      setNames(pAttribute, pAttribute),
      function(pAttr) CQI$cpos2id(.Object@corpus, pAttr, cpos)
      )
    
    idList <- list()
    j <- 0
    for (i in 1:n){
      for (pAttr in pAttribute){
        j <- j + 1
        idList[[j]] <- idListBase[[pAttr]][i:(length(idListBase[[pAttr]]) - (n - i))]
        names(idList)[j] <- paste("id", i, pAttr, sep = "_")
      }
    }
    
    DT <- as.data.table(idList)
    TF <- DT[, .N, by = c(eval(colnames(DT))), with = TRUE]
    setnames(TF, "N", "count")
    pAttrsCols <- rep(pAttribute, times = n)
    tokenNo <- unlist(lapply(1:n, function(x) rep(x, times = length(pAttribute))))
    
    # convert ids to strings
    dummy <- lapply(
      1:(n * length(pAttribute)),
      function(i){
        str <- as.nativeEnc(CQI$id2str(.Object@corpus, pAttrsCols[i], TF[[i]]), from = .Object@encoding)
        TF[, eval(paste(tokenNo[i], pAttrsCols[i], sep = "_")) := str , with = TRUE] 
      })
    
    # remove columns with ids
    lapply(
      grep("id_", colnames(TF), value = TRUE),
      function(x) TF[, eval(x) := NULL, with = TRUE]
    )
    setcolorder(TF, neworder = c(colnames(TF)[!colnames(TF) %in% "count"], "count"))
  } else {
    charSoupBase <- getTokenStream(.Object, pAttribute = pAttribute[1], collapse = "")
    charSoup <- unlist(strsplit(charSoupBase, ""))
    if (char[1] != ""){
      charSoup <- unname(unlist(sapply(charSoup, function(x) ifelse(x %in% char, x, NA))))
      if (any(is.na(charSoup))) charSoup[-which(is.na(charSoup))]
    }
    charSoup <- paste(charSoup[which(!is.na(charSoup))], sep = "", collapse = "")
    charSoupTotal <- nchar(charSoup)
    ngrams <- sapply(
      1:(charSoupTotal - n + 1),
      function(x) {
        if (progress) .progressBar(x, charSoupTotal)
        substr(charSoup, x, x + n - 1)
        })
    tabledNgrams <- table(ngrams)
    TF <- data.table(
      ngram = names(tabledNgrams),
      count = unname(as.vector(tabledNgrams))
      )
  }
  new(
    "ngrams",
    n = as.integer(n), corpus = .Object@corpus, encoding = .Object@encoding,
    size = as.integer(size(.Object)), stat = TF, name = .Object@name,
    pAttribute = if (is.null(char)) pAttribute else "ngram"
    )
})

#' @rdname ngrams
setMethod("ngrams", "partitionBundle", function(.Object, n = 2, char = NULL, pAttribute = "word", mc = FALSE, progress = FALSE, ...){
  newBundle <- new("bundle")
  newBundle@objects <- blapply(
    .Object@objects, f = ngrams,
    n = n, pAttribute = pAttribute, char = char, mc = mc, progress = progress
    )
#   newBundle@objects <- lapply(
#     c(1:length(.Object)),
#     function(i){
#       if (progress == TRUE) .progressBar(i, length(.Object))
#       ngrams(.Object@objects[[i]], char=char, ...)
#     }
#   )
  newBundle@pAttribute <- unique(unlist(lapply(newBundle@objects, function(x) x@pAttribute)))
  names(newBundle@objects) <- names(.Object)
  newBundle
})