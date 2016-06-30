#' @include cooccurrences_class.R
NULL

#' @rdname cooccurrencesBundle-class
setMethod("as.TermDocumentMatrix", "cooccurrencesBundle", function(x, col, directed=TRUE, rel=FALSE, mc=getOption("polmineR.mc")){
  tabs <- lapply(x@objects, as.data.frame)
  if (directed == TRUE){
    keys <- unique(unlist(lapply(tabs, rownames)))
    keyVector <- setNames(c(1:length(keys)), keys)
    i <- unname(unlist(lapply(tabs, function(tab) keyVector[rownames(tab)])))
    j <- unlist(lapply(c(1:length(tabs)), function(i) rep(i, nrow(tabs[[i]]))))
    v <- unlist(lapply(tabs, function(tab) tab[,col]))
  } else if (directed == FALSE){
    .uniqueKeys4tab <- function(tab){
      tabMatrix <- as.matrix(tab[,c("nodeId", "cooccurrenceId", col)])
      tabMatrixPlus <- t(apply(tabMatrix, 1, .minMaxId))
      colnames(tabMatrixPlus) <- c(colnames(tabMatrix), c("idMin", "idMax"))
      tabDataFrame <- data.frame(
        tabMatrixPlus,
        characterKey=paste(
          CQI$id2str(x@corpus, x@pAttribute, tabMatrixPlus[,"idMin"]), "<->",
          CQI$id2str(x@corpus, x@pAttribute, tabMatrixPlus[,"idMax"]), sep=""
        ),
        stringsAsFactors=FALSE
      )
      Encoding(tabDataFrame[,"characterKey"]) <- x@encoding
      tabDataFrame
    }
    tabs <- blapply(tabs, f=.uniqueKeys4tab, mc=mc)
    keys <- unique(unlist(lapply(tabs, function(tab) tab[, "characterKey"])))
    keyVector <- setNames(c(1:length(keys)), keys)
    .reduceTab <- function(i, tab, keyVector) {
      tab <- data.frame(tabs[[i]], no=i, key=keyVector[tabs[[i]][, "characterKey"]])
      tab <- as.matrix(tab[,c("no", col, "key")])
      tabSplit <- split(tab, tab[,"key"])
      tabSplitReduced <- lapply(tabSplit, function(foo) {
        noRow <- length(foo)/3
        return(c(foo[1], foo[noRow+1], foo[2*noRow+1]))
      })
      tabReduced <- do.call(rbind, tabSplitReduced)
      colnames(tabReduced) <- c("no", col, "key")
      return(tabReduced)
    }
    tabsReduced <- blapply(as.list(c(1:length(tabs))), f=.reduceTab, tab=tab, keyVector=keyVector)
    tab <- do.call(rbind, tabsReduced)
    i <- tab[,"key"]
    j <- tab[,"no"]
    v <- tab[,col]
  }
  mat <- simple_triplet_matrix(
    i=i, j=j, v=v,
    ncol=length(tabs),
    nrow=length(keyVector),
    dimnames=list(
      Terms=names(keyVector),
      Docs=names(x@objects)
    )
  ) 
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=x@encoding, to="UTF-8")
  class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  return(mat)
})
