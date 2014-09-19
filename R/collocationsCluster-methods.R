#' @include collocations-class.R collocationsCluster-class.R
NULL

setMethod("as.TermDocumentMatrix", "collocationsCluster", function(x, col){
  tabsWithKey <- lapply(
    x@collocations,
    function(i) {
      data.frame(
        key=paste(i@stat[,"node"], "->", i@stat[,"collocate"], sep=""),
        i@stat,
        stringsAsFactors = FALSE
        )
    })
  keys <- unique(unlist(lapply(tabsWithKey, function(x) x[,"key"])))
  keyVector <- setNames(c(1:length(keys)), keys)
  i <- unname(unlist(lapply(tabsWithKey, function(tab) keyVector[tab[,"key"]])))
  j <- unlist(lapply(c(1:length(tabsWithKey)), function(i) rep(i, nrow(tabsWithKey[[i]]))))
  v <- unlist(lapply(tabsWithKey, function(tab) tab[,col]))
  mat <- simple_triplet_matrix(
     i=i, j=j, v=v,
     ncol=length(tabsWithKey),
     nrow=length(keyVector),
     dimnames=list(
       Terms=names(keyVector),
       Docs=names(x@collocations)
    )
  )
  return(mat)
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=x@encoding, to="UTF-8")
  class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
})
