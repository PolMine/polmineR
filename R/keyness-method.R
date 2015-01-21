#' @include partition-class.R partitionCluster-class.R
NULL

setGeneric("keyness", function(x, ...){standardGeneric("keyness")})


#' compute chi-square values for tokens in a corpus using a reference corpus
#' 
#' Pearson's chi-squared test is calculated to measure the keyness of a token
#' in a corpus of interest (COI). A reference corpus is required for the
#' computation
#' 
#' If pos.filter is supplied, the most propable pos-Tags will be added to the 
#' statistical table and the table will be filtered. This may slow down the 
#' procedure considerably.
#' 
#' 
#' @param x a partition or partitionCluster object
#' @param y a partition object, it is assumed that the coi is a subcorpus of
#' ref
#' @param pAttribute The P-Attribute that will be counted (usually either
#' 'word' or 'lemma')
#' @param minFrequency the minimum frequency of collocates
#' @param method the statistical test to apply (chisquare or log likelihood)
#' @param included TRUE if coi is part of ref, defaults to FALSE
#' @param digits numeric
#' @param verbose defaults to TRUE
#' @return The function returns a data frame with the following structure:
#' - absolute frequencies in the first row
#' - ...
#' @author Andreas Blaette
#' @aliases keyness,collocations-method
#' @docType methods
#' @references Manning / Schuetze ...
#' @exportMethod keyness
#' @rdname  keyness
setMethod("keyness", signature=c(x="partition"), function(
  x,
  y,
  pAttribute=NULL,
  minFrequency=0,
  included=FALSE,
  method="chiSquare",
  digits=2,
  verbose=TRUE
) {
  if (verbose==TRUE) message ('Computing keyness')
  if (is.null(pAttribute)) pAttribute <- slot(get("session", ".GlobalEnv"), "pAttribute")
  keyness <- new(
    'keyness',
    encoding=x@encoding, included=included, minFrequency=minFrequency,
    corpus=x@corpus, pAttribute=pAttribute,
    sizeCoi=x@size, sizeRef=ifelse(included==FALSE, y@size, y@size-x@size),
    call=deparse(match.call())
    )
  keyness@digits <- as.list(setNames(rep(2, times=2+length(method)), c("expCoi", "expRef", method)))
  # check whether tf-lists are available for the pAttribute given
  if (!pAttribute %in% names(x@tf)){
    message("... term frequencies not available for pAttribute given in corpus of interest - enriching the partition ")
    x <- enrich(x, tf=pAttribute, verbose=FALSE)
  }
  if (!pAttribute %in% names(y@tf)){
    message("... term frequencies not available for pAttribute given in reference corpus - enriching the partition ")
    y <- enrich(y, tf=pAttribute, verbose=FALSE)
  }
  if (verbose==TRUE) message("... combining frequency lists")
  keyness@stat <- merge(x@tf[[pAttribute]], y@tf[[pAttribute]], by.x="id", by.y="id")
  rownames(keyness@stat) <- cqi_id2str(
    paste(x@corpus,".", pAttribute, sep=""),
    keyness@stat[,"id"]
    )
  Encoding(rownames(keyness@stat)) <- y@encoding
  colnames(keyness@stat) <- c("id", "countCoi", "countRef")
  if (included == TRUE) keyness@stat[,"countRef"] <- keyness@stat[,"countRef"] - keyness@stat[,"countCoi"]
  if ("chiSquare" %in% method) {
    if (verbose==TRUE) message("... computing chisquare tests")
    keyness <- chisquare(keyness)
  }
  if ("ll" %in% method) {
    if (verbose==TRUE) message("... computing log likelihood tests")
    keyness <- ll(keyness)
  }
  keyness@stat <- cbind(rank=c(1:nrow(keyness@stat)), keyness@stat)
  keyness@stat <- keyness@stat[,-which(colnames(keyness@stat)=="id")]
  if (verbose==TRUE) message("... trimming table with statistical tests")
  keyness <- trim(keyness, digits=keyness@digits, verbose=verbose)
  keyness
})



#' @docType methods
#' @noRd
setMethod("keyness", signature=c(x="partitionCluster"), function(
  x, y, pAttribute=NULL,
  minFrequency=1, included=FALSE, method="chiSquare", verbose=TRUE, mc=TRUE
) {
  if (is.null(pAttribute)) pAttribute <- slot(get("session", ".GlobalEnv"), "pAttribute")
  kclust <- new("keynessCluster")
  .keyness <- function(a) {
    keyness(a, y, pAttribute=pAttribute, minFrequency=minFrequency, included=included, method=method, verbose=verbose)
  }
  if (mc == FALSE){
    kclust@objects <- lapply(setNames(x@partitions, names(x@partitions)), function(a) .keyness(a))
  } else if (mc == TRUE){
    kclust@objects <- mclapply(setNames(x@partitions, names(x@partitions)), function(a) .keyness(a))
  }
  kclust
})



setMethod("keyness", "collocations", function(
  x,y, minFrequency=0, included=FALSE, method="ll", digits=2, mc=TRUE, verbose=TRUE
  ){
  newObject <- new(
    'keynessCollocations',
    encoding=x@encoding, included=included, minFrequency=minFrequency,
    corpus=x@corpus, sizeCoi=x@partitionSize,
    sizeRef=ifelse(included==FALSE, y@partitionSize, y@partitionSize-x@partitionSize)
  )
  newObject@digits <- as.list(setNames(rep(2, times=2+length(method)), c("expCoi", "expRef", method)))
  if (x@pAttribute != y@pAttribute) {
    warning("BEWARE: collocations objects are not based on the same pAttribute!")
  } else {
    newObject@pAttribute <- unique(c(x@pAttribute, y@pAttribute))
  }
  tabs <- list(x=x@stat, y=y@stat)
  if (verbose == TRUE) message("... preparing tabs for matching")
  pimpTabs <- function(xOrY){
    tab <- data.frame(what=ifelse(xOrY == "x", 1, 2), tabs[[xOrY]])
    tabMatrix <- as.matrix(tab[,-which(colnames(tab) %in% c("collocate", "node"))])
    tabMatrixPlus <- t(apply(tabMatrix, 1, .minMaxId))
    colnames(tabMatrixPlus) <- c(colnames(tabMatrix), c("idMin", "idMax"))
    tabMatrixPlus
  }
  if (mc==FALSE) {
    tabsPlus <- lapply(names(tabs), pimpTabs)
  } else if (mc == TRUE) {
    tabsPlus <- mclapply(names(tabs), pimpTabs)
  }
  tab <- do.call(rbind, tabsPlus)
  rownames(tab) <- NULL
  characterKey <- paste(
      cqi_id2str(paste(newObject@corpus, '.', newObject@pAttribute, sep=""), tab[,"idMin"]),
      "<->",
      cqi_id2str(paste(newObject@corpus, '.', newObject@pAttribute, sep=""), tab[,"idMax"]),
      sep=""
    )
  Encoding(characterKey) <- newObject@encoding
  tab <- data.frame(tab, characterKey=characterKey, stringsAsFactors=FALSE)
  keysInX <- unique(subset(tab, what==1)[,"characterKey"])
  reducedTab <- subset(tab, characterKey %in% keysInX)
  if (verbose == TRUE) message("... matching")
  newObject@stat <- ddply(
    .data=reducedTab,
    .variables=.(characterKey),
    .fun=function(tab){
      xValues <- tab[which(tab[,"what"] == 1), "collocateWindowFreq"]
      yValues <- tab[which(tab[,"what"] == 2), "collocateWindowFreq"]
      keySplit <- unlist(strsplit(tab[, "characterKey"], " <-> "))
      foo1 <- min(c(tab[,"nodeId"], tab[,"collocateId"]))
      foo2 <- max(c(tab[,"nodeId"], tab[,"collocateId"]))
      data.frame(nodeId=foo1, collocateId=foo2, term1=keySplit[1], term2=keySplit[2], countCoi=xValues[1], countRef=yValues[1])
    },
    .progress="text"
    )
  ### starting here - the same as keyness,partition-method
  if (included == TRUE) newObject@stat[,"countRef"] <- newObject@stat[,"countRef"] - newObject@stat[,"countCoi"]
  if ("chiSquare" %in% method) {
    if (verbose==TRUE) message("... computing chisquare tests")
    newObject <- chisquare(newObject)
  }
  if ("ll" %in% method) {
    if (verbose==TRUE) message("... computing log likelihood tests")
    newObject <- ll(newObject)
  }
  if (verbose == TRUE) message("... trimming the object")
  newObject@stat <- cbind(rank=c(1:nrow(newObject@stat)), newObject@stat)
  rownames(newObject@stat) <- newObject@stat[,"characterKey"]
  newObject <- trim(newObject, minFrequency=minFrequency, rankBy=method[1])
  newObject
})

#' @rdname keyness
setMethod("keyness", "missing", function(){
  .getClassObjectsAvailable(".GlobalEnv", "keyness")
})




