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
#' @docType methods
#' @references Manning / Schuetze ...
#' @exportMethod keyness
setMethod("keyness", signature=c(x="partition"), function(
  x,
  y,
  pAttribute=drillingControls$pAttribute,
  minFrequency=0,
  included=FALSE,
  method="chiSquare",
  digits=2,
  verbose=TRUE
) {
  if (verbose==TRUE) message ('Computing keyness')
  keyness <- new(
    'keyness',
    encoding=x@encoding, included=included, minFrequency=minFrequency,
    corpus=x@corpus, pattribute=pAttribute,
    sizeCoi=x@size, sizeRef=ifelse(included==FALSE, y@size, y@size-x@size)
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
  keyness <- trim(keyness, digits=keyness@digits)
  keyness
})



#' @docType methods
#' @noRd
setMethod("keyness", signature=c(x="partitionCluster"), function(
  x, y, pAttribute=drillingControls$pAttribute,
  minFrequency=0, included=FALSE, verbose=TRUE
) {
  kclust <- new("keynessCluster")
  kclust@objects <- sapply(
      x@partitions,
      function(a) keyness(
        a,
        y,
        pAttribute=pAttribute,
        minFrequency=minFrequency,
        included=included,
        verbose=verbose
      ),
      simplify = TRUE,
      USE.NAMES = TRUE
    )
  kclust
})


