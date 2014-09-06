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
#' @param included TRUE if coi is part of ref, defaults to FALSE
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
  verbose=TRUE
) {
  if (verbose==TRUE) message ('Computing keyness')
  keyness <- new('keyness')
  keyness@encoding <- x@encoding
  keyness@encoding <- x@corpus
  if (verbose==TRUE) message("... combining frequency lists")
  c <- merge(x@tf[[pAttribute]], y@tf[[pAttribute]], by.x="id", by.y="id")
  if (verbose==TRUE) message("... computing chisquare tests")
  c <- .chisquare(c, included, minFrequency)
  statistic <- data.frame(
    row.names=cqi_id2str(paste(x@corpus,".", pAttribute, sep=""), c[,1]),
    rank=c(1:dim(c)[1]),
    countCoi=c[,2],
    countRef=c[,3],
    chiSquare=round(c[,4], digits=2),
    expCoi=round(c[,5], digits=2),
    expRef=round(c[,6], digits=2)
  )
  Encoding(rownames(statistic)) <- y@encoding
  keyness@statisticalTest <- "chiSquare"
  keyness@statisticalSummary <- .statisticalSummary(keyness)
  keyness@corpus <- x@corpus
  keyness@pattribute <- pAttribute
  keyness@stat <- statistic
  keyness <- trim(keyness)
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
        included=verbose,
        verbose=verbose
      ),
      simplify = TRUE,
      USE.NAMES = TRUE
    )
  kclust
})


