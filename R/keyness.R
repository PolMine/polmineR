#' @include methods.R
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
#' @include partition.R methods.R
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



#' Add POS tags
#' 
#' Add the POS tags to a table with tokens in the rows
#' 
#' The POS tags that occur for a given token are counted. The POS tag with the
#' highest share is added to the table
#' 
#' @param object a context object
#' @return object with pimped stat table
#' @author Andreas Blaette
#' @noRd
.addPos <- function(object) {
  ids = cqi_str2id(paste(object@corpus, ".", object@pattribute, sep=""), rownames(object@stat))
  posIds <- unlist(mclapply(ids, function (x){
    idPos <- cqi_cpos2id(paste(object@corpus, ".pos", sep=""), cqi_id2cpos(paste(object@corpus, ".", object@pattribute, sep=""), x))
    posIdFrequencies <- tabulate(idPos+1)
    mostFrequent <- which.max(posIdFrequencies) - 1
    return(mostFrequent)
  }))
  pos <- cqi_id2str(paste(object@corpus, ".pos", sep=""), posIds)
  object@stat <- cbind(object@stat, pos=pos)
  object
}
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

setMethod("as.matrix", signature(x="keynessCluster"), function(x, col="chi"){
  object <- x
  encoding <- unique(unlist(lapply(object@objects, function(o) o@encoding)))
  corpus <- unique(unlist(lapply(object@objects, function(o) o@corpus)))
  pAttribute <- unique(unlist(lapply(object@contexts, function(o) o@pattribute)))
  pAttr <- paste(corpus, '.', pAttribute, sep='')
  i <- unlist(lapply(object@objects, function(o) (cqi_str2id(pAttr, rownames(o@stat))+1)))
  j <- unlist(lapply(c(1:length(object@objects)), function(m) {rep(m,times=nrow(x[[m]]@stat))}))
  v <- unlist(lapply(object@objects, function(c) c@stat[,col]))
  lexiconSize <- cqi_lexicon_size(pAttr)
  mat <- simple_triplet_matrix(
    i=i, j=j, v=v,
    ncol=length(x@contexts),
    nrow=lexiconSize+1,
    dimnames=list(
      Terms=cqi_id2str(pAttr, c(0:lexiconSize)),
      Docs=names(object@objects)
      )
    )
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")
  class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  mat <- as.matrix(mat)
  mat
})

setMethod("[[", "keynessCluster", function(x, i){
  return(x@objects[[i]])
})

setMethod("summary", "keynessCluster", function(object){
  tab <- do.call(rbind, lapply(object@objects, function(x) summary(x)$no))
  colnames(tab) <- c("0.001", "0.005", "0.010", "0.050")
  tab
})


#' Summary of a keyness object
#' 
#' @exportMethod summary
#' @noRd
setMethod("summary", "keyness", function(object){.statisticalSummary(object)})


setMethod("show", "keyness", function(object){summary(object)})

