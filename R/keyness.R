#' @include generics.R
NULL

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

#' S4 class for comparing corpora
#' 
#' to keep results from a keyness analysis
#' 
#' @section Objects from the class:
#' keyness objects are returned by the function call \code{keyness}
#'   
#' @section Slots:
#' \describe{
#'   \item{\code{corpus}:}{Object of class \code{"character"} ~~ }
#'   \item{\code{pattribute}:}{Object of class \code{"character"} ~~ }
#'   \item{\code{encoding}:}{Object of class \code{"character"} ~~ } 
#'   \item{\code{corpus}:}{Object of class \code{"character"} ~~ } 
#'   \item{\code{stat}:}{Object of class \code{"data.frame"} ~~ } 
#'   \item{\code{statisticalTest}:}{Object of class \code{"character"} statisticalTest used }
#'   \item{\code{statisticalSummary}:}{Object of class \code{"data.frame"} statistical summary }
#'   }
#'  @section Methods:
#'   \describe{
#'    \item{summary}{\code{signature(object = "keyness")}: Display essential information }
#'    \item{addPos}{\code{signature(object = "keyness")}: add POS attribute to statistics table }
#'    }
#' @rdname keyness-class
#' @name keyness-class
#' @aliases keyness keyness-class summary,keyness-method show,keyness-method addPos,keyness-method as.matrix,keynessCluster-method keyness,partitionCluster-method
#' @docType class
#' @exportClass keyness
#' @author Andreas Blaette
setClass("keyness",
         representation(corpus="character",
                        pattribute="character",
                        encoding="character",
                        stat="data.frame",
                        statisticalTest="character",
                        statisticalSummary="data.frame"
         )
)

setClass("keynessCluster", representation(objects="list"))







#' Summary of a keyness object
#' @exportMethod summary
#' @noRd
setMethod(
  "summary", "keyness",
  function(object){
    cat("the statistics table has", nrow(object@stat), "rows\n")
    cat("pos attributest have been added: ")
    if ("pos" %in% colnames(object@stat)){
      cat("YES\n")
    } else {
      cat("NO\n")
    }
    cat("\n** Statistical summary: **\n")
    print(.statisticalSummary(object)) 
    cat("\n** Top ten: **\n")
    print(object@stat[1:10,])
  }
)

setMethod("show", "keyness", function(object){
  summary(object)
})


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
