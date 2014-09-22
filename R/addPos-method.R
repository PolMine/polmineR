#' @include partition-class.R partitionCluster-class.R keyness-class.R
NULL

#' add pos information
#' 
#' Add information on part-of-speech tags of tokens to tokens. The method is 
#' available for objects of the classes 'partition', 'partitionCluster' and
#' 'keyness' respectively. 
#' \code{method?addPos("partitionCluster")}, \code{method?addPos("keyness")}).
#' @param object either a partition, a partitionCluster or a keyness object
#' @param ... further arguments
#' @return the original, enhanced object
#' @docType methods
#' @rdname addPos-method
#' @name addPos
#' @aliases addPos addPos-method addPos,partition-method addPos,partitionCluster-method addPos,keyness-method
setGeneric("addPos", function(object,...){standardGeneric("addPos")})


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
  ids = cqi_str2id(paste(object@corpus, ".", object@pAttribute, sep=""), rownames(object@stat))
  posIds <- unlist(mclapply(ids, function (x){
    idPos <- cqi_cpos2id(paste(object@corpus, ".pos", sep=""), cqi_id2cpos(paste(object@corpus, ".", object@pAttribute, sep=""), x))
    posIdFrequencies <- tabulate(idPos+1)
    mostFrequent <- which.max(posIdFrequencies) - 1
    return(mostFrequent)
  }))
  pos <- cqi_id2str(paste(object@corpus, ".pos", sep=""), posIds)
  object@stat <- cbind(object@stat, pos=pos)
  object
}


#' Fill slot 'pos' of partition (or partitionCluster) object
#' 
#' The 'pos'-slot of the partition (or partitionCluster) object is filled with tables
#' providing a statistic on the frequency of a pos-tag of a token
#' in the partition.
#' 
#' @param object a partition class object
#' @param pAttribute character vector (typically c("word") or c("lemma") or c("word", "lemma"))
#' @return an augmented partition or partitionCluster object (includes pos now)
#' @author Andreas Blaette
#' @exportMethod addPos
#' @docType methods
#' @noRd
setMethod("addPos", "partition", function(object, pAttribute){
  if (length(pAttribute) > 1) warning("taking only one pAttribute at a time")
  message("Adding pos information to partition object ", object@label)
  cpos <- unlist(apply(object@cpos, 1, function(x) c(x[1]:x[2])))
  message("... retrieving corpus information")
  bag <- data.frame(
    token=cqi_cpos2id(paste(object@corpus, '.', pAttribute, sep=''), cpos),
    pos=cqi_cpos2id(paste(object@corpus, '.pos', sep=''), cpos)
  )
  message("... doing the calculations")
  object@pos[[pAttribute]] <- list()
  crosstab <- table(bag)
  rownames(crosstab) <- cqi_id2str(paste(object@corpus, '.', pAttribute, sep=''), as.integer(rownames(crosstab)))
  colnames(crosstab) <- cqi_id2str(paste(object@corpus, '.pos', sep=''), as.integer(colnames(crosstab)))
  object@pos[[pAttribute]] <- apply(crosstab, 1, function(x) colnames(crosstab)[which.max(x)])
  Encoding(names(object@pos[[pAttribute]])) <- object@encoding
  # to make sure that there are no superfluous pos information
  object@pos[[pAttribute]] <- object@pos[[pAttribute]][names(object@pos[[pAttribute]]) %in% rownames(object@tf[[pAttribute]])]
  object
})

#' Fill slot 'pos' of a partitionCluster object with tables giving the statistic of pos
#' 
#' Augment the partitionCluster object
#' 
#' @param object a partition class object
#' @param pAttribute character vector - pos statistic for lemma or word
#' @return an augmented partition object (includes pos now)
#' @author Andreas Blaette
#' @docType methods
#' @noRd
setMethod("addPos", "partitionCluster", function(object, pAttribute){
  pimpedCluster <- object
  if (get('drillingControls', '.GlobalEnv')[['multicore']] == TRUE) {
    pimpedCluster@partitions <- mclapply(object@partitions, function(x) addPos(x, pAttribute))
  } else {
    pimpedCluster@partitions <- lapply(object@partitions, function(x) addPos(x, pAttribute))    
  }
  pimpedCluster
})

#' supplement keyness object with pos information
#' 
#' A keyness object will be supplemented with pos information. The working of the
#' method is potentially slow. It is recommended to trim the object first, before
#' adding pos information.
#' 
#' @param object the keyness object
#' @param Partition a partition object (the corpus of interest)
#' @return an enhanced keyness object 
#' @docType methods
#' @noRd 
setMethod("addPos", "keyness",
          function(object, Partition=NULL){
            if (is.null(Partition)){
              object <- .addPos(object)
            } else if (class(Partition) == "partition"){
              if (object@pAttribute %in% names(Partition@pos)) {
                pos <- vapply(
                  rownames(object@stat[1:50, ]),
                  function(x) return(Partition@pos[[object@pAttribute]][["max"]][x]),
                  USE.NAMES=FALSE,
                  FUN.VALUE="character")
                object@stat <- cbind(object@stat, pos=pos)
              }
            }
            object 
          }
)

