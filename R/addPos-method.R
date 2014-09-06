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
setGeneric("addPos", function(object,...){standardGeneric("addPos")})



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
#' @noRd 
setMethod("addPos", "keyness",
          function(object, Partition=NULL){
            if (is.null(Partition)){
              object <- .addPos(object)
            } else if (class(Partition) == "partition"){
              if (object@pattribute %in% names(Partition@pos)) {
                pos <- vapply(
                  rownames(object@stat[1:50, ]),
                  function(x) return(Partition@pos[[object@pattribute]][["max"]][x]),
                  USE.NAMES=FALSE,
                  FUN.VALUE="character")
                object@stat <- cbind(object@stat, pos=pos)
              }
            }
            object 
          }
)

