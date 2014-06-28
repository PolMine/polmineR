#' @include generics.R partition.R keyness.R
NULL

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


#' enrich partition object (S4 method)
#' 
#' Fill slots of a partition object: Add object size, metainformation, pos.
#' 
#' @param object a partition object
#' @param size logical, defaults to FALSE
#' @param tf character vector providing the p-attributes for which frequency
#'   tables shall be generated
#' @param meta character vector providing s-attributes for which metainformation shall be supplied
#' @param addPos character vector providing p-attributes 
#' @param verbose logical, defaults to TRUE   
#' @exportMethod enrich
#' @aliases enrich,partition-method enrich,partitionCluster-method
#' @docType methods
#' @rdname enrich-partition-method
setMethod("enrich", "partition", function(object, size=FALSE, tf=NULL, meta=NULL, addPos=NULL, verbose=TRUE){
  if (size == TRUE) object@size <- .partition.size(object)
  if (length(tf>0)) {
    for (what in tf){
      if (verbose==TRUE) message('... computing term frequencies (for p-attribute ', what, ')')  
      object@tf[[what]] <- .cpos2tf(object, what)
    }
  }
  if (!is.null(meta)) {
    if (verbose==TRUE) message('... setting up metadata (table and list of values)')
    object <- .partition.metadata(object, meta)
  }
  if(!is.null(addPos)){
    object <- addPos(object, addPos)
  }
  object
})

# documented with enrich,partition-method
setMethod("enrich", "partitionCluster", function(object, size=TRUE, tf=c(), meta=NULL, addPos=NULL, verbose=TRUE){
  object@partitions <- lapply(
    object@partitions,
    function(p) enrich(p, size=size, tf=tf, meta=meta, addPos, verbose=TRUE)
  )
  object
})


#' Enrich keyness object
#' 
#' Wrapper for adding a statistic on pos attributes to keyness object
#' 
#' @param object a keyness object
#' @param addPos no idea ....
#' @param verbose whether R should be talkative
#' @exportMethod enrich
#' @docType methods
#' @rdname enrich-keyness-method
#' @aliases enrich,keyness-method
setMethod("enrich", "keyness", function(object, addPos=NULL, verbose=TRUE){
  object <- addPos(object, Partition=addPos)
  object
})
