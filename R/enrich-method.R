#' enrich an object
#' 
#' Method to fill slots of a partition, partitionCluster or keyness object that 
#' have not been set up previously. See the respective documentation:
#' \describe{
#'  \item{partition:}{\code{method?enrich("partition")}}
#'  \item{partitionCluster:}{\code{method?enrich("partitionCluster")}}
#'  \item{keyness:}{\code{method?enrich("keyness")}}
#' }
#' 
#' @param object a partition, partitionCluster or keyness object
#' @param ... further parameters
#' @aliases enrich enrich-method
#' @docType methods
#' @rdname enrich-method
setGeneric("enrich", function(object, ...){standardGeneric("enrich")})




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
#' @aliases enrich,partition-method
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
#' @param mc logical whether to use multicore parallelization
#' @param verbose logical, defaults to TRUE   
#' @exportMethod enrich
#' @aliases enrich,partitionCluster-method
#' @docType methods
#' @rdname enrich-partitionCluster-method
setMethod("enrich", "partitionCluster", function(object, size=TRUE, tf=c(), meta=NULL, addPos=NULL, mc=FALSE, verbose=TRUE){
  if (mc == FALSE) {
    object@partitions <- lapply(
      object@partitions,
      function(p) enrich(p, size=size, tf=tf, meta=meta, addPos, verbose=TRUE)
    )
  } else if (mc == TRUE){
    object@partitions <- mclapply(
      object@partitions,
      function(p) enrich(p, size=size, tf=tf, meta=meta, addPos, verbose=TRUE)
    )    
  }
  object
})


#' Enrich keyness object
#' 
#' Wrapper for adding a statistic on pos attributes to keyness object
#' 
#' @param object a keyness object
#' @param addPos logical, whether to add POS information to keyness object
#' @param verbose whether R should be talkative
#' @exportMethod enrich
#' @docType methods
#' @rdname enrich-keyness-method
#' @aliases enrich,keyness-method
setMethod("enrich", "keyness", function(object, addPos=NULL, verbose=TRUE){
  if (addPos==TRUE) object <- addPos(object, Partition=NULL)
  object
})

setMethod("enrich", "keynessCluster", function(object, addPos=NULL, verbose=TRUE, mc=NULL){
  if (is.null(mc)) mc <- get("drillingControls", '.GlobalEnv')[['multicore']]
  rework <- new("keynessCluster")
  if (mc==FALSE){
    rework@objects <- lapply(
      setNames(object@objects, names(object@objects)),
      function(x) enrich(x, addPos=addPos, verbose=TRUE)
    )
  } else if (mc==TRUE){
    rework@objects <- mclapply(
      setNames(object@objects, names(object@objects)),
      function(x) enrich(x, addPos=addPos, verbose=TRUE)
    )    
  }
  rework
})

