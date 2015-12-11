#' @include partition_class.R partitionBundle_class.R context_class.R contextBundle_class.R
#' @include keyness_class.R keynessBundle_class.R
NULL

#' enrich an object
#' 
#' Method to fill slots of a partition, partitionBundle or keyness object that 
#' have not been set up previously. See the respective documentation:
#' \describe{
#'  \item{partition:}{\code{method?enrich("partition")}}
#'  \item{partitionBundle:}{\code{method?enrich("partitionBundle")}}
#'  \item{keyness:}{\code{method?enrich("keyness")}}
#' }
#' 
#' @param object a partition, partitionBundle or keyness object
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
#' @param pAttribute character vector providing the p-attributes for which frequency
#'   tables shall be generated
#' @param meta character vector providing s-attributes for which metainformation shall be supplied
#' @param addPos character vector providing p-attributes 
#' @param verbose logical, defaults to TRUE   
#' @exportMethod enrich
#' @aliases enrich,partition-method
#' @docType methods
#' @rdname enrich-partition-method
setMethod("enrich", "partition", function(object, size=FALSE, pAttribute=NULL, id2str=TRUE, meta=NULL, verbose=TRUE, mc=FALSE){
  if (size == TRUE) object@size <- size(object)
  if (!is.null(pAttribute)) {
      if (verbose==TRUE) message('... computing term frequencies (for p-attribute ', pAttribute, ')')  
      object@stat <- getTermFrequencies(.Object=object, pAttribute=pAttribute, id2str=id2str, mc=mc)
      object@pAttribute <- pAttribute
  }
  if (!is.null(meta)) {
    if (verbose==TRUE) message('... setting up metadata (table and list of values)')
    object <- .partition.metadata(object, meta)
  }
  object
})

#' enrich partition object (S4 method)
#' 
#' Fill slots of a partition object: Add object size, metainformation, pos.
#' 
#' @param object a partition object
#' @param size logical, defaults to FALSE
#' @param pAttribute character vector providing the p-attributes for which frequency
#'   tables shall be generated
#' @param meta character vector providing s-attributes for which metainformation shall be supplied
#' @param addPos character vector providing p-attributes
#' @param mc logical whether to use multicore parallelization
#' @param verbose logical, defaults to TRUE   
#' @exportMethod enrich
#' @aliases enrich,partitionBundle-method
#' @docType methods
#' @rdname enrich-partitionBundle-method
setMethod("enrich", "partitionBundle", function(object, size=TRUE, pAttribute=c(), meta=NULL, addPos=NULL, mc=FALSE, verbose=TRUE){
  if (mc == FALSE) {
    object@objects <- lapply(
      object@objects,
      function(p) enrich(p, size=size, pAttribute=pAttribute, meta=meta, addPos, verbose=TRUE)
    )
  } else if (mc == TRUE){
    object@objects <- mclapply(
      object@objects,
      function(p) enrich(p, size=size, pAttribute=pAttribute, meta=meta, addPos, verbose=TRUE)
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

#' @docType methods
#' @noRd
setMethod("enrich", "keynessBundle", function(object, addPos=NULL, verbose=TRUE, mc=NULL){
  if (is.null(mc)) mc <- slot(get("session", '.GlobalEnv'), 'multicore')
  rework <- new("keynessBundle")
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

