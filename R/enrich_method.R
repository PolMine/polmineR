#' @include partition_class.R partitionBundle_class.R context_class.R contextBundle_class.R
#' @include comp_class.R
NULL

#' enrich an object
#' 
#' Method to fill slots of a partition or partitionBundle object that 
#' have not been set up previously. See the respective documentation:
#' @param object a partition, partitionBundle or comp object
#' @param size logical
#' @param pAttribute character vector
#' @param meta character vector
#' @param verbose logical
#' @param mc logical or, if numeric, providing the number of cores
#' @param ... further parameters
#' @aliases enrich enrich-method
#' @docType methods
#' @rdname enrich-method
setGeneric("enrich", function(object, ...){standardGeneric("enrich")})


#' @param id2str logical
#' @param progress logical, whether to show progress bar
#' @exportMethod enrich
#' @docType methods
#' @rdname enrich-method
setMethod("enrich", "partition", function(object, size=FALSE, pAttribute=NULL, id2str=TRUE, meta=NULL, verbose=TRUE, mc=FALSE, ...){
  if (size == TRUE) object@size <- size(object)
  if (!is.null(pAttribute)) {
    stopifnot(is.character(pAttribute) == TRUE, length(pAttribute) <= 2, all(pAttribute %in% pAttributes(object)))
    if (verbose==TRUE) message('... computing term frequencies (for p-attribute ', pAttribute, ')')  
    object@stat <- count(.Object = object, pAttribute = pAttribute, id2str = id2str, mc = mc)
    object@pAttribute <- pAttribute
  }
  if (!is.null(meta)) {
    if (verbose==TRUE) message('... setting up metadata (table and list of values)')
    object@metadata <- meta(object, sAttributes=meta)
  }
  object
})

#' @exportMethod enrich
#' @docType methods
#' @rdname enrich-method
setMethod("enrich", "partitionBundle", function(object, mc=FALSE, progress=TRUE, verbose=FALSE, ...){
  blapply(x=object, f=enrich, mc=mc, progress=progress, verbose=verbose, ...)  
})


#' @rdname kwic-class
setMethod("enrich", "kwic", function(object, meta = NULL){
  if (length(meta) > 0){
    metainformation <- lapply(
      meta,
      function(metadat){
        strucs <- CQI$cpos2struc(object@corpus, metadat, unlist(lapply(object@cpos, function(x)x$node[1])))
        as.utf8(CQI$struc2str(object@corpus, metadat, strucs))
      }
    )
    metainformation <- data.frame(metainformation, stringsAsFactors = FALSE)
    colnames(metainformation) <- meta
    object@table <- data.frame(metainformation, object@table)
    object@metadata <- c(meta, object@metadata)
  }
  object
})
