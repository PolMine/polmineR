#' @include partition_class.R partitionBundle_class.R context_class.R contextBundle_class.R
#' @include features_class.R
NULL

#' enrich an object
#' 
#' Method to fill slots of a partition or partitionBundle object that 
#' have not been set up previously. See the respective documentation:
#' @param object a partition, partitionBundle or comp object
#' @param size logical
#' @param pAttribute character vector
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
setMethod("enrich", "partition", function(object, size = FALSE, pAttribute = NULL, id2str = TRUE, verbose = TRUE, mc=FALSE, ...){
  if (size) object@size <- size(object)
  if (!is.null(pAttribute)) {
    stopifnot(is.character(pAttribute) == TRUE, length(pAttribute) <= 2, all(pAttribute %in% pAttributes(object)))
    .verboseOutput(
      message = paste('getting counts for p-attribute(s):', paste(pAttribute, collapse = ", "), sep = " "),
      verbose = verbose
      )  
    object@stat <- count(.Object = object, pAttribute = pAttribute, id2str = id2str, mc = mc, verbose = verbose)
    object@pAttribute <- pAttribute
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
        cposToGet <- object@cpos[which(object@cpos[["position"]] == 0)][, .SD[1], by = "hit_no", with = TRUE][["cpos"]]
        # cposToGet <- object@cpos[hit_no %in% object@table[["hit_no"]] ]  [position == 0][, .SD[1], by = hit_no][["cpos"]]
        strucs <- CQI$cpos2struc(object@corpus, metadat, cposToGet)
        as.nativeEnc(CQI$struc2str(object@corpus, metadat, strucs), from = object@encoding)
      }
    )
    metainformation <- data.frame(metainformation, stringsAsFactors = FALSE)
    colnames(metainformation) <- meta
    object@table <- data.frame(metainformation, object@table)
    object@metadata <- c(meta, object@metadata)
  }
  object
})
