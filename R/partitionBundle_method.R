#' @include context_class.R
NULL

#' Generate a list of partitions
#' 
#' A list of partition objects with fixed s-attributes and one variable
#' s-attribute is generated
#' 
#' @param object character string, the CWB corpus to be used
#' @param def a list with the definition of a partition that shall be prepared
#' @param var a list that indicates the s-attribute to be variable and that
#' provides a character string of values (e.g. var=list(text_year=c("2005", "2006"))
#' @param prefix a character vector that will serve as a prefix for partition names
#' @param ... further parameters that will be pased into the partition-method called to generate the partitions in the bundle
#' @param mc logical, whether to use multicore parallelization
#' @param verbose logical, whether to provide progress information
#' @return a S4 class 'partitionBundle', which is a list with partition objects
#' @importFrom parallel mclapply
#' @export partitionBundle
#' @aliases partitionBundle partitionBundle,character-method
#' @author Andreas Blaette
#' @name partitionBundle
#' @docType methods
#' @rdname partitionBundle-method
#' @seealso \code{\link{partition}} and \code{\link{bundle-class}}
setGeneric("partitionBundle", function(object, ...) standardGeneric("partitionBundle"))

#' @rdname partitionBundle-method
setMethod("partitionBundle", "character", function(
  object, def=NULL, var, prefix=c(""), ..., mc=NULL, verbose=TRUE, progress=FALSE
) {
  if (length(names(var))==1) {
    sAttributeVar <- names(var)
    sAttributeVarValues <- var[[sAttributeVar]]
  } else {
    warning("only one variable s-attribute may be provided")
  }
  if (is.null(mc)) mc <- slot(get("session", '.GlobalEnv'), 'multicore')
  multicoreMessage <- ifelse(
    mc==TRUE,
    ' (use multicore: TRUE)',
    ' (use multicore: FALSE)'
  )
  if (verbose==TRUE) message('\nPreparing bundle of partitions', multicoreMessage)
  bundle <- new(
    "partitionBundle",
    corpus=object,
    call=deparse(match.call())
  )
  if (!is.null(def)) bundle@sAttributesFixed=def
  
  if (is.null(def)){
    bundle@encoding <- polmineR::parseRegistry(object)[["charset"]]
    stopifnot(length(var) == 1)
    sAttrVar <- paste(object, names(var)[1], sep=".")
    sAttrVarStrucs <- c(0:cqi_attribute_size(sAttrVar) - 1)
    sAttrVarValues <- cqi_struc2str(sAttrVar, sAttrVarStrucs)
    if (!is.null(var[[1]])) {
      sAttrVarStrucs <- sAttrVarStrucs[which(sAttrVarValues %in% var[[1]])]
      sAttrVarValues <- cqi_struc2str(sAttrVar, sAttrVarStrucs)
    }
    cposMatrix <- do.call(rbind, lapply(sAttrVarStrucs, function(x) cqi_struc2cpos(sAttrVar, x)))
    cposList <- split(cposMatrix, f=sAttrVarValues)
    if (verbose == TRUE) message("... generating the partitions")
    if (progress == TRUE) pb <- txtProgressBar(max=length(cposList), style=3)
    bundle@objects <- lapply(
      setNames(c(1:length(cposList)), names(cposList)),
      function(i){
        if (progress == TRUE) setTxtProgressBar(pb, i)
        new(
          "partition",
          corpus=object,
          encoding=bundle@encoding,
          stat=data.table(),
          name=names(cposList)[i],
          cpos=matrix(cposList[[i]], ncol=2)
          )
      })
    return(bundle)
  } else {
    if (verbose==TRUE) message('... setting up base partition')
    partitionBase <- partition(object, def, ..., verbose=FALSE)
    bundle@encoding <- partitionBase@encoding
    if (is.null(sAttributeVarValues)){
      if (verbose==TRUE) message('... getting values of fixed s-attributes')
      sAttributeVarValues <- unique(cqi_struc2str(paste(object, '.', sAttributeVar, sep=''), partitionBase@strucs))
      Encoding(sAttributeVarValues) <- bundle@encoding
      if (verbose==TRUE) message('... number of partitions to be initialized: ', length(sAttributeVarValues))
    }
    if (mc==FALSE) {
      for (sAttribute in sAttributeVarValues){
        sAttr <- list()
        sAttr[[sAttributeVar]] <- sAttribute
        bundle@objects[[sAttribute]] <- partition(partitionBase, def=sAttr, name=sAttribute, ...)
      }
    } else if (mc==TRUE) {
      if (verbose==TRUE) message('... setting up the partitions')
      bundle@objects <- mclapply(
        sAttributeVarValues,
        function(x) partition(
          partitionBase,
          def=sapply(sAttributeVar, function(y) x, USE.NAMES=TRUE),
          name=x, ...
        )
      )
    }
    names(bundle@objects) <- paste(.adjustEncoding(prefix, bundle@encoding), sAttributeVarValues, sep='')
  }
  bundle
})

#' @rdname partitionBundle-method
setMethod("partitionBundle", "list", function(object, var, prefix=c(""), ...) {
  partitionBundle(
    object=get('session', '.GlobalEnv')@corpus,
    def=object, var=var, prefix=prefix, ...
  )
})



setGeneric("as.partitionBundle", function(object, ...) standardGeneric("as.partitionBundle"))

#' @rdname partitionBundle-class
setMethod("as.partitionBundle", "list", function(object, ...){
  as(object, "bundle") # defined in bundle_class.R
})

#' @exportMethod as.partitionBundle
#' @rdname context-class
setMethod("as.partitionBundle", "context", function(object, mc=FALSE){
  newPartitionBundle <- new(
    "partitionBundle",
    corpus=object@corpus,
    encoding=object@encoding,
    explanation="this partitionBundle is derived from a context object"
  )
  .makeNewPartition <- function(cpos){
    newPartition <- new(
      "partition",
      corpus=object@corpus,
      encoding=object@encoding,
      cpos=matrix(c(cpos[["left"]][1], cpos[["right"]][length(cpos[["right"]])]), ncol=2)
    )
    newPartition <- enrich(newPartition, size=TRUE, pAttribute=object@pAttribute)
    newPartition@strucs <- c(
      cqi_cpos2struc(paste(object@corpus, ".", object@sAttribute, sep=""), newPartition@cpos[1,1])
      :
        cqi_cpos2struc(paste(object@corpus, ".", object@sAttribute, sep=""), newPartition@cpos[1,2])
    )
    newPartition
  }
  if (mc == FALSE){
    newPartitionBundle@objects <- lapply(object@cpos, FUN=.makeNewPartition)  
  } else {
    coresToUse <- slot(get("session", ".GlobalEnv"), "cores")
    newPartitionBundle@objects <- mclapply(object@cpos, FUN=.makeNewPartition, mc.cores=coresToUse)  
  }
  return(newPartitionBundle)
})


