#' @include context_class.R
NULL

#' Generate a list of partitions
#' 
#' A list of partition objects with fixed s-attributes and one variable
#' s-attribute is generated
#' 
#' @param .Object character string, a partition, or a list
#' @param def a list that indicates the s-attribute to be variable and that
#' provides a character string of values (e.g. def=list(text_year=c("2005", "2006"))
#' @param prefix a character vector that will serve as a prefix for partition names
#' @param encoding encoding of the corpus (typically "LATIN1 or "(UTF-8)), if NULL, the encoding provided in the registry file of the corpus (charset="...") will be used b
#' @param pAttribute the pAttribute(s) for which term frequencies shall be retrieved
#' @param regex logical (defaults to FALSE), if TRUE, the s-attributes provided will be handeled as regular expressions; the length of the character vectors with s-attributes then needs to be 1
#' @param xml either 'flat' (default) or 'nested'
#' @param id2str whether to turn token ids to strings (set FALSE to minimize object.size / memory consumption)
#' @param type character vector (length 1) specifying the type of corpus / partition (e.g. "plpr")
#' @param progress logical, whether to show progress bar
#' @param ... shortcut to define partitions
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
setGeneric("partitionBundle", function(.Object, ...) standardGeneric("partitionBundle"))

#' @rdname partitionBundle-method
setMethod("partitionBundle", "partition", function(
  .Object, def=NULL, prefix=c(""),
  encoding=NULL, pAttribute=NULL, regex=FALSE, xml="flat", id2str=TRUE, type=NULL,
  mc=getOption("polmineR.mc"), verbose=TRUE, progress=FALSE,
  ...
) {
  if (length(list(...)) != 0 && is.null(def)) def <- list(...)
  bundle <- new(
    "partitionBundle",
    corpus=.Object@corpus,
    call=deparse(match.call()),
    sAttributesFixed=.Object@sAttributes,
    encoding=.Object@encoding
  )
  if (is.null(def[[names(def)]])){
    if (verbose==TRUE) message('... getting values of fixed s-attributes')
    def[[names(def)[1]]] <- sAttributes(.Object, names(def)[1])
    if (verbose==TRUE) message('... number of partitions to be initialized: ', length(def[[1]]))
  }
  if (mc==FALSE) {
    bundle@objects <- lapply(
      setNames(unname(def[[1]]), unname(def[[1]])),
      function(sAttribute){
        bundle@objects[[sAttribute]] <- partition(
          .Object,
          def=setNames(list(sAttribute), names(def)[1]),
          name=sAttribute,
          encoding=encoding, pAttribute=pAttribute, regex=regex, xml=xml, id2str=id2str, type=type
        )
      }
    )
  } else if (mc==TRUE) {
    if (verbose==TRUE) message('... setting up the partitions')
    bundle@objects <- mclapply(
      def[[1]],
      function(x) partition(
        .Object,
        def=setNames(list(x), names(def)[1]),
        name=x,
        encoding=encoding, pAttribute=pAttribute, regex=regex, xml=xml, id2str=id2str, type=type
      )
    )
  }
  names(bundle@objects) <- paste(.adjustEncoding(prefix, bundle@encoding), def[[1]], sep='')
  bundle
})


#' @rdname partitionBundle-method
setMethod("partitionBundle", "character", function(
  .Object, def, prefix=c(""),
  encoding=NULL, pAttribute=NULL, regex=FALSE, xml="flat", id2str=TRUE, type=NULL,
  mc=getOption("polmineR.mc"), verbose=TRUE, progress=FALSE,
  ...
) {
  if (length(list(...)) != 0 && is.null(def)) def <- list(...)
  bundle <- new(
    "partitionBundle",
    corpus=.Object,
    call=deparse(match.call())
  )
  bundle@encoding <- polmineR::parseRegistry(.Object)[["charset"]]
  stopifnot(length(def) == 1)
  sAttrVar <- paste(.Object, names(def)[1], sep=".")
  sAttrVarStrucs <- c(0:CQI$attribute_size(.Object, names(def)[1]) - 1)
  sAttrVarValues <- CQI$struc2str(.Object, names(def)[1], sAttrVarStrucs)
  if (!is.null(def[[1]])) {
    sAttrVarStrucs <- sAttrVarStrucs[which(sAttrVarValues %in% def[[1]])]
    sAttrVarValues <- CQI$struc2str(.Object, names(def)[1], sAttrVarStrucs)
  }
  cposMatrix <- do.call(rbind, lapply(sAttrVarStrucs, function(x) CQI$struc2cpos(.Object, names(def)[1], x)))
  cposList <- split(cposMatrix, f=sAttrVarValues)
  if (verbose == TRUE) message("... generating the partitions")
  if (progress == TRUE) pb <- txtProgressBar(max=length(cposList), style=3)
  bundle@objects <- lapply(
    setNames(c(1:length(cposList)), names(cposList)),
    function(i){
      if (progress == TRUE) setTxtProgressBar(pb, i)
      newBundle <- new(
        "partition",
        corpus=.Object,
        encoding=bundle@encoding,
        stat=data.table(),
        name=names(cposList)[i],
        cpos=matrix(cposList[[i]], ncol=2),
        sAttributeStrucs=names(def)[1]
      )
      newBundle@strucs <- CQI$cpos2struc(.Object, names(def)[1], newBundle@cpos[,1])
      newBundle
    })
  return(bundle)
})


setGeneric("as.partitionBundle", function(.Object, ...) standardGeneric("as.partitionBundle"))

#' @rdname partitionBundle-class
setMethod("as.partitionBundle", "list", function(.Object, ...){
  as(.Object, "bundle") # defined in bundle_class.R
})

#' @exportMethod as.partitionBundle
#' @rdname context-class
setMethod("as.partitionBundle", "context", function(.Object, mc=FALSE){
  newPartitionBundle <- new(
    "partitionBundle",
    corpus=.Object@corpus,
    encoding=.Object@encoding,
    explanation="this partitionBundle is derived from a context object"
  )
  .makeNewPartition <- function(cpos){
    newPartition <- new(
      "partition",
      corpus=.Object@corpus,
      encoding=.Object@encoding,
      cpos=matrix(c(cpos[["left"]][1], cpos[["right"]][length(cpos[["right"]])]), ncol=2)
    )
    newPartition <- enrich(newPartition, size=TRUE, pAttribute=.Object@pAttribute)
    newPartition@strucs <- c(
      CQI$cpos2struc(.Object@corpus, .Object@sAttribute, newPartition@cpos[1,1])
      :
        CQI$cpos2struc(.Object@corpus, .Object@sAttribute, newPartition@cpos[1,2])
    )
    newPartition
  }
  if (mc == FALSE){
    newPartitionBundle@objects <- lapply(.Object@cpos, FUN=.makeNewPartition)  
  } else {
    coresToUse <- getOption("polmineR.cores")
    newPartitionBundle@objects <- mclapply(.Object@cpos, FUN=.makeNewPartition, mc.cores=coresToUse)  
  }
  return(newPartitionBundle)
})