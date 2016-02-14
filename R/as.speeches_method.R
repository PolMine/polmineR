#' Split partition into speeches
#' 
#' A method designed for corpora from the
#' PolMine corpora of plenary protocols. A partition is split into speeches. 
#' 
#' @param .Object a partition .Object
#' @param sAttribute the s-attribute that provides the speaker names
#' @param gap number of tokens between strucs to identify speeches
#' @param addMeta whether to add metainformation to partition name, if wanted, provide s-attribute with date as character vector
#' @param mc whether to use multicore, defaults to FALSE
#' @param verbose logical, defaults to TRUE
#' @return a partitionCluster object
#' @name as.speeches
#' @aliases speeches speeches-method speeches,partition-method
#' @rdname as.speeches
#' @docType methods
#' @exportMethod as.speeches
setGeneric("as.speeches", function(.Object, ...)standardGeneric("as.speeches"))


#' @rdname as.speeches
setMethod("as.speeches", "partition", function(.Object, sAttributeDates, sAttributeNames,  gap=500, mc=FALSE, verbose=FALSE){
  partitionByDate <- lapply(
    sAttributes(.Object, sAttributeDates),
    function(x) partition(.Object, def=setNames(list(x), sAttributeDates), verbose=verbose)
  )
  speakerNestedList <- blapply(
    partitionByDate,
    function(datePartition, ...){
      nested <- lapply(
        sAttributes(datePartition, sAttributeNames),
        function(speakerName){
          beforeSplit <- partition(datePartition, setNames(list(speakerName), sAttributeNames), verbose=FALSE)
          split(beforeSplit, gap=gap, verbose=FALSE)
        }
      )
      unlist(lapply(c(1:length(nested)), function(i) nested[[i]]@objects))
    },
    # param=list("sAttributeNames", "gap"),
    sAttributeNames=sAttributeNames, gap=gap,
    mc=mc
  )
  speakerFlatList <- do.call(c, unlist(speakerNestedList, recursive=FALSE))
  partitionNames <- sapply(
    speakerFlatList,
    function(x) paste(x@sAttributes[[sAttributeNames]], sAttributes(x, sAttributeDates), x@name, sep="_")
  )
  for (i in c(1:length(speakerFlatList))) name(speakerFlatList[[i]]) <- partitionNames[i]
  as.bundle(speakerFlatList)
})