setGeneric("as.speeches", function(.Object, ...)standardGeneric("as.speeches"))

#' Split partition into speeches
#' 
#' A method designed for corpora from the
#' PolMine corpora of plenary protocols. A partition is split into speeches. 
#' 
#' @param .Object a partition .Object
#' @param sAttributeDates the s-attribute that provides the dates of sessions
#' @param sAttributeNames the s-attribute that provides the names of speakers
#' @param gap number of tokens between strucs to identify speeches
#' @param mc whether to use multicore, defaults to FALSE
#' @param verbose logical, defaults to TRUE
#' @param progress logical
#' @return a partitionBundle object
#' @name as.speeches
#' @docType methods
#' @exportMethod as.speeches
#' @rdname as.speeches-method
#' @examples 
#' \dontrun{
#'   use("polmineR.sampleCorpus")
#'   bt <- partition("PLPRBTTXT", text_year = "2009")
#'   speeches <- as.speeches(bt, sAttributeDates = "text_date", sAttributeNames = "text_name")
#'   
#'   # step-by-step, not the fastest way
#'   speeches <- enrich(speeches, pAttribute = "word")
#'   tdm <- as.TermDocumentMatrix(speeches, col = "count")
#'   
#'   # fast option (counts performed when assembling the sparse matrix)
#'   # tdm <- as.TermDocumentMatrix(speeches, pAttribute = "word")
#'   # termsToDropList <- noise(tdm)
#'   # whatToDrop <- c("stopwords", "specialChars", "numbers", "minNchar")
#'   # termsToDrop <- unlist(lapply(whatToDrop, function(x) termsToDropList[[x]]))
#'   # tdm <- trim(tdm, termsToDrop = termsToDrop)
#' }
#' @aliases as.speeches as.speeches,partition-method
setMethod("as.speeches", "partition", function(.Object, sAttributeDates, sAttributeNames,  gap=500, mc=FALSE, verbose=TRUE, progress=TRUE){
  if (verbose) message("... getting dates")
  dates <- sAttributes(.Object, sAttributeDates)
  if (verbose) message("... generating partitions by date")
  toIterate <- lapply(dates, function(x) setNames(x, sAttributeDates))
  partitionByDate <- blapply(
    x = toIterate, .Object = .Object, f = partition, 
    verbose = verbose, progress = progress, mc = mc
  )
  partitionByDate <- lapply(partitionByDate, function(x) as(x, "plprPartition"))
  if (verbose) message("... generating speeches")
  .splitFunction <- function(datePartition, ...){
    nested <- lapply(
      sAttributes(datePartition, sAttributeNames),
      function(speakerName){
        beforeSplit <- partition(datePartition, setNames(list(speakerName), sAttributeNames), verbose=FALSE)
        split(beforeSplit, gap = gap, verbose=FALSE)
      }
    )
    unlist(lapply(c(1:length(nested)), function(i) nested[[i]]@objects))
  }
  speakerNestedList <- blapply(
    x = partitionByDate, f = .splitFunction,
    sAttributeNames = sAttributeNames, gap = gap,
    mc = mc, progress = progress
  )
  speakerFlatList <- do.call(c, unlist(speakerNestedList, recursive=FALSE))
  if (verbose) message("... generating names")
  partitionNames <- sapply(
    speakerFlatList,
    function(x) paste(x@sAttributes[[sAttributeNames]], sAttributes(x, sAttributeDates), x@name, sep="_")
  )
  for (i in c(1:length(speakerFlatList))) name(speakerFlatList[[i]]) <- partitionNames[i]
  
  # at this stage, the list may contain partitions of size 0, that need to be dropped
  toDrop <- which(sapply(speakerFlatList, function(x) size(x)) == 0)
  if (length(toDrop) > 0) for (i in rev(toDrop)) speakerFlatList[[i]] <- NULL
  
  as.bundle(speakerFlatList)
})