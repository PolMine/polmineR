#' Split Partition Into Speeches
#' 
#' A partition is split into speeches. 
#' 
#' @param .Object a partition .Object
#' @param s_attribute_date the s-attribute that provides the dates of sessions
#' @param s_attribute_name the s-attribute that provides the names of speakers
#' @param gap number of tokens between strucs to identify speeches
#' @param mc whether to use multicore, defaults to FALSE
#' @param verbose logical, defaults to TRUE
#' @param progress logical
#' @return a partitionBundle object
#' @name as.speeches
#' @export as.speeches
#' @rdname as.speeches
#' @examples 
#' use("polmineR")
#' bt <- partition("GERMAPARLMINI", date = ".*", regex = TRUE)
#' speeches <- as.speeches(bt, s_attribute_date = "date", s_attribute_name = "speaker")
#'   
#' # step-by-step, not the fastest way
#' speeches <- enrich(speeches, pAttribute = "word")
#' tdm <- as.TermDocumentMatrix(speeches, col = "count")
#'   
#' # fast option (counts performed when assembling the sparse matrix)
#' # tdm <- as.TermDocumentMatrix(speeches, pAttribute = "word")
#' # termsToDropList <- noise(tdm)
#' # whatToDrop <- c("stopwords", "specialChars", "numbers", "minNchar")
#' # termsToDrop <- unlist(lapply(whatToDrop, function(x) termsToDropList[[x]]))
#' # tdm <- trim(tdm, termsToDrop = termsToDrop)
as.speeches <- function(
  .Object,
  s_attribute_date = grep("date", sAttributes(.Object), value = TRUE),
  s_attribute_name = grep("name", sAttributes(.Object), value = TRUE),
  gap = 500, mc = FALSE, verbose = TRUE, progress = TRUE
){
  
  # as a first step, create partitions by date
  .message("generating partitions by date", verbose = verbose)
  if (length(sAttributes(.Object, s_attribute_date)) > 1){
    partition_bundle_dates <- partitionBundle(.Object, sAttribute = s_attribute_date)
  } else {
    partition_bundle_dates <- list(.Object)
  }
  
  .message("generating speeches", verbose = verbose)
  .split_by_speakers <- function(partition_date, ...){
    nested <- lapply(
      sAttributes(partition_date, s_attribute_name),
      function(speaker_name){
        partition_bundle_names <- partition(partition_date, def = setNames(list(speaker_name), s_attribute_name), verbose = FALSE)
        split(partition_bundle_names, gap = gap, verbose = FALSE)
      }
    )
    unlist(lapply(1L:length(nested), function(i) nested[[i]]@objects))
  }
  speaker_list_nested <- blapply(
    x = partition_bundle_dates, f = .split_by_speakers,
    s_attribute_name = s_attribute_name, gap = gap,
    mc = mc, progress = progress
  )
  speaker_list <- do.call(c, unlist(speaker_list_nested, recursive = FALSE))
  .message("generating names", verbose = verbose)
  partition_names <- sapply(
    speaker_list,
    function(x){
      paste(x@sAttributes[[s_attribute_name]], sAttributes(x, s_attribute_date), x@name, sep = "_")
    }
  )
  for (i in 1L:length(speaker_list)) name(speaker_list[[i]]) <- partition_names[i]
  
  # at this stage, the list may contain partitions of size 0, that need to be dropped
  empty <- which(sapply(speaker_list, function(x) size(x)) == 0)
  if (length(empty) > 0) for (i in rev(empty)) speakerFlatList[[i]] <- NULL
  
  # the resulting list may be totally unordered - reorder now
  .message("reordering partitions", verbose = verbose)
  speaker_list_ordered <- lapply(
    order(sapply(speaker_list, function(x) x@cpos[1,1])),
    function(i) speaker_list[[i]]
  )
  
  as.bundle(speaker_list_ordered)
}