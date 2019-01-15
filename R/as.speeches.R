#' @include S4classes.R
NULL

#' Split corpus or partition into speeches.
#' 
#' Split entire corpus or a partition into speeches. The heuristic is to split
#' the corpus/partition into partitions on day-to-day basis first, using the
#' s-attribute provided by \code{s_attribute_date}. These subcorpora are then
#' splitted into speeches by speaker name, using s-attribute
#' \code{s_attribute_name}. If there is a gap larger than the number of tokens
#' supplied by argument \code{gap}, contributions of a speaker are assumed to be
#' two seperate speeches.
#' 
#' @param .Object A \code{partition}, or length-one character vector indicating a CWB corpus.
#' @param s_attribute_date The s-attribute that provides the dates of sessions.
#' @param s_attribute_name The s-attribute that provides the names of speakers.
#' @param gap Number of tokens between strucs assumed to make the difference
#'   whether a speech has been interrupted (by an interjection or question), or
#'   whether to assume seperate speeches.
#' @param mc Whether to use multicore, defaults to \code{FALSE}.
#' @param verbose A logical value, defaults to \code{TRUE}.
#' @param progress logical
#' @return A \code{partition_bundle}, the names of the objects in the bundle are
#'   the speaker name, the date of the speech and an index for the number of the
#'   speech on a given day, concatenated by underscores.
#' @name as.speeches
#' @export as.speeches
#' @rdname as.speeches
#' @examples 
#' use("polmineR")
#' speeches <- as.speeches(
#'   "GERMAPARLMINI",
#'   s_attribute_date = "date", s_attribute_name = "speaker"
#' )
#' speeches_count <- count(speeches, p_attribute = "word")
#' tdm <- as.TermDocumentMatrix(speeches_count, col = "count")
#' 
#' bt <- partition("GERMAPARLMINI", date = "2009-10-27")
#' speeches <- as.speeches(bt, s_attribute_name = "speaker")
#' summary(speeches)
as.speeches <- function(
  .Object,
  s_attribute_date = grep("date", s_attributes(.Object), value = TRUE),
  s_attribute_name = grep("name", s_attributes(.Object), value = TRUE),
  gap = 500, mc = FALSE, verbose = TRUE, progress = TRUE
){
  
  stopifnot(
    is.character(s_attribute_date),
    length(s_attribute_date) == 1,
    is.character(s_attribute_name),
    length(s_attribute_name) == 1,
    is.character(.Object) || is.partition(.Object)
  )
  # as a first step, create partitions by date
  .message("generating partitions by date", verbose = verbose)
  if (length(s_attributes(.Object, s_attribute_date)) > 1){
    partition_bundle_dates <- partition_bundle(.Object, s_attribute = s_attribute_date)
  } else {
    partition_bundle_dates <- list(.Object)
  }
  
  .message("generating speeches", verbose = verbose)
  .split_by_speakers <- function(partition_date, ...){
    nested <- lapply(
      s_attributes(partition_date, s_attribute_name),
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
      paste(x@s_attributes[[s_attribute_name]], s_attributes(x, s_attribute_date), x@name, sep = "_")
    }
  )
  for (i in 1L:length(speaker_list)) name(speaker_list[[i]]) <- partition_names[i]
  
  # at this stage, the list may contain partitions of size 0, that need to be dropped
  empty_partitions <- which(sapply(speaker_list, size) == 0L)
  if (length(empty_partitions) > 0L) for (i in rev(empty_partitions)) speaker_list[[i]] <- NULL
  
  # the resulting list may be totally unordered - reorder now
  .message("reordering partitions", verbose = verbose)
  speaker_list_ordered <- lapply(
    order(sapply(speaker_list, function(x) x@cpos[1,1])),
    function(i) speaker_list[[i]]
  )
  corpus <- if (is.character(.Object)) .Object else .Object@corpus
  properties <- registry_get_properties(corpus = corpus)
  if ("type" %in% names(properties)){
    if (properties[["type"]] == "plpr"){
      .message("coercing partitions to plpr_partitions", verbose = verbose)
      speaker_list_ordered <- lapply(speaker_list_ordered, function(x) as(x, "plpr_partition"))
    }
  }
  
  as.bundle(speaker_list_ordered)
}