#' @include S4classes.R make_region_matrix.R
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
#' @param .Object A \code{partition}, or length-one \code{character} vector
#'   indicating a CWB corpus.
#' @param s_attribute_date A length-one \code{character} vector, the s-attribute
#'   that provides the dates of sessions.
#' @param s_attribute_name A length-one \code{character} vector, the s-attribute
#'   that provides the names of speakers.
#' @param gap An \code{integer} value, the number of tokens between strucs
#'   assumed to make the difference whether a speech has been interrupted (by an
#'   interjection or question), or whether to assume seperate speeches.
#' @param mc Whether to use multicore, defaults to \code{FALSE}. If
#'   \code{progress} is \code{TRUE}, argument \code{mc} is passed into
#'   \code{pblapply} as argument \code{cl}. If \code{progress} is \code{FALSE},
#'   \code{mc} is passed into \code{mclapply} as argument \code{mc.cores}.
#' @param verbose A \code{logical} value, defaults to \code{TRUE}.
#' @param progress A \code{logical} value, whether to show progress bar.
#' @param ... Further arguments.
#' @return A \code{partition_bundle}, the names of the objects in the bundle are
#'   the speaker name, the date of the speech and an index for the number of the
#'   speech on a given day, concatenated by underscores.
#' @name as.speeches
#' @exportMethod as.speeches
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
setGeneric("as.speeches", function(.Object, ...) standardGeneric("as.speeches"))

#' @exportMethod as.speeches
#' @rdname as.speeches
setMethod("as.speeches", "partition", function(
  .Object,
  s_attribute_date = grep("date", s_attributes(.Object), value = TRUE),
  s_attribute_name = grep("name", s_attributes(.Object), value = TRUE),
  gap = 500, mc = FALSE, verbose = TRUE, progress = TRUE
){
  
  stopifnot(
    is.character(s_attribute_date),
    length(s_attribute_date) == 1,
    is.character(s_attribute_name),
    length(s_attribute_name) == 1
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
})


#' @exportMethod as.speeches
#' @rdname as.speeches
setMethod("as.speeches", "subcorpus", function(
  .Object,
  s_attribute_date = grep("date", s_attributes(.Object), value = TRUE),
  s_attribute_name = grep("name", s_attributes(.Object), value = TRUE),
  gap = 500, mc = FALSE, verbose = TRUE, progress = TRUE
){
  as.speeches(
    .Object = as(.Object, "partition"),
    s_attribute_date = s_attribute_date,
    s_attribute_name = s_attribute_name,
    gap = gap,
    mc = mc, verbose = verbose, progress = progress
  )
}
)


#' @rdname as.speeches
#' @examples
#' sp <- as.speeches(.Object = corpus("GERMAPARLMINI"), s_attribute_name = "speaker")
setMethod("as.speeches", "corpus", function( 
  .Object,
  s_attribute_date = grep("date", s_attributes(.Object), value = TRUE),
  s_attribute_name = grep("name", s_attributes(.Object), value = TRUE),
  gap = 500, mc = FALSE, verbose = TRUE, progress = TRUE
){
  m <- make_region_matrix(.Object, s_attribute = s_attribute_name) # in polmineR, not exported
  dates <- s_attributes(.Object, s_attribute = s_attribute_date, unique = FALSE)
  strucs <- 0L:(nrow(m) - 1L)
  speakers <- s_attributes(.Object, s_attribute = s_attribute_name, unique = FALSE)
  
  if (length(dates) != length(speakers))
    stop(
      sprintf(
        "Number of regions for s_attribute '%s' and s_attribute '%s' not identical - procedure will not work",
        s_attribute_date, s_attribute_name)
    )

  chunks_cpos <- split(x = m, f = speakers)
  chunks_dates <- split(x = dates, f = speakers)
  chunks_strucs <- split(x = strucs, f = speakers)

  new_class <- if (length(.Object@type) == 0L) "subcorpus" else paste(.Object@type, "subcorpus", sep = "_")
  
  .iter_fn <- function(i){
    mx <- matrix(data = chunks_cpos[[i]], byrow = FALSE, ncol = 2L)
    
    # if we have a matrix with only one region (i.e. one row), no need for further splitting,
    # we return a subcorpus immediately
    if (nrow(mx) == 1L){
      return(list(new(
        new_class,
        strucs = chunks_strucs[[i]],
        cpos = mx,
        corpus = .Object@corpus,
        type = .Object@type,
        encoding = .Object@encoding,
        data_dir = .Object@data_dir,
        s_attributes = setNames(list(chunks_dates[[i]], names(chunks_cpos)[i]), nm = c(s_attribute_date, s_attribute_name)),
        xml = "flat",
        s_attribute_strucs = s_attribute_name,
        name = sprintf("%s_%s_%d", names(chunks_cpos)[[i]], chunks_dates[[i]], 1L),
        size = mx[,2] - mx[,1] + 1L
      )))
    }
    
    distance <- mx[,1][2L:nrow(mx)] - mx[,2][1L:(nrow(mx) - 1L)]
    beginning <- c(TRUE, ifelse(distance > gap, TRUE, FALSE))
    beginning <- ifelse(
      c(TRUE, chunks_dates[[i]][2L:length(chunks_dates[[i]])] == chunks_dates[[i]][1L:(length(chunks_dates[[i]]) - 1L)]),
      beginning,
      TRUE
    )
    razor <- cumsum(beginning)
    vec_dates <- chunks_dates[[i]][beginning]
    
    # The speech_no vector indicates the number of the speech at a date for the speeches
    # referred to with the vec_dates vector. These lines have seen some revisions to 
    # make the procedure robust. Resorting to the for loop is able to handle situations
    # robustly when dates are not increasing throughout.
    unique_dates <- unique(vec_dates)
    speech_no_aux <- setNames(rep(1L, times = length(unique_dates)), nm = unique_dates)
    speech_no <- rep(NA, times = length(vec_dates))
    for (k in seq_along(vec_dates)){
      speech_no[k] <- speech_no_aux[[vec_dates[k]]]
      speech_no_aux[[vec_dates[k]]] <- speech_no_aux[[vec_dates[k]]] + 1L
    }
    
    li_cpos <- split(mx, f = razor)
    li_strucs <- split(chunks_strucs[[i]], f = razor)
    lapply(
      seq_along(li_cpos),
      function(j){
        cpos_matrix <- matrix(data = li_cpos[[j]], byrow = FALSE, ncol = 2L)
        new(
          new_class,
          strucs = li_strucs[[j]],
          cpos = cpos_matrix,
          corpus = .Object@corpus,
          type = .Object@type,
          encoding = .Object@encoding,
          data_dir = .Object@data_dir,
          s_attributes = setNames(list(vec_dates[[j]], names(chunks_cpos)[i]), nm = c(s_attribute_date, s_attribute_name)),
          xml = "flat",
          s_attribute_strucs = s_attribute_name,
          name = sprintf("%s_%s_%d", names(chunks_cpos)[i], vec_dates[[j]], speech_no[[j]]),
          size = sum(cpos_matrix[,2] - cpos_matrix[,1] + 1L)
        )
      }
    )
  }
  y <- if (progress){
    pblapply(seq_along(chunks_cpos), .iter_fn, cl = mc)
  } else {
    if (isFALSE(mc)){
      lapply(seq_along(chunks_cpos), .iter_fn)
    } else {
      mclapply(seq_along(chunks_cpos), .iter_fn, mc.cores = mc)
    }
  }
  

  retval <- new(
    "subcorpus_bundle",
    xml = "flat",
    objects = unlist(y, recursive = FALSE),
    corpus = .Object@corpus,
    encoding = .Object@encoding
  )
  names(retval@objects) <- sapply(retval@objects, name)
  retval
})



#' @rdname as.speeches
setMethod("as.speeches", "character", function( 
  .Object,
  s_attribute_date = grep("date", s_attributes(.Object), value = TRUE),
  s_attribute_name = grep("name", s_attributes(.Object), value = TRUE),
  gap = 500, mc = FALSE, verbose = TRUE, progress = TRUE
){
  as.speeches(
    .Object = corpus(.Object),
    s_attribute_date = s_attribute_date,
    s_attribute_name = s_attribute_name,
    gap = gap,
    mc = mc,
    verbose = verbose,
    progress = progress
  )
})