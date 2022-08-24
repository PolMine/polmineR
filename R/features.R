#' @include textstat.R bundle.R count.R S4classes.R
NULL


setAs(from = "features", to = "features_ngrams", def = function(from){
  new(
    "features_ngrams",
    corpus = from@corpus,
    registry_dir = from@registry_dir,
    data_dir = from@data_dir,
    info_file = from@info_file,
    template = from@template,
    p_attribute = from@p_attribute,
    encoding = from@p_attribute,
    stat = from@stat,
    size_coi = from@size_coi,
    size_ref = from@size_ref,
    method = from@method,
    included = from@included
  )
})



#' @exportMethod summary
#' @docType methods
#' @rdname features-class
setMethod("summary", "features", function(object) {
  implemented_methods <- c("ll", "chisquare")
  methods <- implemented_methods[implemented_methods %in% colnames(object)]
  if (length(methods) == 0L){
    warning("Returning NULL - no statistical test available.")
    invisible(NULL) 
  }
  y <- data.table(
    "p" = c(0.05, 0.01, 0.005, 0.001),
    "critical_value" = c(3.84, 6.63, 7.88, 10.83)
  )
  for (m in methods){
    y[[paste("N", m, sep = "_")]] <- vapply(
      y[["critical_value"]],
      function(x) length(which(object@stat[[m]] >= x)),
      FUN.VALUE = 1L
    )
  }
  setorderv(y, cols = "p", order = 1L)
  return(y)
})

#' @exportMethod show
#' @docType methods
#' @rdname features-class
setMethod("show", "features", function(object){
  message("the statistics table has ", nrow(object@stat), " rows")
  message("pos attributes have been added: ", appendLF = FALSE)
  if ("pos" %in% colnames(object@stat)) message("YES\n") else "NO\n"
})


#' @rdname features-class
setMethod("summary", "features_bundle", function(object){
  tab <- do.call(rbind, lapply(object@objects, function(x) summary(x)$no))
  colnames(tab) <- c("0.001", "0.005", "0.010", "0.050")
  tab
})



#' @include partition.R partition_bundle.R ngrams.R
NULL

#' Get features by comparison.
#' 
#' The features of two objects, usually a \code{partition} defining a corpus of 
#' interest (coi), and a \code{partition} defining a reference corpus (ref) are compared. 
#' The most important purpose is term extraction.
#' 
#' @param x A \code{partition} or \code{partition_bundle} object.
#' @param y A \code{partition} object, it is assumed that the coi is a subcorpus of
#' ref
#' @param by the columns used for merging, if NULL (default), the p-attribute of
#'   x will be used
#' @param method the statistical test to apply (chisquare or log likelihood)
#' @param included TRUE if coi is part of ref, defaults to FALSE
#' @param verbose A \code{logical} value, defaults to TRUE
#' @param progress logical
#' @param mc logical, whether to use multicore
#' @param ... further parameters
#' @author Andreas Blaette
#' @aliases features
#' @docType methods
#' @exportMethod features
#' @references 
#' Baker, Paul (2006): \emph{Using Corpora in Discourse Analysis}. London: continuum, p. 121-149 (ch. 6).
#' 
#' Manning, Christopher D.; Schuetze, Hinrich (1999): \emph{Foundations of Statistical Natural Language
#' Processing}. MIT Press: Cambridge, Mass., pp. 151-189 (ch. 5).
#' @examples
#' use("polmineR")
#' 
#' kauder <- partition(
#'   "GERMAPARLMINI",
#'   speaker = "Volker Kauder", interjection = "speech",
#'   p_attribute = "word"
#'   )
#' all <- partition("GERMAPARLMINI", interjection = "speech", p_attribute = "word")
#'
#' terms_kauder <- features(x = kauder, y = all, included = TRUE)
#' top100 <- subset(terms_kauder, rank_chisquare <= 100)
#' head(top100)
#' 
#' # a different way is to compare count objects
#' kauder_count <- as(kauder, "count")
#' all_count <- as(all, "count")
#' terms_kauder <- features(kauder_count, all_count, included = TRUE)
#' top100 <- subset(terms_kauder, rank_chisquare <= 100)
#' head(top100)
#' 
#' speakers <- partition_bundle("GERMAPARLMINI", s_attribute = "speaker")
#' speakers <- enrich(speakers, p_attribute = "word")
#' speaker_terms <- features(speakers[1:5], all, included = TRUE, progress = TRUE)
#' dtm <- as.DocumentTermMatrix(speaker_terms, col = "chisquare")
#' @rdname features
#' @name features
setGeneric("features", function(x, y, ...) standardGeneric("features"))



#' @rdname features
#' @importFrom RcppCWB cqp_list_corpora
setMethod("features", "partition", function(
  x, y,
  included = FALSE,
  method = "chisquare",
  verbose = FALSE
) {
  
  # check that counts are available
  if (length(x@p_attribute) == 0) stop("no count performed for x - enrich the object!")
  if (!is.character(y)){
    if (length(y@p_attribute) == 0) stop("no count performed for y - enrich the object!")
    if (!identical(x@p_attribute, y@p_attribute)) stop("mismatch of p-attribute of x and y")
  }
  
  # if y is a character vector, create a partition from corpus
  if (is.character(y)){
    stopifnot(length(y) == 1L) # can only compare to exactly one 
    stopifnot(y %in% cqp_list_corpora()) # make sure that it is a corpus that is available
    if (y == x@corpus && included == FALSE){
      included <- TRUE
      warning("x is derived from corpus y, but included is FALSE - setting to TRUE")
    }
    ref_corpus <- corpus(y)
    y <- count(ref_corpus, p_attribute = x@p_attribute)
  }
  
  .message ('Comparing x and y ...', verbose = verbose)
  features(
    x = as(x, "count"), y = as(y, "count"), 
    included = included, method = method, verbose = verbose
  )
})


#' @rdname  features
setMethod("features", "count", function(x, y, by = NULL, included = FALSE, method = "chisquare", verbose = TRUE){
  stopifnot(
    x@encoding == y@encoding,
    identical(x@p_attribute, y@p_attribute)
  )
  z <- new(
    "features",
    encoding = x@encoding,
    included = included,
    corpus = unique(c(x@corpus, y@corpus)),
    registry_dir = x@registry_dir,
    data_dir = x@data_dir,
    info_file = x@info_file,
    template = x@template,
    size_coi = x@size,
    size_ref = if (included) y@size - x@size else y@size,
    p_attribute = x@p_attribute,
    stat = data.table()
  )
  
  .message("combining frequency lists", verbose = verbose)
  # merge.data.table - good option, because keys would be used if present
  if (is.null(by)) by <- z@p_attribute
  z@stat <- merge(x@stat, y@stat, by = by) 
  
  setnames(z@stat, c("count.x", "count.y"),  c("count_coi", "count_ref"))
  if (included) z@stat[, "count_ref" := z@stat[["count_ref"]] - z@stat[["count_coi"]] ]
  
  for (how in method){
    .message("statistical test: ", how, verbose = verbose)
    z <- do.call(how, args = list(.Object = z))
  }
  z
})




#' @rdname features
setMethod("features", "partition_bundle", function(
  x, y, 
  included = FALSE, method = "chisquare", verbose = TRUE, mc = getOption("polmineR.mc"), progress = FALSE
) {
  .fn <- function(x, y, included, method, ...) features(x = x, y = y, included = included, method = method)
  retval <- new("features_bundle")
  retval@objects <- blapply(
    x@objects,
    f = .fn,
    y = y,
    included = included,
    method = method,
    verbose = verbose,
    mc = mc,
    progress = progress
  )
  names(retval@objects) <- names(x@objects)
  retval
})


#' @rdname features
#' @examples
#' # Get features of objects in a count_bundle
#' ref <- corpus("GERMAPARLMINI") %>% count(p_attribute = "word")
#' cois <- corpus("GERMAPARLMINI") %>%
#'   subset(speaker %in% c("Angela Dorothea Merkel", "Hubertus Heil")) %>%
#'   split(s_attribute = "speaker") %>%
#'   count(p_attribute = "word")
#' y <- features(cois, ref, included = TRUE, method = "chisquare", progress = TRUE)
setMethod("features", "count_bundle", function(
  x, y, 
  included = FALSE, method = "chisquare", verbose = !progress, mc = getOption("polmineR.mc"), progress = FALSE
) {
  .fn <- function(x) features(x = x, y = y, included = included, verbose = verbose, method = method)
  retval <- new("features_bundle")
  retval@objects <- if (progress) pblapply(x@objects, .fn, cl = mc) else lapply(x@objects, .fn)
  names(retval@objects) <- names(x@objects)
  retval
})



#' @rdname features
setMethod(
  "features", "ngrams",
  function(x, y, included = FALSE, method = "chisquare", verbose = TRUE, ...){
    stopifnot(
      identical(x@p_attribute, y@p_attribute),
      x@n == y@n,
      all(method %in% c("chisquare", "ll"))
    )
    token_colnames <- sapply(1L:x@n, function(i) paste(x@p_attribute, i, sep = "_"))
    z <- callNextMethod(
      x = x, y = y, by = token_colnames,
      included = included, method = method, verbose = verbose
    )
    z <- as(z, "features_ngrams")
    z@n <- x@n
    z
  }
)

