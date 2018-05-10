#' @include partition_class.R partitionBundle_class.R ngrams_method.R
NULL

setGeneric("features", function(x, y, ...) standardGeneric("features"))


#' Get features by comparison.
#' 
#' The features of two objects, usually a partition defining a corpus of 
#' interest, and a partition defining a reference corpus are compared. 
#' The most important purpose is term extraction.
#' 
#' @param x a partition or partitionBundle object
#' @param y a partition object, it is assumed that the coi is a subcorpus of
#' ref
#' @param by the columns used for merging, if NULL (default), the pAttribute of
#'   x will be used
#' @param method the statistical test to apply (chisquare or log likelihood)
#' @param included TRUE if coi is part of ref, defaults to FALSE
#' @param verbose logical, defaults to TRUE
#' @param progress logical
#' @param mc logical, whether to use multicore
#' @param ... further parameters
#' @author Andreas Blaette
#' @aliases features
#' @docType methods
#' @references Manning / Schuetze ...
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
#'   pAttribute="word"
#'   )
#' all <- partition("GERMAPARLMINI", interjection = "speech", pAttribute = "word")
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
#' speakers <- partitionBundle("GERMAPARLMINI", sAttribute = "speaker")
#' speakers <- enrich(speakers, pAttribute = "word")
#' speaker_terms <- features(speakers[[1:5]], all, included = TRUE, progress = TRUE)
#' dtm <- as.DocumentTermMatrix(speaker_terms, col = "chisquare")
#' @rdname  features-method
setMethod("features", "partition", function(
  x, y,
  included = FALSE,
  method = "chisquare",
  verbose = FALSE
) {
  
  # check that counts are available
  if (length(x@pAttribute) == 0) stop("no count performed for x - enrich the object!")
  if (!is.character(y)){
    if (length(y@pAttribute) == 0) stop("no count performed for y - enrich the object!")
    if (!identical(x@pAttribute, y@pAttribute)) stop("mismatch of pAttribute of x and y")
  }

  # if y is a character vector, create a partition from corpus
  if (is.character(y)){
    stopifnot(length(y) == 1) # can only compare to exactly one 
    stopifnot(y %in% CQI$list_corpora()) # make sure that it is a corpus that is available
    if (y == x@corpus && included == FALSE){
      included <- TRUE
      warning("x is derived from corpus y, but included is FALSE - setting to TRUE")
    }
    refCorpus <- Corpus$new(y)
    refCorpus$count(pAttribute = x@pAttribute)
    y <- refCorpus$as.partition()
  }
  
  .message ('Comparing x and y ...', verbose = verbose)
  features(
    x = as(x, "count"), y = as(y, "count"), 
    included = included, method = method, verbose = verbose
    )
})


#' @rdname  features-method
setMethod("features", "count", function(x, y, by = NULL, included = FALSE, method = "chisquare", verbose = TRUE){
  stopifnot(
    x@encoding == y@encoding,
    identical(x@pAttribute, y@pAttribute)
    )
  z <- new(
    "features",
    encoding = x@encoding, included = included,
    corpus = unique(c(x@corpus, y@corpus)),
    sizeCoi = x@size,
    sizeRef = if (included) y@size - x@size else y@size,
    pAttribute = x@pAttribute,
    stat = data.table()
  )

  .message("combining frequency lists", verbose = verbose)
  # merge.data.table - good option, because keys would be used if present
  if (is.null(by)) by <- z@pAttribute
  z@stat <- merge(x@stat, y@stat, by = by) 
  
  setnames(z@stat, c("count.x", "count.y"),  c("count_coi", "count_ref"))
  if (included) z@stat[, "count_ref" := z@stat[["count_ref"]] - z@stat[["count_coi"]] ]
  
  for (how in method){
    .message("statistical test: ", how, verbose = verbose)
    z <- do.call(how, args = list(.Object = z))
  }
  z
})


#' @rdname features-method
setMethod("features", "partitionBundle", function(
  x, y, 
  included = FALSE, method = "chisquare", verbose = TRUE, mc = getOption("polmineR.mc"), progress = FALSE
) {
  .features <- function(x, y, included, method, ...) features(x = x, y = y, included = included, method = method)
  retval <- new("featuresBundle")
  retval@objects <- blapply(x@objects, f = .features, y = y, included = included, method = method, verbose = verbose, mc=mc, progress=progress)
  names(retval@objects) <- names(x@objects)
  retval
})




#' @rdname features-method
setMethod(
  "features", "ngrams",
  function(x, y, included = FALSE, method = "chisquare", verbose = TRUE, ...){
    stopifnot(
      identical(x@pAttribute, y@pAttribute),
      x@n == y@n,
      all(method %in% c("chisquare", "ll"))
    )
    tokenColnames <- sapply(1:x@n, function(i) paste(i, x@pAttribute, sep = "_"))
    z <- callNextMethod(
      x = x, y = y, by = tokenColnames,
      included = included, method = method, verbose = verbose
    )
    z <- as(z, "featuresNgrams")
    z@n <- x@n
    z
  }
)

