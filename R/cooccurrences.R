#' @include context.R textstat.R partition.R polmineR.R cooccurrences.R bundle.R S4classes.R
NULL

#' Methods for manipulating \code{cooccurrences_reshaped}-class-objects
#' 
#' @param x cooccurrences for a corpus of interest
#' @param y cooccurrences for a reference corpus
#' @rdname cooccurrences_reshaped
#' @aliases cooccurrences_reshaped merge,cooccurrences_reshaped-method
#' @name cooccurrences_reshaped
NULL


#' @docType methods
#' @rdname cooccurrences-class
setMethod('summary', 'cooccurrences', function(object) {
  cat("\n** Context object: **\n")
  cat(sprintf("%-20s", "CWB corpus:"), object@corpus, "\n")
  cat(sprintf("%-20s", "partition:"), object@partition, "\n")
  cat(sprintf("%-20s", "node:"), object@query, "\n")
  cat(sprintf("%-20s", "p-Attribute:"), object@p_attribute, "\n")
  cat(sprintf("%-20s", "node count:"), object@count, "\n")
  cat(sprintf("%-20s", "stat table length:"), nrow(object@stat), "\n\n")
})



#' @docType methods
#' @rdname cooccurrences-class
setMethod("show", "cooccurrences", function(object) {
  object <- round(object)
  if (Sys.getenv("RSTUDIO") == "1"){
    view(object)
  } else {
    if (getOption("polmineR.browse") == TRUE){
      browse(object@stat)  
    } else {
      return(object@stat) 
    }
  }
})



#' @importFrom data.table copy
#' @rdname cooccurrences-class
setMethod("as.data.frame", "cooccurrences_bundle", function(x){
  dts <- lapply(
    x@objects,
    function(object) copy(object@stat)[, "a" := object@query, with = TRUE]
  )
  dt <- rbindlist(dts)
  pAttr <- unique(unlist(lapply(x@objects, function(C) C@p_attribute)))
  if (length(pAttr) > 1){
    b <- dt[[ pAttr[1] ]]
    for (i in 2:length(pAttr)) b <- paste(b, dt[[pAttr[i]]], sep = "//")
    dt[, "b":= b, with = TRUE]
    for (i in 1:length(pAttr)) dt[, eval(pAttr[i]) := NULL, with = TRUE]
  } else if (length(pAttr) == 1){
    setnames(dt, old = pAttr, new = "b")
  }
  setcolorder(dt, c("a", "b", colnames(dt)[-which(colnames(dt) %in% c("a", "b"))]))
  as.data.frame(dt)
})

#' Get cooccurrence statistics.
#' 
#' @param .Object a partition object, or a character vector with a CWB corpus
#' @param query query, may by a character vector to match a token, or a CQP query
#' @param cqp defaults to \code{is.cqp}-function, or provide TRUE/FALSE, relevant only if query is not NULL
#' @param cpos integer vector with corpus positions, defaults to NULL - then the corpus positions for the whole corpus will be used
#' @param p_attribute the p-attribute of the tokens/the query
#' @param s_attribute if provided, it will be checked that cpos do not extend beyond
#' the region defined by the s-attribute 
#' @param left no of tokens and to the left of the node word
#' @param right no of tokens to the right of the node word
#' @param stoplist exclude a query hit from analysis if stopword(s) is/are in
#'   context (relevant only if query is nut NULL)
#' @param positivelist character vector or numeric vector: include a query hit
#'   only if token in positivelist is present. If positivelist is a character
#'   vector, it is assumed to provide regex expressions (incredibly long if the
#'   list is long) (relevant only if query is nut NULL)
#' @param regex logical, whether stoplist/positivelist are dealt with as regular expressions
#' @param method statistical test to use (defaults to "ll")
#' @param verbose logical, whether to be verbose
#' @param progress logical, whether to be verbose
#' @param keep list with tokens to keep
#' @param mc whether to use multicore
#' @param ... further parameters that will be passed into bigmatrix (applies only of big=TRUE)
#' @return a cooccurrences-class object
#' @exportMethod cooccurrences
#' @docType methods
#' @author Andreas Blaette
#' @export cooccurrences
#' @name cooccurrences
#' @rdname cooccurrences
#' @references 
#' Baker, Paul (2006): \emph{Using Corpora in Discourse Analysis}. London: continuum, p. 95-120 (ch. 5).
#' 
#' Manning, Christopher D.; Schuetze, Hinrich (1999): \emph{Foundations of Statistical Natural Language
#' Processing}. MIT Press: Cambridge, Mass., pp. 151-189 (ch. 5).
#' @examples
#' use("polmineR")
#' merkel <- partition("GERMAPARLMINI", interjection = "speech", speaker = ".*Merkel", regex = TRUE)
#' merkel <- enrich(merkel, p_attribute = "word")
#' cooc <- cooccurrences(merkel, query = "Deutschland")
setGeneric("cooccurrences", function(.Object, ...) standardGeneric("cooccurrences") )

#' @rdname cooccurrences
setMethod("cooccurrences", "character", function(
  .Object, query, cqp = is.cqp,
  p_attribute = getOption("polmineR.p_attribute"), s_attribute = NULL,
  left = getOption("polmineR.left"), right = getOption("polmineR.right"),
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  keep = NULL, cpos = NULL, method = "ll",
  mc = getOption("polmineR.mc"), verbose = FALSE, progress = FALSE,
  ...
){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  if (missing(query)) stop("query missing - it is not possible to calculate cooccurrences")
  C <- context(
    .Object = .Object, query = query, cqp = is.cqp,
    p_attribute = p_attribute, s_attribute = s_attribute,
    left = left, right = right,
    stoplist = stoplist, positivelist = positivelist, regex = regex,
    count = TRUE, 
    mc = mc, verbose = verbose, progress = progress
  )
  if (is.null(C)) invisible(NULL) else cooccurrences(C, method = method, verbose = verbose)
})

#' @rdname cooccurrences
setMethod(
  "cooccurrences", "partition",
  function(
    .Object, query, cqp = is.cqp,
    left = getOption("polmineR.left"), right = getOption("polmineR.right"),
    p_attribute = getOption("polmineR.p_attribute"), s_attribute = NULL,
    stoplist = NULL, positivelist = NULL, keep = NULL,
    method = "ll",
    mc = FALSE, progress = TRUE, verbose = FALSE,
    ...
  ){
    if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
    if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
    C <- context(
      .Object = .Object, query = query, cqp = is.cqp,
      p_attribute = p_attribute, s_attribute = s_attribute,
      left = left, right = right,
      stoplist = stoplist, positivelist = positivelist,
      count = TRUE, 
      mc = mc, verbose = verbose, progress = progress
    )
    retval <- if (is.null(C)) invisible(NULL) else cooccurrences(C, method = method, verbose = verbose)
    retval
  }
)

#' @rdname cooccurrences
setMethod("cooccurrences", "context", function(.Object, method = "ll", verbose = FALSE){
  if (!is.null(method)){
    
    # enrich partition if necessary
    if (!all(paste(.Object@p_attribute, "id", sep = "_") %in% colnames(.Object@partition@stat))){
      .message("adding missing count for p-attribute ", .Object@p_attribute, " to partition", verbose = verbose)
      .Object@partition <- enrich(.Object@partition, p_attribute = .Object@p_attribute, decode = FALSE, verbose = verbose)
    }
    
    setkeyv(.Object@stat, cols = paste(.Object@p_attribute, "id", sep = "_"))
    setkeyv(.Object@partition@stat, cols = paste(.Object@p_attribute, "id", sep = "_"))
    .Object@stat <- .Object@partition@stat[.Object@stat]
    for (pAttr in .Object@p_attribute){
      if (paste("i", pAttr, sep = ".") %in% colnames(.Object@stat)){
        .Object@stat[, eval(paste("i", pAttr, sep = ".")) := NULL, with = TRUE]
      }
    }
    setnames(.Object@stat, old = "count", new = "count_partition")
    for (test in method){
      .message("statistical test:", test, verbose = verbose)
      .Object <- do.call(test, args = list(.Object = .Object))  
    }
  }
  
  # finishing
  if (nrow(.Object@stat) > 0){
    setkeyv(.Object@stat, .Object@p_attribute)
    for (x in grep("_id$", colnames(.Object@stat), value = TRUE)) .Object@stat[[x]] <- NULL
    setcolorder(
      .Object@stat,
      c(.Object@p_attribute, colnames(.Object@stat)[-which(colnames(.Object@stat) %in% .Object@p_attribute)])
    )
    setorderv(.Object@stat, cols = method[1], order = -1L)
  }
  
  retval <- new("cooccurrences")
  slotsToGet <- slotNames(retval)[-grep("partition", slotNames(retval))]
  for (x in slotsToGet) slot(retval, x) <- slot(.Object, x)
  retval
})


#' @rdname context-method
setMethod("cooccurrences", "Corpus", function(.Object, query, p_attribute = getOption("polmineR.p_attribute"), ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if (nrow(.Object$stat) == 0) .Object$count(p_attribute, decode = FALSE)
  P <- .Object$as.partition()
  cooccurrences(P, query = query, p_attribute = p_attribute, ...)
})



#' @rdname cooccurrences
setMethod("cooccurrences", "partition_bundle", function(.Object, query, mc = getOption("polmineR.mc"), ...){
  bundle <- new("cooccurrences_bundle")
  bundle@objects <- pbapply::pblapply(
    .Object@objects,
    function(x) cooccurrences(x, query = query, mc = mc, ...) 
  )
  names(bundle@objects) <- names(.Object@objects)
  for (i in 1L:length(bundle@objects)){
    if (!is.null(bundle@objects[[i]])) bundle@objects[[i]]@name <- .Object@objects[[i]]@name
  }
  for (i in rev(which(sapply(bundle@objects, is.null)))) bundle@objects[[i]] <- NULL
  bundle
})

