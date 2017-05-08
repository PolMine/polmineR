#' Get cooccurrence statistics.
#' 
#' @param .Object a partition object, or a character vector with a CWB corpus
#' @param query query, may by a character vector to match a token, or a CQP query
#' @param cqp defaults to \code{is.cqp}-function, or provide TRUE/FALSE, relevant only if query is not NULL
#' @param cpos integer vector with corpus positions, defaults to NULL - then the corpus positions for the whole corpus will be used
#' @param pAttribute the pAttribute of the tokens/the query
#' @param sAttribute if provided, it will be checked that cpos do not extend beyond
#' the region defined by the s-attribute 
#' @param left no of tokens and to the left of the node word
#' @param right no of tokens to the right of the node word
#' @param stoplist exclude a query hit from analysis if stopword(s) is/are in
#'   context (relevant only if query is nut NULL)
#' @param positivelist character vector or numeric vector: include a query hit
#'   only if token in positivelist is present. If positivelist is a character
#'   vector, it is assumed to provide regex expressions (incredibly long if the
#'   list is long) (relevant only if query is nut NULL)
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
#' @examples
#' \dontrun{
#' use("polmineR.sampleCorpus")
#' merkel <- partition("PLPRBTTXT", text_type = "speech", text_name = ".*Merkel", regex = TRUE)
#' merkel <- enrich(merkel, pAttribute = "word")
#' cooc <- cooccurrences(merkel, query = "Deutschland")
#' }
setGeneric("cooccurrences", function(.Object, ...) standardGeneric("cooccurrences") )

#' @rdname cooccurrences
setMethod("cooccurrences", "character", function(
  .Object, query = NULL, cqp = is.cqp,
  pAttribute = getOption("polmineR.pAttribute"), sAttribute = NULL,
  left = getOption("polmineR.left"), right = getOption("polmineR.right"),
  stoplist = NULL, positivelist = NULL,
  keep = NULL, cpos = NULL, method = "ll",
  mc = getOption("polmineR.mc"), verbose = FALSE, progress = FALSE
  ){
  if (is.null(query) == FALSE){
    C <- context(
      .Object = .Object, query = query, cqp = is.cqp,
      pAttribute = pAttribute, sAttribute = sAttribute,
      left = left, right = right,
      stoplist = stoplist, positivelist = positivelist,
      count = TRUE, 
      mc = mc, verbose = verbose, progress = progress
    )
    return( cooccurrences(C, method = method, verbose = verbose) )
  } else {
  }
})

#' @rdname cooccurrences
setMethod(
  "cooccurrences", "partition",
  function(
    .Object, query, cqp = is.cqp,
    left = getOption("polmineR.left"), right = getOption("polmineR.right"),
    pAttribute = getOption("polmineR.pAttribute"), sAttribute = NULL,
    stoplist = NULL, positivelist = NULL, keep = NULL,
    method = "ll",
    mc = FALSE, progress = TRUE, verbose = FALSE, ...
  ){
    C <- context(
      .Object = .Object, query = query, cqp = is.cqp,
      pAttribute = pAttribute, sAttribute = sAttribute,
      left = left, right = right,
      stoplist = stoplist, positivelist = positivelist,
      count = TRUE, 
      mc = mc, verbose = verbose, progress = progress
    )
    if (is.null(C)){
      return(NULL)
    } else {
      return( cooccurrences(C, method = method, verbose = verbose) )
    }
  })

#' @rdname cooccurrences
setMethod("cooccurrences", "context", function(.Object, method = "ll", verbose = FALSE){
  if (!is.null(method)){
    
    # enrich partition if necessary
    if (!all(paste(.Object@pAttribute, "id", sep = "_") %in% colnames(.Object@partition@stat))){
      if (verbose) message("... adding missing count for pAttribute ", .Object@pAttribute, " to partition")
      .Object@partition <- enrich(.Object@partition, pAttribute = .Object@pAttribute, id2str = FALSE, verbose = verbose)
    }
    
    setkeyv(.Object@stat, cols = paste(.Object@pAttribute, "id", sep = "_"))
    setkeyv(.Object@partition@stat, cols = paste(.Object@pAttribute, "id", sep = "_"))
    .Object@stat <- .Object@partition@stat[.Object@stat]
    for (pAttr in .Object@pAttribute){
      if (paste("i", pAttr, sep = ".") %in% colnames(.Object@stat)){
        .Object@stat[, eval(paste("i", pAttr, sep = ".")) := NULL, with = TRUE]
      }
    }
    setnames(.Object@stat, old = "count", new = "count_partition")
    for (test in method){
      .verboseOutput(message = paste("statistical test:", test), verbose = verbose)
      .Object <- do.call(test, args = list(.Object = .Object))  
    }
  }
  
  # finishing
  if (nrow(.Object@stat) > 0){
    setkeyv(.Object@stat, .Object@pAttribute)
    for (x in grep("_id$", colnames(.Object@stat), value = TRUE)) .Object@stat[[x]] <- NULL
    setcolorder(
      .Object@stat,
      c(.Object@pAttribute, colnames(.Object@stat)[-which(colnames(.Object@stat) %in% .Object@pAttribute)])
    )
    setorderv(.Object@stat, cols = method[1], order = -1L)
  }
  
  retval <- new("cooccurrences")
  slotsToGet <- slotNames(retval)[-grep("partition", slotNames(retval))]
  for (x in slotsToGet) slot(retval, x) <- slot(.Object, x)
  retval
})


#' @rdname context-method
setMethod("cooccurrences", "Corpus", function(.Object, query, pAttribute = getOption("polmineR.pAttribute"), ...){
  if (nrow(.Object$stat) == 0) .Object$count(pAttribute, id2str = FALSE)
  P <- .Object$as.partition()
  cooccurrences(P, query = query, pAttribute = pAttribute, ...)
})



#' @rdname cooccurrences
setMethod("cooccurrences", "partitionBundle", function(.Object, query, mc = getOption("polmineR.mc"), ...){
  bundle <- new("cooccurrencesBundle")
  if (requireNamespace("pbapply", quietly = TRUE)){
    bundle@objects <- pbapply::pblapply(
      .Object@objects,
      function(x) cooccurrences(x, query = query, mc = mc, ...) 
    )
  } else {
    bundle@objects <- lapply(
      .Object@objects,
      function(x) cooccurrences(x, query = query, mc = mc, ...) 
    )
    
  }
  names(bundle@objects) <- names(.Object@objects)
  for (i in 1:length(bundle@objects)) bundle@objects[[i]]@name <- .Object@objects[[i]]@name
  bundle
})

