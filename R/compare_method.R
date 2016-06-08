#' @include partition_class.R partitionBundle_class.R ngrams_method.R
NULL

#' @rdname compare-method
setGeneric("compare", function(x, ...){standardGeneric("compare")})


#' compare features of two partitions
#' 
#' @param x a partition or partitionBundle object
#' @param y a partition object, it is assumed that the coi is a subcorpus of
#' ref
#' @param method the statistical test to apply (chisquare or log likelihood)
#' @param included TRUE if coi is part of ref, defaults to FALSE
#' @param verbose defaults to TRUE
#' @param progress logical
#' @param mc logical, whether to use multicore
#' @param ... further parameters
#' @return The function returns a data frame with the following structure:
#' - absolute frequencies in the first row
#' - ...
#' @author Andreas Blaette
#' @docType methods
#' @references Manning / Schuetze ...
#' @exportMethod compare
#' @rdname  compare-method
setMethod("compare", signature=c(x="partition"), function(
  x, y,
  included=FALSE,
  method="chisquare",
  verbose=FALSE
) {
  if (verbose==TRUE) message ('Comparing x and y ...')
  newObject <- new(
    'comp',
    encoding=x@encoding, included=included,
    corpus=x@corpus,
    sizeCoi=x@size, sizeRef=ifelse(included==FALSE, y@size, y@size-x@size),
    stat=data.table()
    )
  newObject@call <- deparse(match.call())
  # check whether count-lists are available for the pAttribute given
  if (length(x@pAttribute) == 0 || length(y@pAttribute) == 0) stop("no count performed for x and y")
  if (identical(x@pAttribute, y@pAttribute) == FALSE){
    stop("mismatch of pAttribute of x and y")
  } else {
    newObject@pAttribute <- x@pAttribute
    pAttribute <- x@pAttribute
  }
  if (verbose==TRUE) message("... combining frequency lists")
  newObject@stat <- merge(x@stat, y@stat, by=pAttribute)
  setnames(newObject@stat, c("count.x", "count.y"),  c("count_coi", "count_ref"))
  if (included == TRUE) newObject@stat[, "count_ref" := newObject@stat[["count_ref"]] - newObject@stat[["count_coi"]] ]
  for (how in method){
    if (verbose==TRUE) message("... statistical test: ", how)
    newObject <- do.call(how, args=list(.Object=newObject))
  }
  newObject
})



#' @docType methods
#' @rdname compare-method
setMethod("compare", signature=c(x="partitionBundle"), function(
  x, y, 
  included=FALSE, method="chisquare", verbose=TRUE, mc=TRUE, progress=FALSE
) {
  kclust <- new("compBundle")
  .compare <- function(a) {
    compare(a, y, included=included, method=method, verbose=verbose)
  }
  if (mc == FALSE){
    if (progress == FALSE){
      kclust@objects <- lapply(setNames(x@objects, names(x@objects)), function(a) .compare(a))  
    } else {
      tmp <- lapply(
        c(1:length(x@objects)),
        function(i) {
          .progressBar(i, length(x@objects))
          .compare(x@objects[[i]])
          })
      names(tmp) <- names(x@objects)
      kclust@objects <- tmp
    }
  } else if (mc == TRUE){
    kclust@objects <- mclapply(
      setNames(x@objects, names(x@objects)),
      function(a) .compare(a),
      mc.cores=getOption("polmineR.mc")
      )
  }
  kclust
})


#' @rdname compare-method
setMethod("compare", "cooccurrences", function(x, y, included=FALSE, method="ll", mc=TRUE, verbose=TRUE){
  newObject <- new(
    'compCooccurrences',
    encoding=x@encoding, included=included, corpus=x@corpus, sizeCoi=x@partitionSize,
    sizeRef=ifelse(included == FALSE, y@partitionSize, y@partitionSize-x@partitionSize),
    stat=data.table()
  )
  if (identical(x@pAttribute, y@pAttribute) == FALSE) {
    warning("BEWARE: cooccurrences objects are not based on the same pAttribute!")
  } else {
    newObject@pAttribute <- x@pAttribute
  }
  if (verbose == TRUE) message("... preparing tabs for matching")
  keys <- unlist(lapply(c("a", "b"), function(ab) paste(ab, x@pAttribute, sep="_"))) 
  setkeyv(x@stat, keys)
  setkeyv(y@stat, keys)
  MATCH <- y@stat[x@stat]
  
  # remove columns not needed
  colsToDrop <- c(
    "ll", "i.ll", "exp_window", "i.exp_window", "rank_ll", "i.rank_ll",
    "size_window", "i.size_window", "count_a", "i.count_a", "count_b", "i.count_b",
    "exp_partition", "i.exp_partition"
    )
  for (drop in colsToDrop) MATCH[, eval(drop) := NULL, with=TRUE]
  setnames(MATCH, old=c("count_ab", "i.count_ab"), new=c("count_ref", "count_coi"))
  
  if (included == TRUE) MATCH[, "count_ref" := MATCH[["count_ref"]] - MATCH[["count_coi"]] ]
  
  newObject@stat <- MATCH
  for (how in method){
    if (verbose==TRUE) message("... statistical test: ", how)
    newObject <- do.call(how, args=list(.Object=newObject))
  }
  newObject
})

#' @rdname compare-method
setMethod("compare", "missing", function(){
  .getClassObjectsAvailable(".GlobalEnv", "comp")
})

#' @rdname compare-method
setMethod(
  "compare", "ngrams",
  function(x, y, included=FALSE, method="chisquare", verbose=TRUE, ...){
    stopifnot(
      identical(x@pAttribute, y@pAttribute) == TRUE,
      x@n == y@n,
      all(method %in% c("chisquare", "ll"))
    )
    tokenColnames <- unlist(lapply(c(1:x@n), function(i) paste("token_", i, "_", x@pAttribute, sep="")))
    setkeyv(x@stat, cols=tokenColnames)
    setkeyv(y@stat, cols=tokenColnames)
    DT <- y@stat[x@stat]
    setnames(DT, c("count", "i.count"), c("count_ref", "count_coi"))
    setcolorder(DT, c(tokenColnames, "count_coi", "count_ref"))
    if (included == TRUE) DT[, "count_ref" := DT[["count_ref"]] - DT[["count_coi"]] ]
    newObject <- new(
      "compareNgrams",
      encoding=x@encoding, included=included, corpus=x@corpus, sizeCoi=x@size,
      pAttribute=x@pAttribute, n=x@n,
      sizeRef=ifelse(included == FALSE, y@size, y@size-x@size),
      stat=DT
      )
    for (how in method){
      if (verbose==TRUE) message("... statistical test: ", how)
      newObject <- do.call(how, args=list(.Object=newObject))
    }
    newObject
  })

