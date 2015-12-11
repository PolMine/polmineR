#' @include partition_class.R partitionBundle_class.R ngrams_method.R
NULL

setGeneric("keyness", function(x, ...){standardGeneric("keyness")})


#' compute chi-square values for tokens in a corpus using a reference corpus
#' 
#' Pearson's chi-squared test is calculated to measure the keyness of a token
#' in a corpus of interest (COI). A reference corpus is required for the
#' computation
#' 
#' If pos.filter is supplied, the most propable pos-Tags will be added to the 
#' statistical table and the table will be filtered. This may slow down the 
#' procedure considerably.
#' 
#' 
#' @param x a partition or partitionBundle object
#' @param y a partition object, it is assumed that the coi is a subcorpus of
#' ref
#' @param pAttribute The P-Attribute that will be counted (usually either
#' 'word' or 'lemma')
#' @param minFrequency the minimum frequency of cooccurrence
#' @param method the statistical test to apply (chisquare or log likelihood)
#' @param included TRUE if coi is part of ref, defaults to FALSE
#' @param digits numeric
#' @param verbose defaults to TRUE
#' @param progress logical
#' @param mc logical, whether to use multicore
#' @return The function returns a data frame with the following structure:
#' - absolute frequencies in the first row
#' - ...
#' @author Andreas Blaette
#' @aliases keyness,cooccurrences-method
#' @docType methods
#' @references Manning / Schuetze ...
#' @exportMethod keyness
#' @rdname  keyness-method
setMethod("keyness", signature=c(x="partition"), function(
  x, y,
  pAttribute=NULL,
  minFrequency=0, included=FALSE,
  method="chiSquare",
  digits=2,
  verbose=TRUE
) {
  if (verbose==TRUE) message ('Computing keyness')
  if (is.null(pAttribute)) pAttribute <- slot(get("session", ".GlobalEnv"), "pAttribute")
  keyness <- new(
    'keyness',
    encoding=x@encoding, included=included, minFrequency=minFrequency,
    corpus=x@corpus, pAttribute=pAttribute,
    sizeCoi=x@size, sizeRef=ifelse(included==FALSE, y@size, y@size-x@size),
    stat=data.table()
    )
  keyness@call <- deparse(match.call())
  keyness@digits <- as.list(setNames(rep(2, times=2+length(method)), c("expCoi", "expRef", method)))
  # check whether count-lists are available for the pAttribute given
  if (identical(pAttribute, x@pAttribute) == FALSE){
    message("... term frequencies not available for pAttribute given in corpus of interest - enriching the partition ")
    x <- enrich(x, pAttribute=pAttribute, verbose=FALSE)
  }
  if (identical(pAttribute, y@pAttribute) == FALSE){
    message("... term frequencies not available for pAttribute given in reference corpus - enriching the partition ")
    y <- enrich(y, pAttribute=pAttribute, verbose=FALSE)
  }
  if (verbose==TRUE) message("... combining frequency lists")
  keyness@stat <- merge(x@stat, y@stat, by=pAttribute)
  # keyness@stat <- merge(x@stat, y@stat, by.x=pAttribute, by.y=pAttribute)
#   rownames(keyness@stat) <- cqi_id2str(
#     paste(x@corpus,".", pAttribute, sep=""),
#     keyness@stat[,"id"]
#     )
  # keyness@stat[, ids.x := NULL]
  # keyness@stat[, ids.y := NULL]
  setnames(keyness@stat, c("count.x", "count.y"),  c("countCoi", "countRef"))
  # Encoding(rownames(keyness@stat)) <- y@encoding
  # colnames(keyness@stat) <- c("id", "countCoi", "countRef")
  if (included == TRUE) keyness@stat[, countRef := keyness@stat[["countRef"]] - keyness@stat[["countCoi"]]]
  if ("chiSquare" %in% method) {
    if (verbose==TRUE) message("... computing chisquare tests")
    keyness <- chisquare(keyness)
  }
  if ("ll" %in% method) {
    if (verbose==TRUE) message("... computing log likelihood tests")
    keyness <- ll(keyness)
  }
  keyness@stat[, rank := c(1:nrow(keyness@stat))]
#   if (length(pAttribute) == 2){
#     keyness@stat[, pAttribute[1] := gsub("^(.*?)//.*?$", "\\1", keyness@stat[["token"]])]
#     keyness@stat[, pAttribute[2] := gsub("^.*?//(.*?)$", "\\1", keyness@stat[["token"]])]
#     keyness@stat[, token := NULL]
#   }
  # keyness@stat <- keyness@stat[,-which(colnames(keyness@stat)=="id")]
  if (verbose==TRUE) message("... trimming table with statistical tests")
  # keyness <- trim(keyness, digits=keyness@digits, verbose=verbose)
  keyness <- sort(keyness, by=method[1], decreasing=TRUE)
  keyness
})



#' @docType methods
#' @rdname keyness-method
setMethod("keyness", signature=c(x="partitionBundle"), function(
  x, y, pAttribute=NULL,
  minFrequency=1, included=FALSE, method="chiSquare", verbose=TRUE, mc=TRUE, progress=FALSE
) {
  if (is.null(pAttribute)) pAttribute <- slot(get("session", ".GlobalEnv"), "pAttribute")
  kclust <- new("keynessBundle")
  .keyness <- function(a) {
    keyness(a, y, pAttribute=pAttribute, minFrequency=minFrequency, included=included, method=method, verbose=verbose)
  }
  if (mc == FALSE){
    if (progress == FALSE){
      kclust@objects <- lapply(setNames(x@objects, names(x@objects)), function(a) .keyness(a))  
    } else {
      tmp <- lapply(
        c(1:length(x@objects)),
        function(i) {
          .progressBar(i, length(x@objects))
          .keyness(x@objects[[i]])
          })
      names(tmp) <- names(x@objects)
      kclust@objects <- tmp
    }
  } else if (mc == TRUE){
    kclust@objects <- mclapply(
      setNames(x@objects, names(x@objects)),
      function(a) .keyness(a),
      mc.cores=slot(get("session", '.GlobalEnv'), 'multicore')
      )
  }
  kclust
})


#' @importFrom plyr ddply
#' @rdname keyness-method
setMethod("keyness", "cooccurrences", function(x,y, included=FALSE, method="ll", mc=TRUE, verbose=TRUE){
  newObject <- new(
    'keynessCooccurrences',
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
    "ll", "i.ll", "exp_coi", "i.exp_coi", "rank", "i.rank",
    "window_size", "i.window_size", "a_count", "i.a_count", "b_count", "i.b_count"
    )
  for (drop in colsToDrop) MATCH[, eval(drop) := NULL, with=TRUE]
  setnames(MATCH, old=c("ab_count", "i.ab_count"), new=c("y_ab_count", "x_ab_count"))
  
  if (included == TRUE) MATCH[, y_ab_count := y_ab_count - x_ab_count]
  
  newObject@stat <- MATCH
  if ("chiSquare" %in% method) {
    if (verbose==TRUE) message("... computing chisquare tests")
    newObject <- chisquare(newObject)
  }
  if ("ll" %in% method) {
    if (verbose==TRUE) message("... computing log likelihood tests")
    newObject <- ll(newObject, partitionSize=newObject@sizeRef)
  }
  if (verbose == TRUE) message("... trimming the object")
  # newObject <- sort(newObject, by=method[1])
  newObject
})

#' @rdname keyness-method
setMethod("keyness", "missing", function(){
  .getClassObjectsAvailable(".GlobalEnv", "keyness")
})

setMethod(
  "keyness", "ngrams",
  function(x, y, included=FALSE, method="chisquare", verbose=TRUE, ...){
    stopifnot(
      identical(x@pAttribute, y@pAttribute) == TRUE,
      x@n == y@n,
      method %in% c("chisquare")
    )
    tokenColnames <- paste("token_", c(1:x@n), sep="")
    setkeyv(x@stat, cols=tokenColnames)
    setkeyv(y@stat, cols=tokenColnames)
    DT <- y@stat[x@stat]
    setnames(DT, c("count", "i.count"), c("count_x", "count_y"))
    setcolorder(DT, c(tokenColnames, "count_x", "count_y"))
    
  })

