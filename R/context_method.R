#' @include partition_class.R partitionBundle_class.R
NULL



#' @param .Object a partition or a partitionBundle object
#' @param ... further arguments
#' @exportMethod context
#' @docType methods
#' @noRd
setGeneric("context", function(.Object, ...) standardGeneric("context") )

#' Analyze context of a node word
#' 
#' Retrieve the word context of a token, checking for the boundaries of a XML
#' region. For formulating the query, CPQ syntax may be used (see
#' examples). Statistical tests available are log-likelihood, t-test, pmi.
#' 
#' @param .Object a partition or a partitionBundle object
#' @param query query, which may by a character vector or a cqpQuery object
#' @param cqp defaults to is.cqp-function, or provide TRUE/FALSE
#' @param pAttribute p-attribute of the query
#' @param sAttribute if provided, it will be checked that cpos do not extend beyond
#' the region defined by the s-attribute 
#' @param left no of tokens and to the left of the node word
#' @param right no of tokens to the right of the node word
#' @param stoplist exclude a query hit from analysis if stopword(s) is/are in
#'   context
#' @param positivelist character vector or numeric vector: include a query hit
#'   only if token in positivelist is present. If positivelist is a character
#'   vector, it is assumed to provide regex expressions (incredibly long if the
#'   list is long)
#' @param count logical
#' @param method either "LL" (default) or "pmi", if NULL, calculating
#'   the statistics will be skipped
#' @param mc whether to use multicore; if NULL (default), the function will get
#'   the value from the options
#' @param verbose report progress, defaults to TRUE
#' @param progress logical, whether to show progress bar
#' @param ... further parameters
#' @return depending on whether a partition or a partitionBundle serves as
#'   input, the return will be a context object, or a contextBundle object
#' @author Andreas Blaette
#' @aliases context context,partition-method as.matrix,contextBundle-method
#'   context,contextBundle-method
#'   context,partitionBundle-method context,cooccurrences-method
#'   context,cooccurrences-method
#' @examples
#' \dontrun{
#'   use("polmineR.sampleCorpus")
#'   p <- partition("PLPRBTTXT", list(text_type="speech"))
#'   a <- context(p, query="Integration", pAttribute="word")
#' }
#' @import data.table
#' @exportMethod context
#' @rdname context-method
#' @name context
#' @docType methods
#' @aliases context,partition-method
setMethod(
  f="context",
  signature(.Object="partition"),
  function
  (
    .Object, query, cqp=is.cqp,
    pAttribute = getOption("polmineR.pAttribute"),
    sAttribute = NULL,
    left = getOption("polmineR.left"),
    right = getOption("polmineR.right"),
    stoplist = NULL, positivelist = NULL,
    count = TRUE,
    method = "ll",
    mc = getOption("polmineR.mc"),
    verbose = TRUE,
    progress = FALSE
  ) {
    if (!identical(.Object@pAttribute, pAttribute) && !is.null(method)){
      message("... count for pAttribute ", pAttribute, " not available")
      .Object <- enrich(.Object, pAttribute = pAttribute)
    }

    # set method for making left and right context
    if (is.numeric(left) && is.numeric(right)){
      if (is.null(names(left)) && is.null(names(left))){
        cposMethod <- "expandToCpos"
      } else {
        cposMethod <- "expandBeyondRegion"
        sAttribute <- unique(c(names(left), names(right)))
      }
    } else if (is.character(left) && is.character(right)){
      cposMethod <- "expandToRegion"
      sAttribute <- unique(c(left, right))
    }
    
    # instantiate the context object
    ctxt <- new(
      "context",
      query = query, pAttribute = pAttribute,
      stat = data.table(),
      corpus = .Object@corpus,
      left = ifelse(is.character(left), 0, left),
      right = ifelse(is.character(right), 0, right),
      encoding = .Object@encoding, 
      partition = .Object@name,
      partitionSize = as.numeric(.Object@size)
    )
    if (!is.null(sAttribute)) ctxt@sAttribute <- sAttribute
    ctxt@call <- deparse(match.call())
    
    # getting counts of query in partition
    .verboseOutput(message="getting cpos", verbose = verbose)
    hits <- cpos(.Object, query, pAttribute[1], cqp=cqp)
    if (is.null(hits)){
      warning('no hits for query ', query, ' returning NULL object')
      return(NULL)
    }
    if (!is.null(sAttribute)) hits <- cbind(hits, CQI$cpos2struc(.Object@corpus, sAttribute, hits[,1]))
    hits <- lapply(c(1: nrow(hits)), function(i) hits[i,])

    # generate positivelist, negativelist
    stoplistIds <- unlist(lapply(stoplist, function(x) CQI$regex2id(.Object@corpus, pAttribute, x)))
    if (is.numeric(positivelist)){
      positivelistIds <- positivelist
      if (verbose == TRUE) message("... using ids provided as positivelist")
    } else {
      positivelistIds <- unlist(lapply(positivelist, function(x) CQI$regex2id(.Object@corpus, pAttribute, x)))
    }
    
    .verboseOutput(message="generating contexts", verbose = verbose)
    
    bigBag <- blapply(
      hits, f=.surrounding,
      mc=mc, progress=progress, verbose=verbose,
      ctxt=ctxt, left=left, right=right, corpus=.Object@corpus, sAttribute=sAttribute,
      stoplistIds=stoplistIds, positivelistIds=positivelistIds, method=cposMethod
      )
    
    bigBag <- bigBag[!sapply(bigBag, is.null)] # remove empty contexts
    if (!is.null(stoplistIds) || !is.null(positivelistIds)){
      if (verbose==TRUE) message("... hits filtered because stopword(s) occur / elements of positive list do not in context: ", (length(hits)-length(bigBag)))
    }
    ctxt@cpos <- lapply(bigBag, function(x) x$cpos)
    ctxt@size <- length(unlist(lapply(bigBag, function(x) unname(unlist(x$cpos)))))
    ctxt@sizeCoi <- as.integer(ctxt@size)
    ctxt@sizeRef <- as.integer(ctxt@partitionSize - ctxt@sizeCoi)
    ctxt@count <- length(bigBag)
    
    # put together raw stat table
    if (count == TRUE || length(method) > 0){
      .verboseOutput(message="counting tokens", verbose = verbose)
      idList <- lapply(
        c(1:length(pAttribute)),
        function(i) unlist(lapply(bigBag, function(x) x$ids[[i]]))
      )
      names(idList) <- pAttribute
      ID <- as.data.table(idList)
      setkeyv(ID, pAttribute)
      count <- function(x) return(x)
      ctxt@stat <- ID[, count(.N), by=c(eval(pAttribute)), with=TRUE]
      for (i in c(1:length(pAttribute))){
        ctxt@stat[, eval(pAttribute[i]) := as.utf8(CQI$id2str(.Object@corpus, pAttribute[i], ctxt@stat[[pAttribute[i]]]))]
      }
      setnames(ctxt@stat, "V1", "count_window")
      setkeyv(ctxt@stat, pAttribute)
    }
    
    # statistical tests
    if (!is.null(method)){
      ctxt@stat[, "count_partition" := merge(ctxt@stat, .Object@stat, all.x=TRUE, all.y=FALSE)[["count"]]]
      for (test in method){
        .verboseOutput(message = paste("statistical test:", test), verbose = verbose)
        ctxt <- do.call(test, args = list(.Object=ctxt))  
      }
      colnamesOld <- colnames(ctxt@stat)
    }
    ctxt
  })



#' @param set a numeric vector with three items: left cpos of hit, right cpos of hit, struc of the hit
#' @param left no of tokens to the left
#' @param right no of tokens to the right
#' @param sAttribute the integrity of the sAttribute to be checked
#' @return a list!
#' @noRd
.makeLeftRightCpos <- list(
  
  "expandToCpos" = function(set, left, right, corpus, sAttribute){
    cposLeft <- c((set[1] - left):(set[1]-1))
    cposRight <- c((set[2] + 1):(set[2] + right))
    if (!is.null(sAttribute)){
      cposLeft <- cposLeft[which(CQI$cpos2struc(corpus, sAttribute, cposLeft) == set[3])]
      cposRight <- cposRight[which(CQI$cpos2struc(corpus, sAttribute, cposRight)==set[3])]   
    }
    return(list(left=cposLeft, node=c(set[1]:set[2]), right=cposRight))
  },
  
  "expandToRegion" = function(set, left, right, corpus, sAttribute){
    cposLeft <- c((CQI$cpos2lbound(corpus, sAttribute, set[1])):(set[1] - 1))
    cposRight <- c((set[2] + 1):(CQI$cpos2rbound(corpus, sAttribute, set[1])))
    return(list(left=cposLeft, node=c(set[1]:set[2]), right=cposRight))
  },
  
  "expandBeyondRegion" = function(set, left, right, corpus, sAttribute){
    queryStruc <- CQI$cpos2struc(corpus, sAttribute, set[1])
    maxStruc <- CQI$attribute_size(corpus, sAttribute)
    # get left min cpos
    leftStruc <- queryStruc - left
    leftStruc <- ifelse(leftStruc < 0, 0, leftStruc)
    leftCposMin <- CQI$struc2cpos(corpus, sAttribute, leftStruc)[1]
    cposLeft <- c(leftCposMin:(set[1]-1))
    # get right max cpos
    rightStruc <- queryStruc + right
    rightStruc <- ifelse(rightStruc > maxStruc - 1, maxStruc, rightStruc)
    rightCposMax <- CQI$struc2cpos(corpus, sAttribute, rightStruc)[2]
    cposRight <- c((set[2] + 1):rightCposMax)
    # handing it back
    return(list(left=cposLeft, node=c(set[1]:set[2]), right=cposRight))
  }
  
)

.surrounding <- function (set, ctxt, left, right, corpus, sAttribute, stoplistIds=NULL, positivelistIds=NULL, method, ...) {
  cposList <- .makeLeftRightCpos[[method]](
    set, left=left, right=right,
    corpus=corpus, sAttribute=sAttribute
    )
  cpos <- c(cposList$left, cposList$right)
  ids <- lapply(
    ctxt@pAttribute,
    function(pAttr) CQI$cpos2id(ctxt@corpus, pAttr, cpos) 
    )
  
  if (!is.null(stoplistIds) || !is.null(positivelistIds)) {
    exclude <- FALSE
    if (!is.null(stoplistIds)) if (any(stoplistIds %in% ids[[1]])) {exclude <- TRUE}
    if (!is.null(positivelistIds)) {
      if (any(positivelistIds %in% ids[[1]]) == FALSE) { exclude <- TRUE }
    }
  } else { 
    exclude <- FALSE
  }
  if (exclude == TRUE){
    retval <- NULL
  } else {
    retval <- list(cpos=cposList, ids=ids)
  }
  return(retval)
}


#' @rdname context-method
setMethod("context", "character", function(.Object, ...) context(partition(.Object), ...))

#' @rdname context-method
setMethod("context", "Corpus", function(.Object, pAttribute = getOption("polmineR.pAttribute"), ...){
  if (nrow(.Object$stat) == 0) .Object$count(pAttribute)
  context(.Object$as.partition(), pAttribute = pAttribute, ...)
})

#' @docType methods
#' @rdname context-method
setMethod("context", "partitionBundle", function(.Object, query, verbose=TRUE, ...){
  contextBundle <- new("contextBundle", query=query, pAttribute=pAttribute)
  if (!is.numeric(positivelist)){
    # corpus.pAttribute <- paste(
    #   unique(lapply(.Object@objects, function(x) x@corpus)),
    #   ".", pAttribute, sep=""
    #   )
    corpus <- unique(lapply(.Object@objects, function(x) x@corpus))
    positivelist <- unlist(lapply(positivelist, function(x) CQI$regex2id(corpus, pAttribute, x)))
  }
  
  contextBundle@objects <- sapply(
    .Object@objects,
    function(x) {
      if (verbose == TRUE) message("... proceeding to partition ", x@name)
      context(x, query, ...)
      },
    simplify = TRUE,
    USE.NAMES = TRUE
  )
  contextBundle
})

#' @param complete enhance completely
#' @rdname context-method
setMethod("context", "cooccurrences", function(.Object, query, complete=FALSE){
  newObject <- new(
    "context",
    query=query,
    partition=.Object@partition,
    partitionSize=.Object@partitionSize,
    left=.Object@left,
    right=.Object@right,
    pAttribute=.Object@pAttribute,
    corpus=.Object@corpus,
    encoding=.Object@encoding,
    method=.Object@method,
    stat=subset(.Object@stat, .Object@stat[, "node"]==query),
    call=deparse(match.call()),
    size=unique(subset(.Object@stat, .Object@stat[, "node"]==query)[,"size_window"])
  )  
  if (complete == TRUE){
    sAttribute <- names(get(newObject@partition, ".GlobalEnv")@sAttributes)[[1]]
    sAttr <- paste(
      newObject@corpus, ".",
      names(get(newObject@partition, ".GlobalEnv")@sAttributes)[[1]],
      sep=""
      )
    hits <- cpos(
      newObject@query,
      get(newObject@partition, ".GlobalEnv"),
      pAttribute=newObject@pAttribute,
      verbose=FALSE
      )
    newObject@size <- nrow(hits)
    hits <- cbind(hits, CQI$cpos2struc(newObject@corpus, sAttribute, hits[,1]))
    newObject@cpos <- apply(
      hits, 1, function(row) {
        .makeLeftRightCpos[["expandToCpos"]](
          row,
          left=newObject@left,
          right=newObject@right,
          sAttribute=sAttr
          )
      }    
    )
  }
  return(newObject)
})
