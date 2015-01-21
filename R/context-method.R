#' @include partition-class.R partitionCluster-class.R
NULL


##################################################################
#                                                                #
#  Methods that can be applied to objects of the class 'context' #
#                                                                #
##################################################################



#' @param object a partition or a partitionCluster object
#' @param ... further arguments
#' @exportMethod context
#' @docType methods
#' @noRd
setGeneric("context", function(object, ...){standardGeneric("context")})

#' Analyze context of a node word
#' 
#' Retrieve the concordances of a token and calculate the log-likelihood test
#' for collocates For formulating the query, CPQ syntax may be used (see
#' examples).
#' 
#' @param object a partition or a partitionCluster object
#' @param query query, which may by a character vector or a cqpQuery object
#' @param pAttribute p-attribute of the query
#' @param leftContext no of tokens and to the left of the node word
#' @param rightContext no of tokens to the right of the node word
#' @param minSignificance minimum log-likelihood value
#' @param posFilter character vector with the POS tags to be included - may not
#'   be empty!!
#' @param filterType either "include" or "exclude"
#' @param stoplist exclude a query hit from analysis if stopword(s) is/are in
#'   context
#' @param positivelist character vector or numeric vector: include a query hit
#'   only if token in positivelist is present. If positivelist is a character
#'   vector, it is assumed to provide regex expressions (incredibly long if the
#'   list is long)
#' @param statisticalTest either "LL" (default) or "pmi", if NULL, calculating
#'   the statistics will be skipped
#' @param mc whether to use multicore; if NULL (default), the function will get
#'   the value from the session settings
#' @param verbose report progress, defaults to TRUE
#' @return depending on whether a partition or a partitionCluster serves as
#'   input, the return will be a context object, or a contextCluster object
#' @author Andreas Blaette
#' @aliases context context,partition-method as.matrix,contextCluster-method
#'   as.TermContextMatrix,contextCluster-method context,contextCluster-method
#'   context,partitionCluster-method ll ll-method context,collocations-method
#'   context,collocations-method
#' @examples
#' \dontrun{
#' p <- partition("PLPRBTTXT", list(text_type="speech"))
#' a <- context(p, "Integration", "word")
#' }
#' @importFrom parallel mclapply
#' @exportMethod context
#' @rdname context
#' @docType methods
#' @aliases context,partition-method
setMethod(
  f="context",
  signature(object="partition"),
  function
  (
    object,
    query,
    pAttribute=NULL,
    leftContext=0,
    rightContext=0,
    minSignificance=0,
    posFilter=NULL,
    filterType="exclude",
    stoplist=NULL,
    positivelist=NULL,
    statisticalTest="ll",
    mc=NULL,
    verbose=TRUE
  ) {
    if (is.null(pAttribute)) pAttribute <- slot(get("session", '.GlobalEnv'), 'pAttribute')
    if (!pAttribute %in% names(object@tf) && !is.null(statisticalTest)) {
      if (verbose==TRUE) message("... required tf list in partition not yet available: doing this now")
      object <- enrich(object, tf=pAttribute)
    }
    if (leftContext == 0) leftContext <- slot(get("session", '.GlobalEnv'), 'leftContext')
    if (rightContext == 0) rightContext <- slot(get("session", '.GlobalEnv'), 'rightContext')
    if (minSignificance == -1) minSignificance <- slot(get("session", '.GlobalEnv'), 'minSignificance')
    if (is.null(mc)) mc <- slot(get("session", '.GlobalEnv'), 'multicore')
    ctxt <- new(
      "context",
      query=query,
      pAttribute=pAttribute,
      corpus=object@corpus,
      leftContext=leftContext,
      rightContext=rightContext,
      encoding=object@encoding,
      posFilter=as.character(posFilter),
      partition=object@label,
      partitionSize=object@size,
      call=deparse(match.call())
    )
    corpus.pattr <- paste(ctxt@corpus,".", pAttribute, sep="")
    corpus.sattr <- paste(ctxt@corpus,".text_id", sep="")
    if (verbose==TRUE) message("... getting counts for query in partition", appendLF=FALSE)
    # query <- .adjustEncoding(query, object@encoding)
    # Encoding(query) <- ctxt@encoding
    hits <- .queryCpos(ctxt@query, object, pAttribute)
    hits <- cbind(hits, cqi_cpos2struc(corpus.sattr, hits[,1]))
    hits <- apply(hits, 1, function(x) as.list(unname(x)))
    
    if (verbose==TRUE) message(' (', length(hits), " occurrences)")
    stoplistIds <- unlist(lapply(stoplist, function(x) cqi_regex2id(corpus.pattr, x)))
    if (is.numeric(positivelist)){
      positivelistIds <- positivelist
    } else {
      positivelistIds <- unlist(lapply(positivelist, function(x) cqi_regex2id(corpus.pattr, x)))
    }
    if (verbose==TRUE) message("... counting tokens in context ")  
    if (mc==TRUE) {
      bigBag <- mclapply(hits, function(x) .surrounding(x, ctxt, corpus.sattr, filterType, stoplistIds, positivelistIds))
    } else {
      bigBag <- lapply(hits, function(x) .surrounding(x, ctxt, corpus.sattr, filterType, stoplistIds, positivelistIds))
    }
    bigBag <- bigBag[!sapply(bigBag, is.null)]
    if (!is.null(stoplistIds) || !is.null(positivelistIds)){
      if (verbose==TRUE) message("... hits filtered because stopword(s) occur / elements of positive list do not in context: ", (length(hits)-length(bigBag)))
    }
    ctxt@cpos <- lapply(bigBag, function(x) x$cpos)
    ctxt@size <- length(unlist(lapply(bigBag, function(x) unname(unlist(x$cpos)))))
    if (verbose==TRUE) message('... context size: ', ctxt@size)
    ctxt@frequency <- length(bigBag)
    # if statisticalTest is 'NULL' the following can be ommitted
    if (!is.null(statisticalTest)){
      wc <- table(unlist(lapply(bigBag, function(x) x$id)))
      ctxt@stat <- data.frame(
        id=as.integer(names(wc)),
        countCoi=as.integer(unname(wc))
        )
      ctxt@stat$countCorpus <- object@tf[[pAttribute]][match(ctxt@stat[,"id"], object@tf[[pAttribute]][,1]),2]
      rownames(ctxt@stat) <- cqi_id2str(corpus.pattr, ctxt@stat[,"id"])
      Encoding(rownames(ctxt@stat)) <- object@encoding
      if ("ll" %in% statisticalTest){
        if (verbose==TRUE) message("... performing log likelihood test")
        # calc <- .g2Statistic(as.integer(names(wc)), unname(wc), ctxt@size, object, pAttribute)
        ctxt <- ll(ctxt, object)
      }
      if ("pmi" %in% statisticalTest){
        if (verbose==TRUE) message("... calculating pointwise mutual information")
        ctxt <- pmi(ctxt)
      }
      if ("t" %in% statisticalTest){
        ctxt@stat$countCooc <- table(unlist(lapply(bigBag, function(x) unique(x$id))))
        if (verbose==TRUE) message("... calculating t-test")
        ctxt <- tTest(ctxt, object)
      }
      ctxt@stat <- data.frame(
        rank=1:nrow(ctxt@stat),
        ctxt@stat
      )
      ctxt <- trim(ctxt, minSignificance=minSignificance, rankBy=ctxt@statisticalTest[1])
    }
    ctxt
  })

#' @param set a numeric vector with three items: left cpos of hit, right cpos of hit, struc of the hit
#' @param leftContext no of tokens to the left
#' @param rightContext no of tokens to the right
#' @param sAttribute the integrity of the sAttribute to be checked
#' @return a list!
#' @noRd
.leftRightContextChecked <- function(set, leftContext, rightContext, sAttribute){
  cposLeft <- c((set[1] - leftContext):(set[1]-1))
  cposLeft <- cposLeft[which(cqi_cpos2struc(sAttribute, cposLeft)==set[3])]
  cposRight <- c((set[2] + 1):(set[2] + rightContext))
  cposRight <- cposRight[which(cqi_cpos2struc(sAttribute, cposRight)==set[3])]
  return(list(left=cposLeft, node=c(set[1]:set[2]), right=cposRight))
}

.surrounding <- function (set, ctxt, corpus.sattr, filterType, stoplistIds=NULL, positivelistIds=NULL) {
  set <- as.numeric(set)
  cposList <- .leftRightContextChecked(
    set,
    leftContext=ctxt@leftContext,
    rightContext=ctxt@rightContext,
    sAttribute=corpus.sattr
    )
  cpos <- c(cposList$left, cposList$right)
  posChecked <- cpos[.filter[[filterType]](cqi_cpos2str(paste(ctxt@corpus,".pos", sep=""), cpos), ctxt@posFilter)]
  id <- cqi_cpos2id(paste(ctxt@corpus,".", ctxt@pAttribute, sep=""), posChecked)
  if (!is.null(stoplistIds) || !is.null(positivelistIds)) {
    ids <- cqi_cpos2id(paste(ctxt@corpus,".", ctxt@pAttribute, sep=""), cpos)
    if (!is.null(stoplistIds)) if (any(stoplistIds %in% ids)) {bag <- NULL}
    if (!is.null(positivelistIds)) if (any(positivelistIds %in% ids) == FALSE) {bag <- NULL}
  }
  return(list(cpos=cposList, id=id))
}

#' @docType methods
setMethod("context", "partitionCluster", function(
  object, query, pAttribute="useControls",
  leftContext=0, rightContext=0,
  minSignificance=-1, posFilter="useControls", filterType="useControls",
  stoplist=c(), statisticalTest="ll",
  verbose=TRUE  
) {
  contextCluster <- new("contextCluster")
  contextCluster@query <- query
  contextCluster@pAttribute <- pAttribute
  contextCluster@contexts <- sapply(
    partitionCluster@partitions,
    function(x) context(
      query, x,
      pAttribute=pAttribute,
      leftContext=leftContext, rightContext=rightContext,
      minSignificance=minSignificance, posFilter=posFilter, filterType=filterType,
      stoplist=stoplist, statisticalTest=statisticalTest,
      verbose=verbose
    ),
    simplify = TRUE,
    USE.NAMES = TRUE
  )
  contextCluster
})

setMethod("context", "collocations", function(object, query, complete=FALSE){
  newObject <- new(
    "context",
    query=query,
    partition=object@partition,
    partitionSize=object@partitionSize,
    leftContext=object@leftContext,
    rightContext=object@rightContext,
    pAttribute=object@pAttribute,
    corpus=object@corpus,
    encoding=object@encoding,
    posFilter=object@posFilter,
    statisticalTest=object@method,
    cutoff=object@cutoff,
    stat=subset(object@stat, node==query),
    call=deparse(match.call()),
    size=unique(subset(object@stat, node==query)[,"windowSize"])
  )  
  if (complete == TRUE){
    sAttr <- paste(
      newObject@corpus, ".",
      names(get(newObject@partition, ".GlobalEnv")@sAttributes)[[1]],
      sep=""
      )
    hits <- .queryCpos(
      newObject@query,
      get(newObject@partition, ".GlobalEnv"),
      pAttribute=newObject@pAttribute,
      verbose=FALSE
      )
    newObject@size <- nrow(hits)
    hits <- cbind(hits, cqi_cpos2struc(sAttr, hits[,1]))
    newObject@cpos <- apply(
      hits, 1, function(row) {
        .leftRightContextChecked(
          row,
          leftContext=newObject@leftContext,
          rightContext=newObject@rightContext,
          sAttribute=sAttr
          )
      }    
    )
  }
  return(newObject)
})

#' @rdname context
setMethod("context", "missing", function(){
  .getClassObjectsAvailable(".GlobalEnv", "context")
})


