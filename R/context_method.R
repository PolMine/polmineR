#' @include partition_class.R partitionBundle_class.R
NULL



#' @param .Object a partition or a partitionBundle object
#' @param ... further arguments
#' @exportMethod context
#' @docType methods
#' @noRd
setGeneric("context", function(.Object, ...){standardGeneric("context")})

#' Analyze context of a node word
#' 
#' Retrieve the word context of a token, checking for the boundaries of a XML
#' region. For formulating the query, CPQ syntax may be used (see
#' examples). Statistical tests available are log-likelihood, t-test, pmi.
#' 
#' @param .Object a partition or a partitionBundle object
#' @param query query, which may by a character vector or a cqpQuery object
#' @param pAttribute p-attribute of the query
#' @param sAttribute if provided, it will be checked that cpos do not extend beyond
#' the region defined by the s-attribute 
#' @param leftContext no of tokens and to the left of the node word
#' @param rightContext no of tokens to the right of the node word
#' @param stoplist exclude a query hit from analysis if stopword(s) is/are in
#'   context
#' @param positivelist character vector or numeric vector: include a query hit
#'   only if token in positivelist is present. If positivelist is a character
#'   vector, it is assumed to provide regex expressions (incredibly long if the
#'   list is long)
#' @param method either "LL" (default) or "pmi", if NULL, calculating
#'   the statistics will be skipped
#' @param mc whether to use multicore; if NULL (default), the function will get
#'   the value from the session settings
#' @param verbose report progress, defaults to TRUE
#' @return depending on whether a partition or a partitionBundle serves as
#'   input, the return will be a context object, or a contextBundle object
#' @author Andreas Blaette
#' @aliases context context,partition-method as.matrix,contextBundle-method
#'   as.TermContextMatrix,contextBundle-method context,contextBundle-method
#'   context,partitionBundle-method ll ll-method context,cooccurrences-method
#'   context,cooccurrences-method
#' @examples
#' \dontrun{
#' p <- partition("PLPRBTTXT", list(text_type="speech"))
#' a <- context(p, "Integration", "word")
#' }
#' @importFrom parallel mclapply
#' @importFrom rcqp cqi_cpos2lbound cqi_cpos2rbound
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
    .Object, query, pAttribute=NULL, sAttribute="text_id",
    leftContext=NULL, rightContext=NULL,
    stoplist=NULL, positivelist=NULL,
    method="ll",
    mc=NULL, verbose=TRUE
  ) {
    if (is.null(pAttribute)) {
      pAttribute <- slot(get("session", '.GlobalEnv'), 'pAttribute')
      message("param pAttribute not state, using session setting: ", pAttribute)
    }
    if (!identical(.Object@pAttribute, pAttribute)){
      message("... tf for pAttribute ", pAttribute, " not available, enriching object now")
      .Object <- enrich(.Object, pAttribute=pAttribute)
    }
    # get values from session object if params are not provided
    if (is.null(leftContext)) leftContext <- slot(get("session", '.GlobalEnv'), 'leftContext')
    if (is.null(rightContext)) rightContext <- slot(get("session", '.GlobalEnv'), 'rightContext')
    if (is.null(mc)) mc <- slot(get("session", '.GlobalEnv'), 'multicore')
    
    pAttr <- paste(.Object@corpus, ".", pAttribute, sep="")
    sAttr <- .setMethod(leftContext, rightContext, sAttribute, corpus=.Object@corpus)[1]
    cposMethod <- .setMethod(leftContext, rightContext, sAttribute, corpus=.Object@corpus)[2]
    
    # instantiate the context object
    ctxt <- new(
      "context",
      query=query, pAttribute=pAttribute, stat=data.table(),
      sAttribute=sAttribute, corpus=.Object@corpus,
      leftContext=ifelse(is.character(leftContext), 0, leftContext),
      rightContext=ifelse(is.character(rightContext), 0, rightContext),
      encoding=.Object@encoding, 
      partition=.Object@name, partitionSize=.Object@size
    )
    ctxt@call <- deparse(match.call())
    
    # getting counts of query in partition
    if (verbose==TRUE) message("... getting counts for query in partition", appendLF=FALSE)
    hits <- cpos(.Object, query, pAttribute[1])
    if (is.null(hits)){
      if (verbose==TRUE) message(' -> no hits')
      return(NULL)
    }
    if (!is.null(sAttribute)) hits <- cbind(hits, cqi_cpos2struc(sAttr, hits[,1]))
    hits <- lapply(c(1: nrow(hits)), function(i) hits[i,])
    if (verbose==TRUE) message(': ', length(hits))
    
    # generate positivelist, negativelist
    stoplistIds <- unlist(lapply(stoplist, function(x) cqi_regex2id(pAttr, x)))
    if (is.numeric(positivelist)){
      positivelistIds <- positivelist
      if (verbose == TRUE) message("... using ids provided as positivelist")
    } else {
      positivelistIds <- unlist(lapply(positivelist, function(x) cqi_regex2id(pAttr, x)))
    }
    
    
    if (verbose==TRUE) message("... counting tokens in context ")  
    if (mc==TRUE) {
      bigBag <- mclapply(hits, function(x) .surrounding(x, ctxt, leftContext, rightContext, sAttr, stoplistIds, positivelistIds, cposMethod))
    } else {
      bigBag <- lapply(hits, function(x) .surrounding(x, ctxt, leftContext, rightContext, sAttr, stoplistIds, positivelistIds, cposMethod))
    }
    bigBag <- bigBag[!sapply(bigBag, is.null)] # remove empty contexts
    if (!is.null(stoplistIds) || !is.null(positivelistIds)){
      if (verbose==TRUE) message("... hits filtered because stopword(s) occur / elements of positive list do not in context: ", (length(hits)-length(bigBag)))
    }
    ctxt@cpos <- lapply(bigBag, function(x) x$cpos)
    ctxt@size <- length(unlist(lapply(bigBag, function(x) unname(unlist(x$cpos)))))
    ctxt@sizeCoi <- as.integer(ctxt@size)
    ctxt@sizeRef <- as.integer(ctxt@partitionSize - ctxt@sizeCoi)
    if (verbose==TRUE) message('... window size total: ', ctxt@size)
    ctxt@count <- length(bigBag)
    
    # put together raw stat table
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
      ctxt@stat[, eval(pAttribute[i]) := cqi_id2str(pAttr[i], ctxt@stat[[pAttribute[i]]]) %>% as.utf8()]
    }
    setnames(ctxt@stat, "V1", "count_window")
    setkeyv(ctxt@stat, pAttribute)
    
    # statistical tests
    if (!is.null(method)){
      ctxt@stat[,count_partition := merge(ctxt@stat, .Object@stat, all.x=TRUE, all.y=FALSE)[["count"]]]
      for (test in method){
        if (verbose == TRUE) message("... statistical test: ", test)
        ctxt <- do.call(test, args=list(.Object=ctxt))  
      }
      colnamesOld <- colnames(ctxt@stat)
    }
    ctxt
  })


.setMethod <- function(leftContext, rightContext, sAttribute, corpus){
  if (is.numeric(leftContext) && is.numeric(rightContext)){
    if (is.null(names(leftContext)) && is.null(names(leftContext))){
      method <- "expandToCpos"
      if (!is.null(sAttribute)) {
        corpus.sAttribute <- paste(corpus, ".", sAttribute, sep="")
      } else {
        corpus.sAttribute <- NA
      }
    } else {
      method <- "expandBeyondRegion"
      sAttribute <- unique(c(names(leftContext), names(rightContext)))
      if (length(sAttribute) == 1){
        corpus.sAttribute <- paste(corpus, ".", sAttribute, sep="")
      } else {
        warning("please check names of left and right context provided")
      }
    }
  } else if (is.character(leftContext) && is.character(rightContext)){
    method <- "expandToRegion"
    sAttribute <- unique(c(leftContext, rightContext))
    if (length(sAttribute) == 1){
      corpus.sAttribute <- paste(corpus, ".", sAttribute, sep="")
    } else {
      warning("please check names of left and right context provided")
    }
  }
  return(c(corpus.sAttribute, method))
}




#' @param set a numeric vector with three items: left cpos of hit, right cpos of hit, struc of the hit
#' @param leftContext no of tokens to the left
#' @param rightContext no of tokens to the right
#' @param sAttribute the integrity of the sAttribute to be checked
#' @return a list!
#' @noRd
.makeLeftRightCpos <- list(
  
  "expandToCpos" = function(set, leftContext, rightContext, corpus.sAttribute){
    cposLeft <- c((set[1] - leftContext):(set[1]-1))
    cposRight <- c((set[2] + 1):(set[2] + rightContext))
    if (!is.na(corpus.sAttribute)){
      cposLeft <- cposLeft[which(cqi_cpos2struc(corpus.sAttribute, cposLeft)==set[3])]
      cposRight <- cposRight[which(cqi_cpos2struc(corpus.sAttribute, cposRight)==set[3])]   
    }
    return(list(left=cposLeft, node=c(set[1]:set[2]), right=cposRight))
  },
  
  "expandToRegion" = function(set, leftContext, rightContext, corpus.sAttribute){
    cposLeft <- c((cqi_cpos2lbound(corpus.sAttribute, set[1])):(set[1] - 1))
    cposRight <- c((set[2] + 1):(cqi_cpos2rbound(corpus.sAttribute, set[1])))
    return(list(left=cposLeft, node=c(set[1]:set[2]), right=cposRight))
  },
  
  "expandBeyondRegion" = function(set, leftContext, rightContext, corpus.sAttribute){
    queryStruc <- cqi_cpos2struc(corpus.sAttribute, set[1])
    maxStruc <- cqi_attribute_size(corpus.sAttribute)
    # get left min cpos
    leftStruc <- queryStruc - leftContext
    leftStruc <- ifelse(leftStruc < 0, 0, leftStruc)
    leftCposMin <- cqi_struc2cpos(corpus.sAttribute, leftStruc)[1]
    cposLeft <- c(leftCposMin:(set[1]-1))
    # get right max cpos
    rightStruc <- queryStruc + rightContext
    rightStruc <- ifelse(rightStruc > maxStruc - 1, maxStruc, rightStruc)
    rightCposMax <- cqi_struc2cpos(corpus.sAttribute, rightStruc)[2]
    cposRight <- c((set[2] + 1):rightCposMax)
    # handing it back
    return(list(left=cposLeft, node=c(set[1]:set[2]), right=cposRight))
  }
  
)

.surrounding <- function (set, ctxt, leftContext, rightContext, corpus.sAttribute, stoplistIds=NULL, positivelistIds=NULL, method) {
  cposList <- .makeLeftRightCpos[[method]](
    set,
    leftContext=leftContext,
    rightContext=rightContext,
    corpus.sAttribute=corpus.sAttribute
    )
  cpos <- c(cposList$left, cposList$right)
  ids <- lapply(
    ctxt@pAttribute,
    function(pAttr) cqi_cpos2id(paste(ctxt@corpus,".", pAttr, sep=""), cpos) 
    )
  
  if (!is.null(stoplistIds) || !is.null(positivelistIds)) {
    exclude <- FALSE
    # ids <- cqi_cpos2id(paste(ctxt@corpus,".", ctxt@pAttribute, sep=""), cpos)
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


#' @docType methods
#' @rdname context-method
setMethod("context", "partitionBundle", function(.Object, query, ...){
  contextBundle <- new("contextBundle", query=query, pAttribute=pAttribute)
  if (!is.numeric(positivelist)){
    corpus.pAttribute <- paste(
      unique(lapply(.Object@objects, function(x) x@corpus)),
      ".", pAttribute, sep=""
      )
    positivelist <- unlist(lapply(positivelist, function(x) cqi_regex2id(corpus.pAttribute, x)))
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
    leftContext=.Object@leftContext,
    rightContext=.Object@rightContext,
    pAttribute=.Object@pAttribute,
    corpus=.Object@corpus,
    encoding=.Object@encoding,
    method=.Object@method,
    stat=subset(.Object@stat, node==query),
    call=deparse(match.call()),
    size=unique(subset(.Object@stat, node==query)[,"size_window"])
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

#' @rdname context-method
setMethod("context", "missing", function(){
  .getClassObjectsAvailable(".GlobalEnv", "context")
})


