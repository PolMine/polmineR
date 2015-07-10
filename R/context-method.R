#' @include partition-class.R partitionBundle-class.R
NULL


##################################################################
#                                                                #
#  Methods that can be applied to objects of the class 'context' #
#                                                                #
##################################################################



#' @param object a partition or a partitionBundle object
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
#' @param object a partition or a partitionBundle object
#' @param query query, which may by a character vector or a cqpQuery object
#' @param pAttribute p-attribute of the query
#' @param sAttribute if provided, it will be checked that cpos do not extend beyond
#' the region defined by the s-attribute 
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
#' @return depending on whether a partition or a partitionBundle serves as
#'   input, the return will be a context object, or a contextBundle object
#' @author Andreas Blaette
#' @aliases context context,partition-method as.matrix,contextBundle-method
#'   as.TermContextMatrix,contextBundle-method context,contextBundle-method
#'   context,partitionBundle-method ll ll-method context,collocations-method
#'   context,collocations-method
#' @examples
#' \dontrun{
#' p <- partition("PLPRBTTXT", list(text_type="speech"))
#' a <- context(p, "Integration", "word")
#' }
#' @importFrom parallel mclapply
#' @importFrom rcqp cqi_cpos2lbound cqi_cpos2rbound
#' @exportMethod context
#' @rdname context-method
#' @name context
#' @docType methods
#' @aliases context,partition-method
setMethod(
  f="context",
  signature(object="partition"),
  function
  (
    object, query,
    pAttribute=NULL, sAttribute="text_id",
    leftContext=NULL, rightContext=NULL,
    minSignificance=0, posFilter=NULL, filterType="exclude",
    stoplist=NULL, positivelist=NULL,
    statisticalTest="ll",
    mc=NULL, verbose=TRUE
  ) {
    if (is.null(pAttribute)) pAttribute <- slot(get("session", '.GlobalEnv'), 'pAttribute')
    if (!pAttribute %in% names(object@tf) && !is.null(statisticalTest)) {
      if (verbose==TRUE) message("... required tf list in partition not yet available: doing this now")
      object <- enrich(object, tf=pAttribute)
    }
    if (is.null(leftContext)) leftContext <- slot(get("session", '.GlobalEnv'), 'leftContext')
    if (is.null(rightContext)) rightContext <- slot(get("session", '.GlobalEnv'), 'rightContext')
    if (is.null(minSignificance)) minSignificance <- slot(get("session", '.GlobalEnv'), 'minSignificance')
    if (is.null(mc)) mc <- slot(get("session", '.GlobalEnv'), 'multicore')
    
    corpus.pAttribute <- paste(object@corpus, ".", pAttribute, sep="")
    corpus.sAttribute <- .setMethod(leftContext, rightContext, sAttribute, corpus=object@corpus)[1]
    method <- .setMethod(leftContext, rightContext, sAttribute, corpus=object@corpus)[2]
    
    ctxt <- new(
      "context",
      query=query, pAttribute=pAttribute, sAttribute=sAttribute, corpus=object@corpus,
      leftContext=ifelse(is.character(leftContext), 0, leftContext),
      rightContext=ifelse(is.character(rightContext), 0, rightContext),
      encoding=object@encoding, posFilter=as.character(posFilter),
      partition=object@name, partitionSize=object@size
    )
    ctxt@call <- deparse(match.call())
    
    if (verbose==TRUE) message("... getting counts for query in partition", appendLF=FALSE)
    # query <- .adjustEncoding(query, object@encoding)
    # Encoding(query) <- ctxt@encoding
    hits <- cpos(object, query, pAttribute)
    if (is.null(hits)){
      if (verbose==TRUE) message(' -> no hits')
      return(NULL)
    }
    if (!is.null(sAttribute)) hits <- cbind(hits, cqi_cpos2struc(corpus.sAttribute, hits[,1]))
    hits <- lapply(c(1: nrow(hits)), function(i) hits[i,])
    
    if (verbose==TRUE) message(' (', length(hits), " occurrences)")
    stoplistIds <- unlist(lapply(stoplist, function(x) cqi_regex2id(corpus.pAttribute, x)))
    if (is.numeric(positivelist)){
      positivelistIds <- positivelist
      if (verbose == TRUE) message("... using ids provided as positivelist")
    } else {
      positivelistIds <- unlist(lapply(positivelist, function(x) cqi_regex2id(corpus.pAttribute, x)))
    }
    if (verbose==TRUE) message("... counting tokens in context ")  
        
    if (mc==TRUE) {
      bigBag <- mclapply(hits, function(x) .surrounding(x, ctxt, leftContext, rightContext, corpus.sAttribute, filterType, stoplistIds, positivelistIds, method))
    } else {
      bigBag <- lapply(hits, function(x) .surrounding(x, ctxt, leftContext, rightContext, corpus.sAttribute, filterType, stoplistIds, positivelistIds, method))
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
      rownames(ctxt@stat) <- cqi_id2str(corpus.pAttribute, ctxt@stat[,"id"])
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

.surrounding <- function (set, ctxt, leftContext, rightContext, corpus.sAttribute, filterType, stoplistIds=NULL, positivelistIds=NULL, method) {
  cposList <- .makeLeftRightCpos[[method]](
    set,
    leftContext=leftContext,
    rightContext=rightContext,
    corpus.sAttribute=corpus.sAttribute
    )
  cpos <- c(cposList$left, cposList$right)
  posChecked <- cpos[.filter[[filterType]](cqi_cpos2str(paste(ctxt@corpus,".pos", sep=""), cpos), ctxt@posFilter)]
  id <- cqi_cpos2id(paste(ctxt@corpus,".", ctxt@pAttribute, sep=""), posChecked)
  if (!is.null(stoplistIds) || !is.null(positivelistIds)) {
    exclude <- FALSE
    ids <- cqi_cpos2id(paste(ctxt@corpus,".", ctxt@pAttribute, sep=""), cpos)
    if (!is.null(stoplistIds)) if (any(stoplistIds %in% ids)) {exclude <- TRUE}
    if (!is.null(positivelistIds)) {
      if (any(positivelistIds %in% ids) == FALSE) { exclude <- TRUE }
    }
  } else { 
    exclude <- FALSE
  }
  if (exclude == TRUE){
    retval <- NULL
  } else {
    retval <- list(cpos=cposList, id=id)
  }
  return(retval)
}


#' @docType methods
#' @rdname context-method
setMethod("context", "partitionBundle", function(
  object, query, pAttribute=NULL, sAttribute="text_id",
  leftContext=NULL, rightContext=NULL,
  minSignificance=NULL, posFilter=NULL, filterType="exclude",
  stoplist=c(), positivelist=NULL,
  statisticalTest="ll",
  mc=FALSE, verbose=TRUE  
) {
  contextBundle <- new("contextBundle", query=query, pAttribute=pAttribute)
  if (!is.numeric(positivelist)){
    corpus.pAttribute <- paste(
      unique(lapply(object@objects, function(x) x@corpus)),
      ".", pAttribute, sep=""
      )
    positivelist <- unlist(lapply(positivelist, function(x) cqi_regex2id(corpus.pAttribute, x)))
  }
  
  contextBundle@objects <- sapply(
    object@objects,
    function(x) {
      if (verbose == TRUE) message("... proceeding to partition ", x@name)
      context(
        x, query,
        pAttribute=pAttribute, sAttribute=sAttribute,
        leftContext=leftContext, rightContext=rightContext,
        minSignificance=minSignificance, posFilter=posFilter, filterType=filterType,
        stoplist=stoplist, positivelist=positivelist,
        statisticalTest=statisticalTest,
        mc=mc, verbose=verbose
      )
      },
    simplify = TRUE,
    USE.NAMES = TRUE
  )
  contextBundle
})

#' @param complete enhance completely
#' @rdname context-method
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

#' @rdname context-method
setMethod("context", "missing", function(){
  .getClassObjectsAvailable(".GlobalEnv", "context")
})


