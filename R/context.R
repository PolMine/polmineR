#' @include partition-class.R context-class.R
NULL

#' @param object a partition or a partitionCluster object
#' @param ... further arguments
#' @exportMethod context
#' @docType methods
#' @noRd
setGeneric("context", function(object, ...){standardGeneric("context")})

#' Analyze context of a node word
#' 
#' Retrieve the concordances of a token and calculate the log-likelihood test for collocates
#' For formulating the query, CPQ syntax may be used (see examples).
#' 
#' @param object a partition or a partitionCluster object
#' @param query query, which may by a character vector or a cqpQuery object
#' @param pAttribute p-attribute of the query
#' @param leftContext no of tokens and to the left of the node word
#' @param rightContext no of tokens to the right of the node word
#' @param minSignificance minimum log-likelihood value
#' @param posFilter character vector with the POS tags to be included - may not be empty!!
#' @param filterType either "include" or "exclude"
#' @param stoplist exclude a query hit from analysis if stopword(s) is/are in context
#' @param positivelist character vector or numeric vector: include a query hit only if token in positivelist is present. If positivelist is a character vector, it is assumed to provide regex expressions (incredibly long if the list is long)
#' @param statisticalTest either "LL" (default) or "pmi", if NULL, calculating the statistics will be skipped
#' @param mc whether to use multicore; if NULL (default), the function will get the setting from the drillingControls
#' @param verbose report progress, defaults to TRUE
#' @return depending on whether a partition or a partitionCluster serves as input, the return will be a context object, or a contextCluster object
#' @author Andreas Blaette
#' @aliases context context,partition-method as.matrix,contextCluster-method as.TermContextMatrix,contextCluster-method context,contextCluster-method context,partitionCluster-method
#' @examples
#' \dontrun{
#' p <- partition(list(text_type="speech"), "PLPRBTTXT")
#' a <- context('"Integration"', p)
#' }
#' @importFrom parallel mclapply
#' @exportMethod context
#' @rdname context-methods
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
    statisticalTest="LL",
    mc=NULL,
    verbose=TRUE
  ) {
    if (!pAttribute %in% names(object@tf) && !is.null(statisticalTest)) {
      if (verbose==TRUE) message("... required tf list in partition not yet available: doing this now")
      object <- enrich(object, tf=pAttribute)
    }
    if (is.null(pAttribute)) pAttribute <- get("drillingControls", '.GlobalEnv')[['pAttribute']]
    if (leftContext == 0) leftContext <- get("drillingControls", '.GlobalEnv')[['leftContext']]
    if (rightContext == 0) rightContext <- get("drillingControls", '.GlobalEnv')[['rightContext']]
    if (minSignificance == -1) minSignificance <- get("drillingControls", '.GlobalEnv')[['minSignificance']]
    if (is.null(mc)) mc <- get("drillingControls", '.GlobalEnv')[['multicore']]
    ctxt <- new(
      "context",
      query=query,
      pattribute=pAttribute,
      corpus=object@corpus,
      left.context=leftContext,
      right.context=rightContext,
      encoding=object@encoding,
      posFilter=as.character(posFilter),
      partition=object@label,
      partitionSize=object@size
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
      ctxt@statisticalTest <- statisticalTest
      wc <- table(unlist(lapply(bigBag, function(x) x$id)))
      if (statisticalTest == "LL"){
        if (verbose==TRUE) message("... performing log likelihood test")
        calc <- .g2Statistic(as.integer(names(wc)), unname(wc), ctxt@size, object, pAttribute)
        ctxt@statisticalSummary <- .statisticalSummary(ctxt)
      } else if (statisticalTest=="pmi"){
        if (verbose==TRUE) message("... calculating pointwise mutual information")
        calc <- .pmi(
          windowIds=as.integer(names(wc)),
          windowFreq=unname(wc),
          countTarget=ctxt@frequency,
          partitionObject=partition,
          pAttribute=pAttribute
        )
      } else {
        warning("test suggested not supported")
      }
      ctxt@stat <- as.data.frame(cbind(
        rank=1:nrow(calc),
        calc
      ))
      ctxt <- trim(ctxt, minSignificance=minSignificance)
      rownames(ctxt@stat) <- cqi_id2str(corpus.pattr, ctxt@stat[,"collocateId"])
      Encoding(rownames(ctxt@stat)) <- object@encoding
    }
    ctxt
  })


.surrounding <- function (set, ctxt, corpus.sattr, filterType, stoplistIds, positivelistIds) {
  bag <- list()
  set <- as.numeric(set)
  cpos.left <- c((set[1]-ctxt@left.context):(set[1]-1))
  cpos.left <- cpos.left[which(cqi_cpos2struc(corpus.sattr, cpos.left)==set[3])]
  cpos.right <- c((set[2]+1):(set[2]+ctxt@right.context))
  cpos.right <- cpos.right[which(cqi_cpos2struc(corpus.sattr, cpos.right)==set[3])]
  bag$cpos <- list(left=cpos.left, node=c(set[1]:set[2]), right=cpos.right)
  cpos <- c(cpos.left, cpos.right)
  posChecked <- cpos[.filter[[filterType]](cqi_cpos2str(paste(ctxt@corpus,".pos", sep=""), cpos), ctxt@posFilter)]
  bag$id <- cqi_cpos2id(paste(ctxt@corpus,".", ctxt@pattribute, sep=""), posChecked)
  if (!is.null(stoplistIds) || !is.null(positivelistIds)) {
    ids <- cqi_cpos2id(paste(ctxt@corpus,".", ctxt@pattribute, sep=""), cpos)
    if (!is.null(stoplistIds)) if (any(stoplistIds %in% ids)) {bag <- NULL}
    if (!is.null(positivelistIds)) if (any(positivelistIds %in% ids) == FALSE) {bag <- NULL}
  }
  bag
}


setMethod('summary', 'context',
  function(object) {
  .statisticalSummary(object)
  
}
)

setMethod('head', 'context', function(x, n=10) {
  x@stat[1:n,c(1,3,4,7)]
})

setMethod('show', 'context',
          function(object) {
            cat("\n** Context object - general information: **\n")
            cat(sprintf("%-20s", "CWB-Korpus:"), object@corpus, "\n")
            cat(sprintf("%-20s", "Partition:"), object@partition, "\n")
            cat(sprintf("%-20s", "Node:"), object@query, "\n")
            cat(sprintf("%-20s", "P-Attribute:"), object@pattribute, "\n")
            cat(sprintf("%-20s", "Node count:"), object@frequency, "\n")
            cat(sprintf("%-20s", "Stat table length:"), nrow(object@stat), "\n\n")
})

  
setMethod('[', 'context',
          function(x,i) {
            drillingControls <- get("drillingControls", '.GlobalEnv')
            conc <- kwic(x, metadata=drillingControls$kwicMetadata)
            conc@table <- conc@table[i,]
            show(conc)
          }        
)

setMethod('[[', 'context',
  function(x,i) {
    drillingControls <- get("drillingControls", '.GlobalEnv')
    conc <- kwic(x, metadata=drillingControls$kwicMetadata, collocate=i)
    foo <- show(conc)
  }        
)



#' Prepare data for an ego-network
#' 
#' For a node word, collocates of n degrees are calculated
#' 
#' The function returns a data frame that can be converted into an igraph object easily.
#' This conversion is not part of the function to keep number of dependencies of the 
#' package low.
#' 
#' @param node query, which may by a multi-word unit
#' @param Partition a partition object
#' @param degrees the degrees of the resulting egoNetwork
#' @param pAttribute pattribute of the query
#' @param leftContext no of tokens and to the left of the node word
#' @param rightContext no of tokens to the right of the node word
#' @param minSignificance minimum log-likelihood value
#' @param posFilter character vector with the POS tags to be included - may not be empty!!
#' @return a data frame that can be turned into an igraph object with graph.data.frame (see example)
#' @examples
#' \dontrun{
#'  nw <- partition(list(text_year="2005", text_type="speech"), corpus="PLPRNWHTM")
#'  net <- egoNetwork(nw, "Integration", 1, "word", 5,5, 3.84, "NN")
#'  g <- graph.data.frame(net[,c(1,2,3,7)])
#'  tklplot(g)
#'  }
#' @author Andreas Blaette
#' @export egoNetwork
egoNetwork <- function(node, Partition, degrees, pAttribute="useControls", leftContext=0, rightContext=0, minSignificance, posFilter=c()) {
  gData <- context(node, Partition, pAttribute, leftContext, rightContext, minSignificance, posFilter)@stat
  gData <- cbind(node=rep(node, times=nrow(gData)), target=rownames(gData), degree=rep(1, times=nrow(gData)), gData)
  rownames(gData) <- NULL
  for ( degree in 2:degrees ) {
    terms <- gData[which(gData$degree==(degree-1)),2]
    for ( term in terms ) {
      dataNew <- context(term, Partition, pAttribute, leftContext, rightContext, minSignificance, posFilter)@stat
      dataNew <- cbind(node=rep(term, times=nrow(dataNew)), target=rownames(dataNew), degree=degree, dataNew)
      rownames(dataNew) <- NULL
      gData <- rbind(gData, dataNew)
    }
  }
  vertices <- unique(c(as.vector(unname(unlist(gData[,1]))), as.vector(unname(unlist(gData[,2])))))
  # verticeData <- data.frame(vertices=vertices, Partition@tf[[pAttribute]][vertices,"tf"])
  gData
}

setMethod("context", "partitionCluster", function(
  object, query, pAttribute="useControls",
  leftContext=0, rightContext=0,
  minSignificance=-1, posFilter="useControls", filterType="useControls",
  stoplist=c(), statisticalTest="LL",
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

