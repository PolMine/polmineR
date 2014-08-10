#' trim an object
#' 
#' Method that includes varying to adjust objects from the driller package by 
#' applying thresholds, minimum frequencies etc. It can be applied to 'context',
#' 'keyness', 'context', 'partition' and 'partitionCluster' objects. See 
#' the respective documentation:
#' \describe{
#'  \item{context:}{\code{method?trim("context")}}
#'  \item{keyness:}{\code{method?trim("keyness")}} 
#'  \item{partition:}{\code{method?trim("partition")}}
#'  \item{partitionCluster:}{\code{method?trim("partitionCluster")}} 
#'  \item{crosstab:}{\code{method?trim("crosstab")}}
#' }
#' 
#' @param object the object to be trimmed
#' @param ... further arguments
#' @author Andreas Blaette
#' @docType methods
#' @aliases trim trim-method
#' @rdname trim-method
setGeneric("trim", function(object, ...){standardGeneric("trim")})

#' add pos information
#' 
#' Add information on part-of-speech tags of tokens to tokens. The method is 
#' available for objects of the classes 'partition', 'partitionCluster' and
#' 'keyness' respectively. 
#' \code{method?addPos("partitionCluster")}, \code{method?addPos("keyness")}).
#' @param object either a partition, a partitionCluster or a keyness object
#' @param ... further arguments
#' @return the original, enhanced object
#' @noRd
#' @docType methods
setGeneric("addPos", function(object,...){standardGeneric("addPos")})

setGeneric("keyness", function(x, ...){standardGeneric("keyness")})

# documented with meta,partition-method
setGeneric("meta", function(object, ...){standardGeneric("meta")})

#' contextual analysis
#' 
#' Statistical analysis of the context of a token. The method can be applied to partition or partitionCluster class objects.
#' \describe{
#'  \item{partition:}{\code{method?context("partition")}}
#'  \item{partitionCluster:}{\code{method?trim("partitionCluster")}} 
#' }

#' @param object a partition or a partitionCluster object
#' @param ... further arguments
#' @exportMethod context
#' @docType methods
#' @noRd
setGeneric("context", function(object, ...){standardGeneric("context")})


#' get term frequencies
#' 
#' Method to obtain term frequencies for one or multiple terms or queries.
#' The method can be applied to partition or partitionCluster class objects.
#' If object is a character string, frequencies for a whole corpus are returned.
#' Please see the respective documentation for details 
#' (\code{method?tf("partition")}, \code{method?tf("partitionCluster")} or
#' \code{method?tf("character")}).
#' 
#' @param object either a partition or a partitionCluster object
#' @param ... further parameters
#' @aliases tf tf-method
#' @rdname tf-method
setGeneric("tf", function(object, ...){standardGeneric("tf")})

#' mail result
#' 
#' Mail a result (to yourself).
#' Please see the respective documentation for details 
#' (\code{method?mail("keyness")}, \code{method?mail("partition")}.
#' 
#' @param object a driller object
#' @param ... further parameters
#' @aliases mail mail-method
#' @rdname mail
setGeneric("mail", function(object, ...){standardGeneric("mail")})

setGeneric("sAttributes", function(object,...){standardGeneric("sAttributes")})

#' enrich an object
#' 
#' Method to fill slots of a partition, partitionCluster or keyness object that 
#' have not been set up previously. See the respective documentation:
#' \describe{
#'  \item{partition and partitionCluster:}{\code{method?enrich("partition")}}
#'  \item{keyness}{\code{method?enrich("keyness")}}
#' }
#' 
#' @param object a partition, partitionCluster or keyness object
#' @param ... further parameters
#' @aliases enrich enrich-method
#' @docType methods
#' @rdname enrich-method
setGeneric("enrich", function(object, ...){standardGeneric("enrich")})

setGeneric("html", function(object, ...){standardGeneric("html")})

setGeneric("as.sparseMatrix", function(x,...){standardGeneric("as.sparseMatrix")})

setGeneric("as.partitionCluster", function(object,...){standardGeneric("as.partitionCluster")})

setGeneric("as.TermContextMatrix", function(x, col, ...) {standardGeneric("as.TermContextMatrix")})

setGeneric("score", function(object, ...){standardGeneric("score")})

setGeneric("speeches", function(object, ...){standardGeneric("speeches")})

setGeneric("kwic", function(object, ...){standardGeneric("kwic")})

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
#' @param positivelist include a query hit only if token in positivelist is present
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
    if (is.null(pAttribute)) pAttribute <- get("drillingControls", '.GlobalEnv')[['pAttribute']]
    if (!pAttribute %in% names(object@tf)) {
      if (verbose==TRUE) message("... required tf list in partition not yet available, needs to be done first")
      object <- enrich(object, tf=pAttribute)
    }
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
    
    message(' (', length(hits), " occurrences)")
    stoplistIds <- unlist(lapply(stoplist, function(x) cqi_regex2id(corpus.pattr, x)))
    positivelistIds <- unlist(lapply(positivelist, function(x) cqi_regex2id(corpus.pattr, x)))
    if (verbose==TRUE) message("... counting tokens in context ")  
    if (mc==TRUE) {
      bigBag <- mclapply(hits, function(x) .surrounding(x, ctxt, corpus.sattr, filterType, stoplistIds, positivelistIds))
    } else {
      bigBag <- lapply(hits, function(x) .surrounding(x, ctxt, corpus.sattr, filterType, stoplistIds, positivelistIds))
    }
    bigBag <- bigBag[!sapply(bigBag, is.null)]
    if (!is.null(stoplistIds) || !is.null(positivelistIds)){
      message("... hits filtered because stopword(s) occur in context: ", (length(hits)-length(bigBag)))
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

#' Transform a context cluster into a Term Context Matrix
#' 
#' Method based on the tm package, adds to as.TermDocumentMatrix
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a contextCluster object (S3 class)
#' @param col the col of the stat table to take
#' @param ... to make the check happy
#' @method as.TermContextMatrix contextCluster
#' @return a TermContextMatrix
#' @author Andreas Blaette
#' @docType method
#' @importFrom slam simple_triplet_matrix
#' @exportMethod as.TermContextMatrix
#' @noRd
setMethod("as.TermContextMatrix", "contextCluster", function (x, col, ...) {
  encoding <- unique(unlist(lapply(x@contexts, function(c) c@encoding)))
  corpus <- unique(unlist(lapply(x@contexts, function(c) c@corpus)))
  pAttribute <- unique(unlist(lapply(x@contexts, function(c) c@pattribute)))
  pAttr <- paste(corpus, '.', pAttribute, sep='')
  i <- unlist(lapply(x@contexts, function(c) (cqi_str2id(pAttr, rownames(c@stat))+1)))
  j <- unlist(lapply(c(1:length(x@contexts)), function(m) {rep(m,times=nrow(x[[m]]@stat))}))
  v <- unlist(lapply(x@contexts, function(c) c@stat[,col]))
  lexiconSize <- cqi_lexicon_size(pAttr)
  mat <- simple_triplet_matrix(i=i, j=j, v=v,
                               ncol=length(x@contexts),
                               nrow=lexiconSize+1,
                               dimnames=list(
                                 Terms=cqi_id2str(pAttr, c(0:lexiconSize)),
                                 Docs=names(x@contexts))
  )
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")
  class(mat) <- c("TermContextMatrix", "TermDocumentMatrix", "simple_triplet_matrix")
  mat
})

#' Fill slot 'pos' of partition (or partitionCluster) object
#' 
#' The 'pos'-slot of the partition (or partitionCluster) object is filled with tables
#' providing a statistic on the frequency of a pos-tag of a token
#' in the partition.
#' 
#' @param object a partition class object
#' @param pAttribute character vector (typically c("word") or c("lemma") or c("word", "lemma"))
#' @return an augmented partition or partitionCluster object (includes pos now)
#' @author Andreas Blaette
#' @noRd
setMethod("addPos", "partition", function(object, pAttribute){
  if (length(pAttribute) > 1) warning("taking only one pAttribute at a time")
  message("Adding pos information to partition object ", object@label)
  cpos <- unlist(apply(object@cpos, 1, function(x) c(x[1]:x[2])))
  message("... retrieving corpus information")
  bag <- data.frame(
    token=cqi_cpos2id(paste(object@corpus, '.', pAttribute, sep=''), cpos),
    pos=cqi_cpos2id(paste(object@corpus, '.pos', sep=''), cpos)
  )
  message("... doing the calculations")
  object@pos[[pAttribute]] <- list()
  crosstab <- table(bag)
  rownames(crosstab) <- cqi_id2str(paste(object@corpus, '.', pAttribute, sep=''), as.integer(rownames(crosstab)))
  colnames(crosstab) <- cqi_id2str(paste(object@corpus, '.pos', sep=''), as.integer(colnames(crosstab)))
  object@pos[[pAttribute]] <- apply(crosstab, 1, function(x) colnames(crosstab)[which.max(x)])
  Encoding(names(object@pos[[pAttribute]])) <- object@encoding
  # to make sure that there are no superfluous pos information
  object@pos[[pAttribute]] <- object@pos[[pAttribute]][names(object@pos[[pAttribute]]) %in% rownames(object@tf[[pAttribute]])]
  object
})

#' Fill slot 'pos' of a partitionCluster object with tables giving the statistic of pos
#' 
#' Augment the partitionCluster object
#' 
#' @param object a partition class object
#' @param pAttribute character vector - pos statistic for lemma or word
#' @return an augmented partition object (includes pos now)
#' @author Andreas Blaette
#' @noRd
setMethod("addPos", "partitionCluster", function(object, pAttribute){
  pimpedCluster <- object
  if (get('drillingControls', '.GlobalEnv')[['multicore']] == TRUE) {
    pimpedCluster@partitions <- mclapply(object@partitions, function(x) addPos(x, pAttribute))
  } else {
    pimpedCluster@partitions <- lapply(object@partitions, function(x) addPos(x, pAttribute))    
  }
  pimpedCluster
})

#' supplement keyness object with pos information
#' 
#' A keyness object will be supplemented with pos information. The working of the
#' method is potentially slow. It is recommended to trim the object first, before
#' adding pos information.
#' 
#' @param object the keyness object
#' @param Partition a partition object (the corpus of interest)
#' @return an enhanced keyness object 
#' @noRd 
setMethod("addPos", "keyness",
          function(object, Partition=NULL){
            if (is.null(Partition)){
              object <- .addPos(object)
            } else if (class(Partition) == "partition"){
              if (object@pattribute %in% names(Partition@pos)) {
                pos <- vapply(
                  rownames(object@stat[1:50, ]),
                  function(x) return(Partition@pos[[object@pattribute]][["max"]][x]),
                  USE.NAMES=FALSE,
                  FUN.VALUE="character")
                object@stat <- cbind(object@stat, pos=pos)
              }
            }
            object 
          }
)

#' enrich partition object (S4 method)
#' 
#' Fill slots of a partition object: Add object size, metainformation, pos.
#' 
#' @param object a partition object
#' @param size logical, defaults to FALSE
#' @param tf character vector providing the p-attributes for which frequency
#'   tables shall be generated
#' @param meta character vector providing s-attributes for which metainformation shall be supplied
#' @param addPos character vector providing p-attributes 
#' @param verbose logical, defaults to TRUE   
#' @exportMethod enrich
#' @aliases enrich,partition-method enrich,partitionCluster-method
#' @docType methods
#' @rdname enrich-partition-method
setMethod("enrich", "partition", function(object, size=FALSE, tf=NULL, meta=NULL, addPos=NULL, verbose=TRUE){
  if (size == TRUE) object@size <- .partition.size(object)
  if (length(tf>0)) {
    for (what in tf){
      if (verbose==TRUE) message('... computing term frequencies (for p-attribute ', what, ')')  
      object@tf[[what]] <- .cpos2tf(object, what)
    }
  }
  if (!is.null(meta)) {
    if (verbose==TRUE) message('... setting up metadata (table and list of values)')
    object <- .partition.metadata(object, meta)
  }
  if(!is.null(addPos)){
    object <- addPos(object, addPos)
  }
  object
})

# documented with enrich,partition-method
setMethod("enrich", "partitionCluster", function(object, size=TRUE, tf=c(), meta=NULL, addPos=NULL, mc=FALSE, verbose=TRUE){
  if (mc == FALSE) {
    object@partitions <- lapply(
      object@partitions,
      function(p) enrich(p, size=size, tf=tf, meta=meta, addPos, verbose=TRUE)
    )
  } else if (mc == TRUE){
    object@partitions <- mclapply(
      object@partitions,
      function(p) enrich(p, size=size, tf=tf, meta=meta, addPos, verbose=TRUE)
    )    
  }
  object
})


#' Enrich keyness object
#' 
#' Wrapper for adding a statistic on pos attributes to keyness object
#' 
#' @param object a keyness object
#' @param addPos no idea ....
#' @param verbose whether R should be talkative
#' @exportMethod enrich
#' @docType methods
#' @rdname enrich-keyness-method
#' @aliases enrich,keyness-method
setMethod("enrich", "keyness", function(object, addPos=NULL, verbose=TRUE){
  object <- addPos(object, Partition=addPos)
  object
})

