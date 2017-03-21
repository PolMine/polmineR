#' @include partition_class.R partitionBundle_class.R
NULL

#' @param ... further arguments
#' @exportMethod context
#' @docType methods
#' @noRd
setGeneric("context", function(.Object, ...) standardGeneric("context") )

#' Analyze context of a node word.
#' 
#' Retrieve the word context of a token, optionally checking for boundaries of a XML
#' region.
#' 
#' For formulating the query, CPQ syntax may be used (see
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
setMethod(f = "context", "partition", function(
    .Object, query, cqp = is.cqp,
    left = getOption("polmineR.left"), right = getOption("polmineR.right"),
    pAttribute = getOption("polmineR.pAttribute"), sAttribute = NULL,
    stoplist = NULL, positivelist = NULL,
    count = TRUE,
    mc = getOption("polmineR.mc"), verbose = TRUE, progress = FALSE
  ) {
    
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
    
    # generate the context object (ctxt)
    ctxt <- new(
      "context",
      query = query, pAttribute = pAttribute, corpus = .Object@corpus,
      stat = data.table(), cpos = data.table(),
      left = if (is.character(left)) 0 else left,
      right = if (is.character(right)) 0 else right,
      encoding = .Object@encoding,
      partition = .Object,
      partitionSize = as.numeric(.Object@size),
      sAttribute = if (!is.null(sAttribute)) sAttribute else character()
    )
    # ctxt@call <- deparse(match.call()) # kept seperate for debugging purposes
    
    # getting counts of query in partition
    .verboseOutput(message = "getting cpos", verbose = verbose)
    hits <- cpos(.Object, query, pAttribute[1], cqp = cqp)
    if (is.null(hits)){
      warning('No hits for query ', query, ' (returning NULL object)')
      return(NULL)
    }
    colnames(hits) <- c("hit_cpos_left", "hit_cpos_right")
    
    if (!is.null(sAttribute)) hits <- cbind(hits, struc_hit = CQI$cpos2struc(.Object@corpus, sAttribute, hits[,1]))
    hits <- cbind(hits, hit_no = 1:nrow(hits))
    
    # create matrix_list (expanded form), then data.table in ctxt@cpos 
    matrix_list <- lapply(
      1:nrow(hits),
      function(i){
        cbind(
          hit_no = i,
          .makeLeftRightCpos[[cposMethod]](hits[i,], left, right, corpus, sAttribute)
        )
      }
    )
    cpos_matrix <- do.call(rbind, matrix_list)
    ctxt@cpos <- data.table(cpos_matrix)
    setnames(ctxt@cpos, old = c("V2", "V3"), new = c("cpos", "position"))
    
    # generate positivelist, negativelist
    # DOES NOT WORK RIGHT NOW!
    stoplistIds <- unlist(lapply(stoplist, function(x) CQI$regex2id(.Object@corpus, pAttribute, x)))
    if (is.numeric(positivelist)){
      positivelistIds <- positivelist
      if (verbose == TRUE) message("... using ids provided as positivelist")
    } else {
      positivelistIds <- unlist(lapply(positivelist, function(x) CQI$regex2id(.Object@corpus, pAttribute, x)))
    }
    
    .verboseOutput(message = "generating contexts", verbose = verbose)
    
    # add decoded tokens
    for (pAttr in pAttribute){
      ctxt@cpos[[paste(pAttr, "id", sep = "_")]] <- CQI$cpos2id(.Object@corpus, pAttr, ctxt@cpos[["cpos"]])
    }
    
    ctxt@size <- nrow(ctxt@cpos)
    ctxt@sizeCoi <- as.integer(ctxt@size)
    ctxt@sizeRef <- as.integer(ctxt@partitionSize - ctxt@sizeCoi)
    ctxt@count <- length(which(ctxt@cpos[["position"]] != 0))
    
    # put together raw stat table
    if (count){
      .verboseOutput(message = "counting tokens", verbose = verbose)
      
      setkeyv(ctxt@cpos, paste(pAttribute, "id", sep = "_"))
      ctxt@stat <- ctxt@cpos[which(ctxt@cpos[["position"]] != 0)][, .N, by = c(eval(paste(pAttribute, "id", sep = "_"))), with = TRUE]
      setnames(ctxt@stat, "N", "count_window")
      
      for (i in c(1:length(pAttribute))){
        newColumn <- CQI$id2str(.Object@corpus, pAttribute[i], ctxt@stat[[paste(pAttribute[i], "id", sep = "_")]])
        newColumnUtf8 <- as.utf8(newColumn)
        ctxt@stat[, eval(pAttribute[i]) := newColumnUtf8]
      }
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
    cposLeft <- c((set[1] - left):(set[1] -1))
    cposRight <- c((set[2] + 1):(set[2] + right))
    matrix(
      c(
        c(cposLeft, set[1]:set[2], cposRight),
        c(-left:-1, rep(0, set[2] - set[1] + 1), 1:right)
      ),
      ncol = 2
    )
  },
  
  "expandToRegion" = function(set, left, right, corpus, sAttribute){
    stop("NOT Implemented at present")
    cposLeft <- c((CQI$cpos2lbound(corpus, sAttribute, set[1])):(set[1] - 1))
    cposRight <- c((set[2] + 1):(CQI$cpos2rbound(corpus, sAttribute, set[1])))
    return(list(left = cposLeft, node = c(set[1]:set[2]), right = cposRight))
  },
  
  "expandBeyondRegion" = function(set, left, right, corpus, sAttribute){
    stop("NOT Implemented at present")
    queryStruc <- CQI$cpos2struc(corpus, sAttribute, set[1])
    maxStruc <- CQI$attribute_size(corpus, sAttribute, type = "s")
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
    return(list(left = cposLeft, node = c(set[1]:set[2]), right = cposRight))
  }
  
)


#' @rdname context-method
setMethod("context", "character", function(.Object, query, pAttribute = getOption("polmineR.pAttribute"), ...){
  C <- Corpus$new(.Object)
  C$count(pAttribute, id2str = FALSE)
  context(C$as.partition(), query = query, pAttribute = pAttribute, ...)
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
  stop("due to refactoring the context method, this does not work at present")
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
