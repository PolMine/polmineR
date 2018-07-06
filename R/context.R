#' @include textstat.R features.R S4classes.R
NULL


#' @rdname context-class
setMethod("sample", "context", function(x, size){
  hits_unique <- unique(x@cpos[["hit_no"]])
  if (size > length(hits_unique)){
    warning("argument size exceeds number of hits, returning original object")
    return(x)
  }
  x@cpos <- x@cpos[which(x@cpos[["hit_no"]] %in% sample(hits_unique, size = size))]
  x@count <- size
  x@size <- length(which(x@cpos[["position"]] != 0))
  x
})



#' @include partition.R partition_bundle.R
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
#' @param .Object a partition or a partition_bundle object
#' @param query query, which may by a character vector or a cqpQuery object
#' @param cqp defaults to is.cqp-function, or provide TRUE/FALSE
#' @param p_attribute p-attribute of the query
#' @param s_attribute if provided, it will be checked that corpus positions do not extend beyond
#' the region defined by the s-attribute 
#' @param left no of tokens and to the left of the node word
#' @param right no of tokens to the right of the node word
#' @param stoplist exclude a query hit from analysis if stopword(s) is/are in
#'   context. See positivelist for further explanation.
#' @param positivelist character vector or numeric/integer vector: include a query hit
#'   only if token in positivelist is present. If positivelist is a character
#'   vector, it may include regular expressions (see parameter regex)
#' @param regex logical, defaults to FALSE - whether stoplist and/or positivelist are
#'   regular expressions 
#' @param count logical
#' @param mc whether to use multicore; if NULL (default), the function will get
#'   the value from the options
#' @param verbose report progress, defaults to TRUE
#' @param progress logical, whether to show progress bar
#' @param ... further parameters
#' @return depending on whether a partition or a partition_bundle serves as
#'   input, the return will be a context object, or a \code{context_bundle} object
#' @author Andreas Blaette
#' @aliases context context,partition-method as.matrix,context_bundle-method
#'   context,context_bundle-method
#'   context,partition_bundle-method context,cooccurrences-method
#'   context,cooccurrences-method
#' @examples
#' use("polmineR")
#' p <- partition("GERMAPARLMINI", interjection = "speech")
#' y <- context(p, query = "Integration", p_attribute = "word")
#' y <- context(p, query = "Integration", p_attribute = "word", positivelist = "Bildung")
#' y <- context(
#'   p, query = "Integration", p_attribute = "word",
#'   positivelist = c("[aA]rbeit.*", "Ausbildung"), regex = TRUE
#' )
#' @import data.table
#' @exportMethod context
#' @rdname context-method
#' @name context
#' @docType methods
#' @aliases context,partition-method
setMethod("context", "partition", function(
  .Object, query, cqp = is.cqp,
  left = getOption("polmineR.left"),
  right = getOption("polmineR.right"),
  p_attribute = getOption("polmineR.p_attribute"), s_attribute = NULL,
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  count = TRUE,
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = TRUE,
  ...
) {
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]

  # set method for making left and right context
  left <- as.integer(left)
  right <- as.integer(right)
  if (is.integer(left) && is.integer(right)){
    if (is.null(names(left)) && is.null(names(left))){
      cposMethod <- "expandToCpos"
    } else {
      cposMethod <- "expandBeyondRegion"
      s_attribute <- unique(c(names(left), names(right)))
    }
  } else if (is.character(left) && is.character(right)){
    cposMethod <- "expandToRegion"
    s_attribute <- unique(c(left, right))
  }
  
  # generate the context object (ctxt)
  ctxt <- new(
    "context",
    query = query, p_attribute = p_attribute, corpus = .Object@corpus,
    stat = data.table(), cpos = data.table(),
    left = if (is.character(left)) 0L else as.integer(left),
    right = if (is.character(right)) 0L else as.integer(right),
    encoding = .Object@encoding,
    partition = .Object,
    size_partition = as.integer(.Object@size),
    s_attribute = if (!is.null(s_attribute)) s_attribute else character()
  )
  # ctxt@call <- deparse(match.call()) # kept seperate for debugging purposes
  
  # getting counts of query in partition
  .message("getting corpus positions", verbose = verbose)
  hits <- cpos(.Object, query, p_attribute[1], cqp = cqp)
  if (is.null(hits)){
    warning('No hits for query ', query, ' (returning NULL)')
    return(invisible(NULL))
  } else {
    if (is.null(hits)){
      warning('No hits for query ', query, ' (returning NULL)')
      return( invisible(NULL) )
    } else {
      .message("number of hits:", nrow(hits), verbose = verbose)
    }
  }
  colnames(hits) <- c("hit_cpos_left", "hit_cpos_right")
  
  hits <- cbind(hits, hit_no = 1:nrow(hits))
  
  # create matrix_list (expanded form), then data.table in ctxt@cpos 
  matrix_list <- lapply(
    1L:nrow(hits),
    function(i){
      cbind(
        hit_no = i,
        .makeLeftRightCpos[[cposMethod]](hits[i,], left, right, corpus, s_attribute)
      )
    }
  )
  cpos_matrix <- do.call(rbind, matrix_list) # potentially slow, move to Rcpp later
  ctxt@cpos <- data.table(cpos_matrix)
  setnames(ctxt@cpos, old = c("V2", "V3"), new = c("cpos", "position"))
  
  # add decoded tokens (ids at this stage)
  ctxt <- enrich(ctxt, p_attribute = p_attribute, decode = FALSE, verbose = verbose)
  
  # generate positivelist/stoplist with ids and apply it
  if (!is.null(positivelist)) ctxt <- trim(ctxt, positivelist = positivelist, regex = regex, verbose = verbose)
  if (!is.null(stoplist)) ctxt <- trim(ctxt, stoplist = stoplist, regex = regex, verbose = verbose)
  
  .message("generating contexts", verbose = verbose)
  
  ctxt@size <- nrow(ctxt@cpos)
  ctxt@size_coi <- as.integer(ctxt@size)
  ctxt@size_ref <- as.integer(ctxt@size_partition - ctxt@size_coi)
  ctxt@count <- length(unique(ctxt@cpos[["hit_no"]]))
  
  # check that windows do not transgress s-attribute
  .message("checking that context positions to not transgress regions", verbose = verbose)
  ctxt <- enrich(ctxt, s_attribute = s_attribute, verbose = verbose)
  ctxt <- trim(ctxt, s_attribute = s_attribute, verbose = verbose, progress = progress)
  
  # put together raw stat table
  if (count){
    .message("counting tokens", verbose = verbose)
    
    setkeyv(ctxt@cpos, paste(p_attribute, "id", sep = "_"))
    ctxt@stat <- ctxt@cpos[which(ctxt@cpos[["position"]] != 0)][, .N, by = c(eval(paste(p_attribute, "id", sep = "_"))), with = TRUE]
    setnames(ctxt@stat, "N", "count_window")
    
    for ( i in 1:length(p_attribute) ){
      newColumn <- CQI$id2str(.Object@corpus, p_attribute[i], ctxt@stat[[paste(p_attribute[i], "id", sep = "_")]])
      newColumnNative <- as.nativeEnc(newColumn, from = .Object@encoding)
      ctxt@stat[, eval(p_attribute[i]) := newColumnNative]
    }
  }
  ctxt
})



#' @param set a numeric vector with three items: left cpos of hit, right cpos of hit, struc of the hit
#' @param left no of tokens to the left
#' @param right no of tokens to the right
#' @param s_attribute the integrity of the s_attribute to be checked
#' @return a list!
#' @noRd
.makeLeftRightCpos <- list(
  
  "expandToCpos" = function(set, left, right, corpus, s_attribute){
    cposLeft <- (set[1] - left):(set[1] -1)
    cposRight <- (set[2] + 1):(set[2] + right)
    matrix(
      c(
        c(cposLeft, set[1]:set[2], cposRight),
        c(-left:-1, rep(0, set[2] - set[1] + 1), 1:right)
      ),
      ncol = 2
    )
  },
  
  "expandToRegion" = function(set, left, right, corpus, s_attribute){
    stop("NOT Implemented at present")
    cposLeft <- c((CQI$cpos2lbound(corpus, s_attribute, set[1])):(set[1] - 1))
    cposRight <- c((set[2] + 1):(CQI$cpos2rbound(corpus, s_attribute, set[1])))
    return(list(left = cposLeft, node = c(set[1]:set[2]), right = cposRight))
  },
  
  "expandBeyondRegion" = function(set, left, right, corpus, s_attribute){
    stop("NOT Implemented at present")
    queryStruc <- CQI$cpos2struc(corpus, s_attribute, set[1])
    maxStruc <- CQI$attribute_size(corpus, s_attribute, type = "s")
    # get left min cpos
    leftStruc <- queryStruc - left
    leftStruc <- ifelse(leftStruc < 0, 0, leftStruc)
    leftCposMin <- CQI$struc2cpos(corpus, s_attribute, leftStruc)[1]
    cposLeft <- c(leftCposMin:(set[1]-1))
    # get right max cpos
    rightStruc <- queryStruc + right
    rightStruc <- ifelse(rightStruc > maxStruc - 1, maxStruc, rightStruc)
    rightCposMax <- CQI$struc2cpos(corpus, s_attribute, rightStruc)[2]
    cposRight <- c((set[2] + 1):rightCposMax)
    # handing it back
    return(list(left = cposLeft, node = c(set[1]:set[2]), right = cposRight))
  }
  
)


#' @rdname context-method
setMethod("context", "character", function(
  .Object, query, cqp = is.cqp,
  p_attribute = getOption("polmineR.p_attribute"), s_attribute = NULL,
  left = getOption("polmineR.left"), right = getOption("polmineR.right"),
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  count = TRUE,
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = TRUE,
  ...
){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]

  C <- Corpus$new(.Object)
  C$count(p_attribute, decode = FALSE)
  context(
    C$as.partition(), query = query, cqp = is.cqp,
    p_attribute = p_attribute, s_attribute = s_attribute,
    stoplist = stoplist, positivelist = positivelist, regex = regex,
    count = count, mc = mc, verbose = verbose, progress = progress
  )
})


#' @docType methods
#' @rdname context-method
setMethod("context", "partition_bundle", function(.Object, query, p_attribute, verbose = TRUE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]

  retval <- new("context_bundle", query = query, p_attribute = p_attribute)
  if (!is.numeric(positivelist)){
    corpus <- unique(lapply(.Object@objects, function(x) x@corpus))
    positivelist <- unlist(lapply(positivelist, function(x) CQI$regex2id(corpus, p_attribute, x)))
  }
  
  retval@objects <- sapply(
    .Object@objects,
    function(x) {
      .message("proceeding to partition ", x@name, verbose = verbose)
      context(x, query, ...)
    },
    simplify = TRUE,
    USE.NAMES = TRUE
  )
retval
})

#' @param complete enhance completely
#' @rdname context-method
setMethod("context", "cooccurrences", function(.Object, query, complete = FALSE){
  newObject <- new(
    "context",
    query = query,
    partition = .Object@partition,
    size_partition = as.integer(.Object@partition_size),
    left = as.integer(.Object@left),
    right = as.integer(.Object@right),
    p_attribute = .Object@p_attribute,
    corpus = .Object@corpus,
    encoding = .Object@encoding,
    method = .Object@method,
    stat = subset(.Object@stat, .Object@stat[, "node"]==query),
    call = deparse(match.call()),
    size = unique(subset(.Object@stat, .Object@stat[, "node"]==query)[,"size_window"])
  )  
  stop("due to refactoring the context method, this does not work at present")
  if (complete == TRUE){
    s_attribute <- names(get(newObject@partition, ".GlobalEnv")@s_attributes)[[1]]
    sAttr <- paste(
      newObject@corpus, ".",
      names(get(newObject@partition, ".GlobalEnv")@s_attributes)[[1]],
      sep=""
    )
    hits <- cpos(
      newObject@query,
      get(newObject@partition, ".GlobalEnv"),
      p_attribute=newObject@p_attribute,
      verbose = FALSE
    )
    newObject@size <- nrow(hits)
    hits <- cbind(hits, CQI$cpos2struc(newObject@corpus, s_attribute, hits[,1]))
    newObject@cpos <- apply(
      hits, 1, function(row) {
        .makeLeftRightCpos[["expandToCpos"]](
          row,
          left = newObject@left,
          right = newObject@right,
          s_attribute = sAttr
        )
      }    
    )
  }
  return(newObject)
})


#' @include bundle.R
NULL


#' @docType methods
#' @noRd
setMethod("summary", "context_bundle", function(object, top = 3){
  sizes_partition <- unlist(lapply(object@objects, function(x) x@partition_size))
  counts <- unlist(lapply(object@objects, function(x) x@frequency))
  overview <- data.frame(
    count = counts,
    freq = round(counts / sizes_partition * 100000, 2)
  )
  overview <- cbind(overview, t(data.frame(lapply(object@objects, function(x) .statisticalSummary(x)$no))))
  colnames(overview)[3:6] <- criticalValue <- c(">10.83", ">7.88", ">6.63", ">3.84")
  overview <- cbind(overview, t(data.frame(lapply(object@objects, function(x) rownames(x@stat)[1:top]))))
  overview
})

#' @docType methods
#' @noRd
setMethod("show", "context_bundle", function(object){
  summary(object)
})


