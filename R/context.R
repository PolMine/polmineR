#' @include textstat.R features.R S4classes.R
NULL


#' @rdname context-class
setMethod("sample", "context", function(x, size){
  hits_unique <- unique(x@cpos[["match_id"]])
  if (size > length(hits_unique)){
    warning("argument size exceeds number of hits, returning original object")
    return(x)
  }
  x@cpos <- x@cpos[which(x@cpos[["match_id"]] %in% sample(hits_unique, size = size))]
  x@count <- as.integer(size)
  x@size <- length(which(x@cpos[["position"]] != 0))
  x
})



#' @include partition.R partition_bundle.R
NULL

#' @exportMethod context
#' @docType methods
#' @rdname context-method
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
#' @param query A query, which may by a character vector or a CQP query.
#' @param cqp defaults to is.cqp-function, or provide TRUE/FALSE
#' @param check A \code{logical} value, whether to check validity of CQP query
#'   using \code{check_cqp_query}.
#' @param p_attribute The p-attribute of the query.
#' @param boundary If provided, a length-one character vector specifying a
#'   s-attribute. It will be checked that corpus positions do not extend beyond
#'   the region defined by the s-attribute.
#' @param left Number of tokens to the left of the query match.
#' @param right Number of tokens to the right of the query match.
#' @param stoplist Exclude match for query if stopword(s) is/are are present in
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
#' @aliases context,slice-method as.matrix,context_bundle-method context,partition-method
#' @examples
#' use("polmineR")
#' p <- partition("GERMAPARLMINI", interjection = "speech")
#' y <- context(p, query = "Integration", p_attribute = "word")
#' y <- context(p, query = "Integration", p_attribute = "word", positivelist = "Bildung")
#' y <- context(
#'   p, query = "Integration", p_attribute = "word",
#'   positivelist = c("[aA]rbeit.*", "Ausbildung"), regex = TRUE
#' )
#' @exportMethod context
#' @rdname context-method
#' @name context
#' @docType methods

setMethod("context", "slice", function(
  .Object, query, cqp = is.cqp, check = TRUE,
  left = getOption("polmineR.left"),
  right = getOption("polmineR.right"),
  p_attribute = getOption("polmineR.p_attribute"),
  boundary = NULL,
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  count = TRUE,
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = TRUE,
  ...
) {
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if ("sAttribute" %in% names(list(...))) boundary <- list(...)[["sAttribute"]]
  if ("s_attribute" %in% names(list(...))) boundary <- list(...)[["s_attribute"]]
  
  if (is.numeric(left)) left <- as.integer(left) # input may be numeric
  if (is.numeric(right)) right <- as.integer(right) # input may be numeric
  
  # get regions for query matches
  .message("getting corpus positions", verbose = verbose)
  regions <- cpos(.Object = .Object, query = query, p_attribute = p_attribute[1], cqp = cqp, check = check)
  if (is.null(regions)){
    warning('No hits for query ', query, ' (returning NULL)')
    return( invisible(NULL) )
  } else {
    .message("number of hits:", nrow(regions), verbose = verbose)
  }
  colnames(regions) <- c("hit_cpos_left", "hit_cpos_right")
  
  ctxt <- context(.Object = regions, left = left, right = right, corpus = .Object@corpus)
  ctxt@query <- query
  ctxt@p_attribute <- p_attribute
  ctxt@corpus <- .Object@corpus
  ctxt@encoding <- .Object@encoding
  ctxt@partition <- as(.Object, "partition")
  ctxt@size_partition <- as.integer(.Object@size)
  ctxt@boundary <- if (!is.null(boundary)) boundary else character()

  # add decoded tokens (ids at this stage)
  ctxt <- enrich(ctxt, p_attribute = p_attribute, decode = FALSE, verbose = verbose)
  
  # generate positivelist/stoplist with ids and apply it
  if (!is.null(positivelist)) ctxt <- trim(ctxt, positivelist = positivelist, regex = regex, verbose = verbose)
  if (is.null(ctxt)) return(NULL)
  if (!is.null(stoplist)) ctxt <- trim(ctxt, stoplist = stoplist, regex = regex, verbose = verbose)
  if (is.null(ctxt)) return(NULL)
  
  .message("generating contexts", verbose = verbose)
  
  ctxt@size <- nrow(ctxt@cpos)
  ctxt@size_match <- as.integer(sum(regions[,2] - regions[,1]) + nrow(regions))
  ctxt@size_coi <- as.integer(ctxt@size) - ctxt@size_match
  ctxt@size_ref <- as.integer(ctxt@size_partition - ctxt@size_coi - ctxt@size_match)
  ctxt@size_partition <- size(.Object)
  ctxt@count <- length(unique(ctxt@cpos[["match_id"]]))
  
  # check that windows do not transgress s-attribute
  if (!is.null(boundary)){
    stopifnot(boundary %in% registry_get_s_attributes(ctxt@corpus))
    .message("checking that context positions to not transgress regions", verbose = verbose)
    ctxt <- enrich(ctxt, s_attribute = boundary, verbose = verbose)
    ctxt <- trim(ctxt, s_attribute = boundary, verbose = verbose, progress = progress)
  }
  
  # put together raw stat table
  if (count){
    .message("counting tokens", verbose = verbose)
    
    setkeyv(ctxt@cpos, paste(p_attribute, "id", sep = "_"))
    ctxt@stat <- ctxt@cpos[which(ctxt@cpos[["position"]] != 0)][, .N, by = c(eval(paste(p_attribute, "id", sep = "_"))), with = TRUE]
    setnames(ctxt@stat, "N", "count_coi")
    
    for (i in seq_along(p_attribute)){
      newColumn <- cl_id2str(corpus = .Object@corpus, p_attribute = p_attribute[i], id = ctxt@stat[[paste(p_attribute[i], "id", sep = "_")]], registry = registry())
      newColumnNative <- as.nativeEnc(newColumn, from = .Object@encoding)
      ctxt@stat[, eval(p_attribute[i]) := newColumnNative]
    }
  }
  ctxt
})

#' @rdname context-method
setMethod("context", "partition", function(
  .Object, query, cqp = is.cqp, check = TRUE,
  left = getOption("polmineR.left"),
  right = getOption("polmineR.right"),
  p_attribute = getOption("polmineR.p_attribute"),
  boundary = NULL,
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  count = TRUE,
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = TRUE,
  ...
) callNextMethod()
)

#' @rdname context-method
setMethod("context", "subcorpus", function(
  .Object, query, cqp = is.cqp, check = TRUE,
  left = getOption("polmineR.left"),
  right = getOption("polmineR.right"),
  p_attribute = getOption("polmineR.p_attribute"),
  boundary = NULL,
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  count = TRUE,
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = TRUE,
  ...
) callNextMethod()
)

#' @details If \code{.Object} is a \code{matrix}, the \code{context}-method will
#'   unfold the \code{matrix} (interpreted as regions defining left and right
#'   corpus positions) and return an elementary ... object.
#' @param corpus A length-one \code{character} vector stating the corpus ID of a
#'   CWB corpus.
#' @rdname context-method
#' @importFrom data.table between
setMethod("context", "matrix", function(.Object, corpus, left, right){
  if (ncol(.Object) != 2L) stop("context,matrix-method: .Object is required to be a two-column matrix")
  
  if (is.numeric(left)) left <- as.integer(left)
  if (is.numeric(right)) right <- as.integer(right)

  if (is.integer(left) && is.integer(right)){

    if (is.null(names(left)) && is.null(names(left))){
      
      positions_left <- rep(list(if (left >= 1L) -left:-1L else integer()), nrow(.Object))
      positions_right <- rep(list(if (right >= 1L) 1L:right else integer()), nrow(.Object))
      match_length <- .Object[,2] - .Object[,1]
      
      dt_left <- data.table(
        cpos = unlist(mapply(function(a, b) a + b, .Object[,1], positions_left, SIMPLIFY = FALSE)),
        position = unlist(positions_left),
        match_id = rep(1L:nrow(.Object), each = left)
      )
      dt_right <- data.table(
        cpos = unlist(mapply(function(a, b) a + b, .Object[,2], positions_right, SIMPLIFY = FALSE)),
        position = unlist(positions_right),
        match_id = rep(1L:nrow(.Object), each = right)
      )
      dt_node <- data.table(
        cpos = unlist(lapply(1L:nrow(.Object), function(i) .Object[i,1]:.Object[i,2])),
        position = rep(0L, sum(match_length) + nrow(.Object)),
        match_id = unlist(lapply(1L:nrow(.Object), function(i) rep(i, times = match_length[i] + 1L)))
      )
      
      cpos_dt <- rbind(dt_left, dt_right, dt_node)
      setorderv(cpos_dt, cols = c("match_id", "cpos"))
      

    } else {
      # set, left, right, corpus, s_attribute
      .Object <- cbind(.Object, match_id = 1L:nrow(.Object))
      regions_dt <- data.table(.Object)
      .fn <- function(.SD){
        stop("NOT Implemented at present")
        # hit_struc <- cl_cpos2struc(corpus = corpus, s_attribute = names(left), cpos = set[1], registry = registry())
        # maxStruc <- cl_attribute_size(corpus = corpus, attribute = s_attribute, attribute_type = "s", registry = registry())
        # get left min cpos
        # leftStruc <- queryStruc - left
        # leftStruc <- ifelse(leftStruc < 0, 0, leftStruc)
        # leftCposMin <- cl_struc2cpos(corpus = corpus, s_attribute = s_attribute, struc = leftStruc, registry = registry())[1]
        # cposLeft <- c(leftCposMin:(set[1]-1))
        # get right max cpos
        # rightStruc <- queryStruc + right
        # rightStruc <- ifelse(rightStruc > maxStruc - 1, maxStruc, rightStruc)
        # rightCposMax <- cl_struc2cpos(corpus = corpus, s_attribute = s_attribute, struc = rightStruc, registry = registry())[2]
        # cposRight <- c((set[2] + 1):rightCposMax)
        # handing it back
        # list(left = cposLeft, node = c(set[1]:set[2]), right = cposRight)
      }
      cpos_dt <- regions_dt[, .fn(.SD), by = c("match_id")]
      setnames(cpos_dt, old = c("V1", "V2"), new = c("cpos", "position"))
    }
  } else if (is.character(left) && is.character(right)){
    .fn <- function(.SD){
      cpos_left <- seq.int(
        from = cl_cpos2lbound(corpus = corpus, s_attribute = left, cpos = .SD[[1]][1], registry = registry()),
        to = .SD[[1]][1] - 1L
      )
      cpos_right <- seq.int(
        from = .SD[[2]][1] + 1L,
        to = cl_cpos2rbound(corpus = corpus, s_attribute = right, cpos = .SD[[2]][1], registry = registry())
      )
      list(
        c(cpos_left, .SD[[1]][1]:.SD[[2]][1], cpos_right),
        c(
          seq.int(from = -length(cpos_left), to = -1L, by = 1L),
          rep(0L, .SD[[2]][1] - .SD[[1]][1] + 1L),
          seq.int(from = 1L, to = length(cpos_right), by = 1L)
        )
      )
    }
    cpos_dt <- regions_dt[, .fn(.SD), by = c("match_id")]
    setnames(cpos_dt, old = c("V1", "V2"), new = c("cpos", "position"))
    
  }
  
  cpos_dt_min <- cpos_dt[between(cpos_dt[["cpos"]], lower = 0L, upper = (size(corpus) - 1L))]

  new(
    "context",
    query = character(),
    p_attribute = character(),
    count = nrow(.Object),
    corpus = corpus,
    stat = data.table(),
    cpos = cpos_dt_min,
    left = if (is.character(left)) 0L else as.integer(left),
    right = if (is.character(right)) 0L else as.integer(right),
    encoding = character(),
    partition = new("partition", stat = data.table(), size = 0L),
    boundary = character()
  )
})


#' @rdname context-method
setMethod("context", "corpus", function(
  .Object, query, cqp = is.cqp,
  p_attribute = getOption("polmineR.p_attribute"), boundary = NULL,
  left = getOption("polmineR.left"), right = getOption("polmineR.right"),
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  count = TRUE,
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = TRUE,
  ...
){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if ("sAttribute" %in% names(list(...))) boundary <- list(...)[["sAttribute"]]
  if ("sAttribute" %in% names(list(...))) boundary <- list(...)[["s_attribute"]]
  
  p <- as(.Object, "partition")
  
  # There is a potential overhead of performing the count here: When context-method
  # is called by kwic, the count is not needed. However, if context is called by
  # cooccurrences, it is much, much faster (for one p-attribute) having done the 
  # count here for the entire corpus.
  
  if (length(p_attribute) == 1L)
    p@stat <- count(.Object@corpus, p_attribute = p_attribute, decode = FALSE)@stat
  
  context(
    p, query = query, cqp = is.cqp,
    left = left, right = right,
    p_attribute = p_attribute, boundary = boundary,
    stoplist = stoplist, positivelist = positivelist, regex = regex,
    count = count, mc = mc, verbose = verbose, progress = progress
  )
})


#' @rdname context-method
setMethod("context", "character", function(
  .Object, query, cqp = is.cqp,
  p_attribute = getOption("polmineR.p_attribute"), boundary = NULL,
  left = getOption("polmineR.left"), right = getOption("polmineR.right"),
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  count = TRUE,
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = TRUE,
  ...
){
  context(
    .Object = corpus(.Object),
    query = query,
    cqp = cqp,
    p_attribute = p_attribute,
    boundary = boundary,
    left = left, right = right,
    stoplist = stoplist, positivelist = positivelist, regex = regex,
    count = count,
    mc = mc,
    verbose = verbose,
    progress = progress,
    ...
  )
})


#' @docType methods
#' @rdname context-method
setMethod("context", "partition_bundle", function(.Object, query, p_attribute, verbose = TRUE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]

  retval <- new("context_bundle", query = query, p_attribute = p_attribute)
  if (!is.numeric(positivelist)){
    corpus <- unique(lapply(.Object@objects, function(x) x@corpus))
    positivelist <- unlist(lapply(
      positivelist,
      function(x) cl_regex2id(corpus = corpus, p_attribute = p_attribute, regex = x, registry = registry()))
      )
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
setMethod("context", "cooccurrences", function(.Object, query, check = TRUE, complete = FALSE){
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
    size = unique(subset(.Object@stat, .Object@stat[, "node"] == query)[,"size_window"])
  )  
  stop("due to refactoring the context method, this does not work at present")
  # if (complete){
  #   s_attribute <- names(get(newObject@partition, ".GlobalEnv")@s_attributes)[[1]]
  #   sAttr <- paste(
  #     newObject@corpus, ".",
  #     names(get(newObject@partition, ".GlobalEnv")@s_attributes)[[1]],
  #     sep = ""
  #   )
  #   hits <- cpos(
  #     newObject@query,
  #     get(newObject@partition, ".GlobalEnv"),
  #     p_attribute = newObject@p_attribute,
  #     verbose = FALSE, check = check
  #   )
  #   newObject@size <- nrow(hits)
  #   hits <- cbind(hits, cl_cpos2struc(corpus = newObject@corpus, s_attribute = s_attribute, cpos = hits[,1], registry = registry()))
  #   newObject@cpos <- .make_context_dt(hits, left = newObject@left, right = newObject@right, corpus = newObject@corpus, s_attribute = sAttr)
  # }
  newObject
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


setAs(from = "kwic", to = "context", def = function(from){
  new(
    "context",
    query = "",
    count = -1L,
    partition = new("partition", size = 0L, stat = data.table()),
    size_partition = -1L,
    size_match = -1L,
    size = -1L,
    boundary = character(),
    call = "",
    left = from@left,
    right = from@right,
    p_attribute = from@p_attribute,
    corpus = from@corpus,
    stat = data.table(),
    encoding = from@encoding,
    cpos = from@cpos
  )
})


