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

setAs(
  from = "context", to = "matrix",
  def = function(from){
    x <- copy(from@cpos)
    setorderv(x, cols = c("cpos", "match_id"))
    x[, "direction" := sign(x[["position"]])]
    dt <- rbindlist(lapply(
      c(-1L, 0L, 1L),
      function(dir){
        x[x[["direction"]] == dir][, list(
          cpos_left = .SD$cpos[1],
          cpos_right = .SD$cpos[nrow(.SD)]),
          by = "match_id"
        ]
      }
    ))

    as.matrix(setorderv(dt, cols = "cpos_left")[, "match_id" := NULL])
  }
)


#' @include partition.R partition_bundle.R
NULL

#' @exportMethod context
#' @docType methods
#' @rdname context-method
setGeneric("context", function(.Object, ...) standardGeneric("context") )

#' Analyze context of a node word.
#' 
#' Retrieve the word context of a token, optionally checking for boundaries of a
#' XML region.
#' 
#' For formulating the query, CPQ syntax may be used (see
#' examples). Statistical tests available are log-likelihood, t-test, pmi.
#' 
#' @param .Object a partition or a partition_bundle object
#' @param query A query, which may by a character vector or a CQP query.
#' @param cqp defaults to is.cqp-function, or provide TRUE/FALSE
#' @param check A `logical` value, whether to check validity of CQP query using
#'   `check_cqp_query`.
#' @param p_attribute The p-attribute of the query.
#' @param region An s-attribute, given by a length-one `character` vector.
#'   The context of query matches will be expanded to the left and right
#'   boundary of the region where the match is located. If arguments `left` and
#'   `right` are > 1, the left and right boundaries of the respective number of
#'   regions will be identified.
#' @param boundary If provided, a length-one character vector specifying a
#'   s-attribute. It will be checked that corpus positions do not extend beyond
#'   the region defined by the s-attribute.
#' @param left A single `integer` value defining the number of tokens to the
#'   left of the query match to include in the context. Advanced usage: (a) If
#'   `left` is a length-one `character` vector stating an s-attribute, the
#'   context will be expanded to the (left) boundary of the region where the
#'   match occurs. (b) If `left` is a named length-one `integer` vector, this
#'   value is the number regions of the structural attribute referred to by the
#'   vector's name  to the left of the query match that are included in the
#'   context.
#' @param right A single `integer` value, a length-one `character` vector or a
#'   named length-one `integer` value, with equivalent effects to argument
#'   `left`.
#' @param stoplist Exclude match for query if stopword(s) is/are are present in
#'   context. See positivelist for further explanation.
#' @param positivelist A `character` vector or `numeric`/`integer` vector:
#'   include a query hit only if token in positivelist is present. If
#'   positivelist is a `character` vector, it may include regular expressions
#'   (see parameter regex).
#' @param regex A `logical` value, defaults to `FALSE` - whether `stoplist`
#'   and/or `positivelist` are regular expressions.
#' @param count logical
#' @param mc Whether to use multicore; if `NULL` (default), the function will get
#'   the value from the options.
#' @param verbose Report progress? A `logical` value, defaults to `TRUE`.
#' @param progress A `logical` value, whether to show progress bar.
#' @param ... Further parameters.
#' @return depending on whether a `partition` or a `partition_bundle` serves as
#'   input, the return will be a context object, or a `context_bundle` object.
#'   Note that the number of objects in the `context_bundle` may differ from the
#'   number of objects in the input `bundle` object: `NULL` objects that result
#'   if no hit is obtained are dropped.
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
  region = NULL, boundary = NULL,
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  count = TRUE,
  mc = getOption("polmineR.mc"), verbose = FALSE, progress = TRUE,
  ...
) {
  
  if (is.numeric(left) && !is.integer(left))
    left <- setNames(as.integer(left), names(left))# input may be numeric
  
  if (is.numeric(right) && !is.integer(right))
    right <- setNames(as.integer(right), names(left)) # input may be numeric
  
  # get regions for query matches
  .message("getting corpus positions", verbose = verbose)
  regions <- cpos(
    .Object = .Object, query = query, p_attribute = p_attribute[1], cqp = cqp,
    check = check
  )
  if (is.null(regions)){
    warning('No hits for query ', query, ' (returning NULL)')
    return( invisible(NULL) )
  } else {
    .message("number of hits:", nrow(regions), verbose = verbose)
  }
  colnames(regions) <- c("hit_cpos_left", "hit_cpos_right")
  
  ctxt <- context(
    .Object = regions,
    left = left, right = right,
    p_attribute = p_attribute, region = region,
    boundary = boundary,
    corpus = .Object@corpus
  )
  
  ctxt@query <- query
  ctxt@p_attribute <- p_attribute
  ctxt@corpus <- .Object@corpus
  ctxt@encoding <- .Object@encoding
  ctxt@data_dir <- .Object@data_dir
  ctxt@registry_dir <- .Object@registry_dir
  ctxt@info_file <- .Object@info_file
  ctxt@template <- .Object@template
  ctxt@partition <- as(.Object, "partition")
  ctxt@size_partition <- as.integer(.Object@size)
  ctxt@boundary <- if (!is.null(boundary)) boundary else character()

  # add decoded tokens (ids at this stage)
  ctxt <- enrich(ctxt, p_attribute = p_attribute, decode = FALSE, verbose = verbose)
  
  # generate positivelist/stoplist with ids and apply it
  if (!is.null(positivelist))
    ctxt <- trim(ctxt, positivelist = positivelist, regex = regex, verbose = verbose)
  if (is.null(ctxt)) return(NULL)
  
  if (!is.null(stoplist))
    ctxt <- trim(ctxt, stoplist = stoplist, regex = regex, verbose = verbose)
  if (is.null(ctxt)) return(NULL)
  
  .message("generating contexts", verbose = verbose)
  
  ctxt@size <- nrow(ctxt@cpos)
  ctxt@size_match <- as.integer(sum(regions[,2] - regions[,1]) + nrow(regions))
  ctxt@size_coi <- as.integer(ctxt@size) - ctxt@size_match
  ctxt@size_ref <- as.integer(ctxt@size_partition - ctxt@size_coi - ctxt@size_match)
  ctxt@size_partition <- size(.Object)
  ctxt@count <- length(unique(ctxt@cpos[["match_id"]]))
  
  # put together raw stat table
  if (count) ctxt <- enrich(ctxt, stat = TRUE, verbose = verbose)
  
  ctxt
})

#' @rdname context-method
setMethod("context", "partition", function(
  .Object, query, cqp = is.cqp, check = TRUE,
  left = getOption("polmineR.left"),
  right = getOption("polmineR.right"),
  p_attribute = getOption("polmineR.p_attribute"),
  region = NULL,
  boundary = NULL,
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  count = TRUE,
  mc = getOption("polmineR.mc"), verbose = FALSE, progress = TRUE,
  ...
) callNextMethod()
)

#' @rdname context-method
setMethod("context", "subcorpus", function(
  .Object, query, cqp = is.cqp, check = TRUE,
  left = getOption("polmineR.left"),
  right = getOption("polmineR.right"),
  p_attribute = getOption("polmineR.p_attribute"),
  region = NULL,
  boundary = NULL,
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  count = TRUE,
  mc = getOption("polmineR.mc"), verbose = FALSE, progress = TRUE,
  ...
) callNextMethod()
)

#' @details If `.Object` is a `matrix`, the `context`-method will call
#'   `RcppCWB::region_matrix_context()`, the worker behind the
#'   `context()`-method.
#' @param corpus A length-one `character` vector stating a corpus ID.
#' @rdname context-method
#' @importFrom data.table between
#' @importFrom RcppCWB region_matrix_context corpus_s_attributes
setMethod("context", "matrix", function(.Object, corpus, left, right, p_attribute, region = NULL, boundary = NULL){
  if (ncol(.Object) != 2L) stop("context,matrix-method: .Object is required to be a two-column matrix")
  
  if (inherits(left, "numeric"))
    left <- setNames(as.integer(left), nm = names(left))
  
  if (inherits(right, "numeric"))
    right <- setNames(as.integer(right), nm = names(right))

  if (is.integer(left) && is.integer(right)){
    if (is.null(names(left)) && is.null(names(left))){
      if (is.null(region)){
        s_attr <- NULL
      } else {
        s_attr_present <- corpus_s_attributes(
          corpus = corpus,
          registry = corpus_registry_dir(corpus)
        )
        if (region %in% s_attr_present){
          s_attr <- region
        } else {
          warning(sprintf("s-attribute '%s' not defined", region))
          s_attr <- NULL
        }
      }
      
    } else {
      s_attr <- unique(c(names(left), names(right)))
      if (length(s_attr) > 1L) stop("Only one single s-attribute allowed.")
    }
  } else if (is.character(left) && is.character(right)){
    s_attr <- unique(left, right)
    if (length(s_attr) > 1L) stop("Only one single s-attribute allowed.")
    left <- 0L
    right <- 0L
  }
  
  regdir <- corpus_registry_dir(corpus)
  cpos_matrix <- region_matrix_context(
    corpus = corpus,
    matrix = .Object,
    s_attribute = s_attr,
    p_attribute = p_attribute[1],
    left = left, right = right,
    boundary = boundary,
    registry = regdir
  )
  cpos_dt <- as.data.table(cpos_matrix)
  
  colnames(cpos_dt) <- c(
    "position", "cpos", "match_id", paste(p_attribute[1], "id", sep = "_")
  )
  
  # region_matrix_context can only handle one p-attribute.
  # Add ids for further p-attributes now
  if (length(p_attribute) > 1L){
    for (i in 2L:length(p_attribute)){
      ids <- cl_cpos2id(
        corpus = corpus, registry = regdir,
        p_attribute = p_attribute[i], cpos = cpos_dt[["cpos"]]
      )
      cpos_dt[, (paste(p_attribute[i], "id", sep = "_")) := ids]
    }
  }

  setcolorder(cpos_dt, c("match_id", "cpos"))
  
  retval <- as(corpus(corpus), "context")
  retval@count <- nrow(.Object)
  retval@cpos <- cpos_dt
  retval@left <- if (is.character(left)) 0L else as.integer(left)
  retval@right <- if (is.character(right)) 0L else as.integer(right)
  retval@boundary <- if (is.null(boundary)) character() else boundary
  retval@p_attribute <- p_attribute
  
  retval
})


#' @rdname context-method
setMethod("context", "corpus", function(
  .Object, query, cqp = is.cqp,
  p_attribute = getOption("polmineR.p_attribute"),
  region = NULL, boundary = NULL,
  left = getOption("polmineR.left"), right = getOption("polmineR.right"),
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  count = TRUE,
  mc = getOption("polmineR.mc"), verbose = FALSE, progress = TRUE,
  ...
){
  
  if ("pAttribute" %in% names(list(...))){
    lifecycle::deprecate_warn(
      when = "0.8.7", 
      what = "context(pAttribute)",
      with = "context(p_attribute)"
    )
    p_attribute <- list(...)[["pAttribute"]]
  }
  if ("sAttribute" %in% names(list(...))){
    lifecycle::deprecate_warn(
      when = "0.8.7", 
      what = "context(sAttribute)",
      with = "context(boundary)"
    )
    boundary <- list(...)[["sAttribute"]]
  }

  p <- as(.Object, "partition")
  
  # There is a potential overhead of performing the count here: When context-method
  # is called by kwic, the count is not needed. However, if context is called by
  # cooccurrences, it is much, much faster (for one p-attribute) having done the 
  # count here for the entire corpus.
  
  if (length(p_attribute) == 1L){
    p@stat <- count(.Object@corpus, p_attribute = p_attribute, decode = FALSE)@stat
  }
    
  context(
    p, query = query, cqp = is.cqp,
    left = left, right = right,
    p_attribute = p_attribute,
    region = region, boundary = boundary,
    stoplist = stoplist, positivelist = positivelist, regex = regex,
    count = count, mc = mc, verbose = verbose, progress = progress
  )
})


#' @rdname context-method
setMethod("context", "character", function(
  .Object, query, cqp = is.cqp,
  p_attribute = getOption("polmineR.p_attribute"),
  region = NULL, boundary = NULL,
  left = getOption("polmineR.left"), right = getOption("polmineR.right"),
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  count = TRUE,
  mc = getOption("polmineR.mc"), verbose = FALSE, progress = TRUE,
  ...
){
  context(
    .Object = corpus(.Object),
    query = query,
    cqp = cqp,
    p_attribute = p_attribute,
    region = region, boundary = boundary,
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
setMethod("context", "partition_bundle", function(.Object, query, p_attribute, stoplist = NULL, positivelist = NULL, regex = FALSE, verbose = TRUE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]

  # Turn tokens on positivelist into ids once for entire corpus to avoid doing
  # this for every single object.
  if (!is.null(positivelist) && !is.numeric(positivelist)){
    if (regex){
      positivelist <- unlist(lapply(
        positivelist,
        function(x) regex2id(x = .Object, p_attribute = p_attribute, regex = x)
      ))
    } else {
      positivelist <- str2id(
        x = .Object, p_attribute = p_attribute, str = positivelist
      )
    }
  }
  
  # Same for stoplist.
  if (!is.null(stoplist) && !is.numeric(stoplist)){
    if (regex){
      stoplist <- unlist(lapply(
        stoplist,
        function(x) regex2id(x = .Object, p_attribute = p_attribute, regex = x)
      ))
    } else {
      stoplist <- str2id(
        x = .Object, p_attribute = p_attribute, str = stoplist
      )
    }
  }
  
  y <- as(as(.Object, "corpus"), "context_bundle")
  y@query <- query
  y@p_attribute <- p_attribute

  y@objects <- sapply(
    .Object@objects,
    function(x) {
      .message("get context for partition ", x@name, verbose = verbose)
      context(
        x, query = query, p_attribute = p_attribute,
        positivelist = positivelist, stoplist = stoplist,
        verbose = FALSE,
        ...
      )
    },
    simplify = TRUE,
    USE.NAMES = TRUE
  )
  
  # Remove NULL objects that result of no match has been obtained
  for (i in rev(which(sapply(y@objects, is.null)))) y@objects[[i]] <- NULL
  
  y
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
    registry_dir = .Object@registry_dir,
    data_dir = .Object@data_dir,
    template = .Object@template,
    info_file = .Object@info_file,
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
  
  sizes_partition <- unname(sapply(object@objects, slot, "size_partition"))
  counts <- unname(sapply(object@objects, slot, "count"))
  y <- data.frame(count = counts, freq = counts / sizes_partition)
  
  method <- unlist(unique(unname(sapply(object@objects, slot, "method"))))
  if (length(method) > 0L){
    y <- cbind(
      y,
      t(
        data.frame(lapply(object@objects, function(x) .statisticalSummary(x)$no))
      )
    )
    colnames(y)[3:6] <- c(">10.83", ">7.88", ">6.63", ">3.84")
  }
  
  if (top > 0L){
    y <- cbind(
      y,
      t(
        data.frame(
          lapply(
            object@objects,
            function(x) x@stat[[object@p_attribute]][1:top]
          )
        )
      )
    )
  }
  
  y
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
    registry_dir = from@registry_dir,
    info_file = from@info_file,
    template = from@template,
    stat = data.table(),
    encoding = from@encoding,
    cpos = from@cpos
  )
})


