#' @include S4classes.R
NULL

#' Get hits for query
#' 
#' Get hits for queries, optionally with s-attribute values.
#' 
#' If the \code{character} vector provided by \code{query} is named, these names
#' will be reported in the \code{data.table} that is returned rather than the
#' queries.
#' 
#' If \code{freq} is \code{TRUE}, the \code{data.table} returned in the DT-slot will deliberately
#' include the subsets of the \code{partition}/\code{corpus} with no hits (query is NA,
#' count is 0).
#' 
#' @param query A `character` vector (optionally named, see details) with one or
#'   more queries.
#' @param cqp Either a \code{logical} value (\code{TRUE} if query is a CQP
#'   query), or a function to check whether \code{query} is a CQP query or not.
#' @param check A \code{logical} value, whether to check validity of CQP query
#'   using \code{check_cqp_query}.
#' @param s_attribute A \code{character} vector of s-attributes that will be
#'   used to breakdown counts for matches for query/queries.
#' @param p_attribute A \code{character} vector stating a p-attribute.
#' @param size A \code{logical} value, whether to report the size of subcorpus.
#' @param freq A \code{logcial} value, whether to report relative frequencies.
#' @param fill A \code{logical} value, whethet to report counts (optionally
#'   frequencies) for combinations of s-attributes where not matchers occurr.
#' @param .Object A length-one \code{character} vector with a corpus ID, a
#'   \code{partition} or \code{partition_bundle} object
#' @param mc A \code{logical} value, whether to use multicore.
#' @param progress A \code{logical} value, whether to show progress bar.
#' @param verbose A \code{logical} value, whether to output messages.
#' @param ... Further arguments (used for backwards compatibility).
#' @return A \code{hits} class object.
#' @seealso See the documentation of the \code{hits} class
#'   (\code{\link{hits-class}}) for details.
#' @rdname hits
#' @exportMethod hits
setGeneric("hits", function(.Object, ...) standardGeneric("hits"))

#' @rdname hits
#' @examples
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' 
#' # get hits for corpus object
#' y <- corpus("REUTERS") %>% hits(query = "oil")
#' y <- corpus("REUTERS") %>% hits(query = c("oil", "barrel"))
#' y <- corpus("REUTERS") %>% hits(query = "oil", s_attribute = "places", freq = TRUE)
#' 
#' # specify corpus by corpus ID
#' y <- hits("REUTERS", query = "oil")
#' y <- hits("REUTERS", query = "oil", s_attribute = "places", freq = TRUE)
#' 
#' # get hits for partition
#' p <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
#' y <- hits(p, query = "oil", s_attribute = "id")
#' 
#' # get hits for subcorpus
#' y <- corpus("REUTERS") %>%
#'   subset(grep("saudi-arabia", places)) %>%
#'   hits(query = "oil")
setMethod("hits", "corpus", function(.Object, query, cqp = FALSE, check = TRUE, s_attribute, p_attribute = "word", size = FALSE, freq = FALSE, fill = FALSE, mc = 1L, verbose = TRUE, progress = FALSE, ...){
  
  if (is.logical(mc)) if (mc) mc <- getOption("polmineR.cores") else mc <- 1L
  
  if ("sAttribute" %in% names(list(...))){
    lifecycle::deprecate_warn(
      when = "0.8.7", 
      what = "hits(sAttribute)",
      with = "hits(s_attribute)"
    )
    s_attribute <- list(...)[["sAttribute"]]
  }
  if ("pAttribute" %in% names(list(...))){
    lifecycle::deprecate_warn(
      when = "0.8.7", 
      what = "hits(pAttribute)",
      with = "hits(p_attribute)"
    )
    p_attribute <- list(...)[["pAttribute"]]
  }
  
  if (missing(s_attribute)){
    dt <- count(
      .Object = .Object,
      query = query, cqp = cqp, check = check, p_attribute = p_attribute,
      freq = freq, mc = mc, verbose = verbose, progress = progress, ...
    )
    if (!is.null(names(query))){
      if (any(nchar(names(query)) == 0L))
        stop("if query is a named vector, all queries need to be named")
      q <- unname(setNames(names(query), unname(query))[dt[["query"]]])
      dt[, "query" := q]
    }
    retval <- as(as(.Object, "corpus"), "hits")
    retval@stat <- dt
    retval@corpus <- .Object@corpus
    retval@query <- query
    
    return(retval)
  }
  
  if (!is.null(s_attribute))
    stopifnot(all(s_attribute %in% s_attributes(.Object)))
  
  rngs <- ranges(
    .Object,
    query = query, cqp = cqp, p_attribute = p_attribute,
    mc = mc, progress = progress
  )
  DT <- as.data.table(rngs)

  for (i in seq_along(s_attribute)){
    strucs <- cl_cpos2struc(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      s_attribute = s_attribute[i], cpos = DT[["cpos_left"]]
    )
    s_attr_values <- cl_struc2str(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      s_attribute = s_attribute[i], struc = strucs
    )
    DT[, eval(s_attribute[i]) := as.nativeEnc(s_attr_values, from = .Object@encoding)]
  }
  TF <- DT[, .N, by = c(eval(c("query", s_attribute))), with = TRUE]
  setnames(TF, old = "N", new = "count")
  
  if (fill){
    # Using s_attributes() requires decoding s-attributes and is potentially not the fastest solution,
    # but it ensures that only combinations that do occurr are reported
    s_attr_dt <- s_attributes(.Object = .Object, s_attribute = s_attribute, unique = TRUE)
    .fn <- function(.SD){
      dt <- .SD[s_attr_dt, on = s_attribute]
      dt[, "count" := ifelse(is.na(dt[["count"]]), 0L, dt[["count"]])]
    }
    TF <- TF[, .fn(.SD), by = "query", .SDcols = c(s_attribute, "count")]
  }
  
  if (freq) size <- TRUE
  if (size){
    .message("getting sizes", verbose = verbose)
    SIZE <- size(.Object, s_attribute = s_attribute)
    setkeyv(TF, cols = s_attribute)
    TF <- TF[SIZE]
    if (isFALSE(fill)) TF <- TF[!is.na(TF[["query"]])]
    TF[, "count" := ifelse(is.na(TF[["count"]]), 0L, TF[["count"]])]
    if (freq){
      .message("frequencies", verbose = verbose)
      TF[, "freq" := TF[["count"]] / TF[["size"]]]
    }
  }
  retval <- as(as(.Object, "corpus"), "hits")
  retval@stat = TF
  retval@corpus = .Object@corpus
  retval@query = query
  retval
})


#' @rdname hits
setMethod("hits", "character", function(.Object, query, cqp = FALSE, check = TRUE, s_attribute, p_attribute = "word", size = FALSE, freq = FALSE, mc = FALSE, verbose = TRUE, progress = TRUE, ...){
  hits(
    .Object = corpus(.Object),
    query = query, cqp = cqp, check = check,
    s_attribute = s_attribute, p_attribute = p_attribute,
    size = size, freq = freq,
    mc = mc, verbose = verbose, progress = progress,
    ...)
})



#' @rdname hits
setMethod("hits", "subcorpus", function(.Object, query, cqp = FALSE, check = TRUE, s_attribute, p_attribute = "word", size = FALSE, freq = FALSE, fill = FALSE, mc = FALSE, progress = FALSE, verbose = TRUE, ...){
  callNextMethod()
})


#' @rdname hits
setMethod("hits", "partition", function(.Object, query, cqp = FALSE, check = TRUE, s_attribute, p_attribute = "word", size = FALSE, freq = FALSE, fill = FALSE, mc = FALSE, progress = FALSE, verbose = TRUE, ...){
  hits(
    .Object = as(.Object, "subcorpus"),
    query = query,
    cqp = cqp,
    check = check,
    s_attribute = s_attribute,
    p_attribute = p_attribute,
    size = size, 
    freq = freq,
    fill = fill,
    mc = mc, 
    progress = progress,
    verbose
  )
})

#' @rdname hits
setMethod("hits", "partition_bundle", function(
  .Object, query, cqp = FALSE, check = TRUE, p_attribute = getOption("polmineR.p_attribute"), s_attribute, size = TRUE, freq = FALSE,
  mc = getOption("polmineR.mc"), progress = FALSE, verbose = TRUE, ...
){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]

  corpus_id <- unique(unlist(lapply(.Object@objects, function(x) x@corpus)))
  if (length(corpus_id) > 1L) stop("partiton_bundle not derived from one corpus")
  corpus_obj <- corpus(corpus_id)
  s_attribute_strucs <- unique(unlist(lapply(.Object@objects, function(x) x@s_attribute_strucs)))
  stopifnot(length(s_attribute_strucs) == 1L)
  
  # combine strucs and partition names into an overall data.table
  .message("preparing struc table", verbose = verbose)
  struc_dt <- data.table(
    struc = unlist(lapply(.Object@objects, function(x) x@strucs)),
    partition = unlist(lapply(.Object@objects, function(x) rep(x@name, times = length(x@strucs))))
  )
  
  # perform counts
  .message("now performing counts", verbose = verbose)
  if (any(is.na(query))) stop("Please check your queries - there is an NA among them!")
  .fn <- function(q, corpus_obj, ...) {
    m <- cpos(.Object = corpus_obj, query = q, cqp = cqp, check = check, verbose = verbose)
    if (!is.null(m)) data.table(m)[, query := q] else NULL
  }
  count_dt_list <- blapply(as.list(query), f = .fn, corpus_obj = corpus_obj, mc = mc, progress = progress, verbose = FALSE)
  count_dt <- rbindlist(count_dt_list)
  
  .message("finalizing tables", verbose = verbose)
  if (nrow(count_dt) > 0L){
    strucs <- cl_cpos2struc(
      corpus = corpus_id, registry = corpus_registry_dir(corpus_id),
      s_attribute = s_attribute_strucs, cpos = count_dt[["V1"]]
    )
    count_dt[, "struc" := strucs, with = TRUE][, "V1" := NULL][, "V2" := NULL]
    dt <- struc_dt[count_dt, on = "struc"] # merge
    nas <- which(is.na(dt[["partition"]]) == TRUE)
    if (missing(s_attribute)) s_attribute <- NULL
    for (s_attr in s_attribute){
      values <- cl_struc2str(
        corpus = corpus_id, registry = corpus_registry_dir(corpus_id),
        s_attribute = s_attr, struc = dt[["struc"]]
      )
      dt[, (s_attr) := as.nativeEnc(values, from = .Object@objects[[1]]@encoding)]
    }
    if (length(nas) > 0) dt <- dt[-nas] # remove hits that are not in partition_bundle
    tf <- dt[, .N, by = c(c("partition", "query"), s_attribute)]
    setnames(tf, old = "N", new = "count")
    if (freq) size <- TRUE
    if (size) tf[, size := sapply(.Object@objects, function(x) x@size)[tf[["partition"]]]]
    if (freq) tf[, freq := count / size]
  } else {
    tf <- data.table(partition = character(), query = character(), N = integer())
    if (size) tf[, "size" := integer()]
    if (freq) tf[, "freq" := numeric()]
  }
  new("hits", stat = tf, corpus = corpus_id)
})

#' @param x A \code{hits} object.
#' @param size A non-negative integer giving the number of items to choose.
#' @rdname hits_class
setMethod("sample", "hits", function(x, size){
  if (size > nrow(x@stat)){
    warning("size exceeds nrow of the data.table of the hits object")
    size <- nrow(x@stat)
  }
  new(
    "hits",
    dt = x@stat[sample(1L:nrow(x@stat), size = size)],
    corpus = x@corpus,
    registry_dir = x@registry_dir,
    data_dir = x@data_dir,
    info_file = x@info_file,
    template = x@template,
    query = x@query
  )
})

#' @rdname hits
setMethod("hits", "context", function(.Object, s_attribute = NULL, verbose = TRUE, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]

  ctxtMin <- .Object@cpos[which(.Object@cpos[["position"]] == 0)]
  
  .message("compressing data.table", verbose = verbose)
  .makeCpos <- function(.SD){
    list(
      cpos_left = min(.SD[["cpos"]]),
      cpos_right = max(.SD[["cpos"]])
      )
  }
  DT <- ctxtMin[, .makeCpos(.SD), by = "match_id"]
  
  .message("adding s_attributes", verbose = verbose)
  for (x in s_attribute){
    strucs <- cl_cpos2struc(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      s_attribute = x, cpos = DT[["cpos_left"]]
    )
    str <- cl_struc2str(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      s_attribute = x, struc = strucs
    )
    DT[, eval(x) := str]
  }
  
  new(
    "hits",
    stat = DT,
    corpus = .Object@corpus,
    registry_dir = .Object@registry_dir,
    data_dir = .Object@data_dir,
    info_file = .Object@info_file,
    template = .Object@template,
    query = .Object@query
  )
})

#' @rdname hits
setMethod("hits", "remote_corpus", function(.Object, ...){
  ocpu_exec(fn = "hits", corpus = .Object@corpus, server = .Object@server, restricted = .Object@restricted, .Object = as(.Object, "corpus"), ...)
})

#' @rdname hits
setMethod("hits", "remote_subcorpus", function(.Object, ...){
  ocpu_exec(fn = "hits", corpus = .Object@corpus, server = .Object@server, restricted = .Object@restricted, .Object = as(.Object, "subcorpus"), ...)
})
