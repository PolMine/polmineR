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
#' @param query A \code{character} vector (optionally named, see details) with
#'   one or more queries.
#' @param cqp Either a \code{logical} value (\code{TRUE} if query is a CQP
#'   query), or a function to check whether \code{query} is a CQP query or not.
#' @param check A \code{logical} value, whether to check validity of CQP query
#'   using \code{check_cqp_query}.
#' @param s_attribute A \code{character} vector of s-attributes that will be
#'   reported as metadata.
#' @param p_attribute A \code{character} vector stating a p-attribute.
#' @param size A \code{logical} value, whether to report the size of subcorpus.
#' @param freq A \code{logcial} value, whether to report relative frequencies.
#' @param .Object A length-one \code{character} vector with a corpus ID, a \code{partition} or \code{partition_bundle} object
#' @param mc A \code{logical} value, whether to use multicore.
#' @param progress A \code{logical} value, whether to show progress bar.
#' @param verbose A \code{logical} value, whether to output messages.
#' @param ... Further arguments (used for backwards compatibility).
#' @rdname hits
#' @exportMethod hits
setGeneric("hits", function(.Object, ...) standardGeneric("hits"))

#' @rdname hits
#' @examples
#' use("polmineR")
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
#' # get hits for partition´
#' p <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
#' y <- hits(p, query = "oil")
#' y <- hits(p, query = "oil", s_attribute = "id")
#' 
#' # get hits for subcorpus
#' y <- corpus("REUTERS") %>%
#'   subset(grep("saudi-arabia", places)) %>%
#'   hits(query = "oil")
setMethod("hits", "corpus", function(.Object, query, cqp = FALSE, check = TRUE, s_attribute = NULL, p_attribute = "word", size = FALSE, freq = FALSE, mc = 1L, verbose = TRUE, progress = FALSE, ...){
  
  if (is.logical(mc)) if (mc) mc <- getOption("polmineR.cores") else mc <- 1L
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  if (!is.null(s_attribute)) stopifnot(all(s_attribute %in% s_attributes(.Object)))
  
  .fn <- function(q){
    cpos(.Object = .Object, query = q, cqp = cqp, check = check, p_attribute = p_attribute, verbose = FALSE)
  }
  cpos_list <- if (progress) pblapply(as.list(query), .fn, cl = mc) else mclapply(as.list(query), .fn, mc.cores = mc)

  if (is.null(names(query))) names(cpos_list) <- query else names(cpos_list) <- names(query)
  
  for (i in length(query):1L){
    if (is.null(cpos_list[[i]])){
      warning("no hits for query: ", query[i])
      cpos_list[[i]] <- NULL
    }
  }
  
  DT <- data.table(
    cpos_left = unlist(lapply(cpos_list, function(x) x[,1])),
    cpos_right = unlist(lapply(cpos_list, function(x) x[,2])),
    query = unlist(lapply(names(cpos_list), function(x) rep(x, times = nrow(cpos_list[[x]]))))
  )
  
  if (!is.null(s_attribute)){
    for (i in seq_along(s_attribute)){
      strucs <- cl_cpos2struc(corpus = .Object@corpus, s_attribute = s_attribute[i], cpos = DT[["cpos_left"]], registry = registry())
      s_attr_values <- cl_struc2str(corpus = .Object@corpus, s_attribute = s_attribute[i], struc = strucs)
      DT[, eval(s_attribute[i]) := as.nativeEnc(s_attr_values, from = .Object@encoding)]
    }
    TF <- DT[, .N, by = c(eval(c("query", s_attribute))), with = TRUE]
    setnames(TF, old = "N", new = "count")
    
    if (freq) size <- TRUE
    if (size){
      .message("getting sizes", verbose = verbose)
      SIZE <- size(.Object, s_attribute = s_attribute)
      setkeyv(TF, cols = s_attribute)
      TF <- TF[SIZE]
      TF[, "count" := ifelse(is.na(TF[["count"]]), 0L, TF[["count"]])]
      # TF <- TF[is.na(TF[["query"]]) == FALSE]
      if (freq){
        .message("frequencies", verbose = verbose)
        TF[, "freq" := TF[["count"]] / TF[["size"]]]
      }
    }
  } else {
    TF <- DT
  }
  new("hits", stat = TF, corpus = .Object@corpus, query = query)
})


#' @rdname hits
setMethod("hits", "character", function(.Object, query, cqp = FALSE, check = TRUE, s_attribute = NULL, p_attribute = "word", size = FALSE, freq = FALSE, mc = FALSE, verbose = TRUE, progress = TRUE, ...){
  hits(
    .Object = corpus(.Object),
    query = query, cqp = cqp, check = check,
    s_attribute = s_attribute, p_attribute = p_attribute,
    size = size, freq = freq,
    mc = mc, verbose = verbose, progress = progress,
    ...)
})


#' @rdname hits
setMethod("hits", "slice", function(.Object, query, cqp = FALSE, s_attribute = NULL, p_attribute = "word", size = FALSE, freq = FALSE, mc = FALSE, progress = FALSE, verbose = TRUE, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  stopifnot(all(s_attribute %in% s_attributes(.Object@corpus)))
  if (freq) size <- TRUE # not possible to calculate frequencies without sizes
  
  DT <- hits(.Object@corpus, query = query, cqp = cqp, s_attribute = NULL, p_attribute = p_attribute, mc = mc, progress = progress)@stat
  DT[, "struc" := cl_cpos2struc(corpus = .Object@corpus, s_attribute = .Object@s_attribute_strucs, cpos = DT[["cpos_left"]], registry = registry()), with = TRUE]
  DT <- subset(DT, DT[["struc"]] %in% .Object@strucs)
  
  if (!is.null(s_attribute)){
    for (i in 1L:length(s_attribute)){
      s_attr_values <- cl_struc2str(corpus = .Object@corpus, s_attribute = s_attribute[i], struc = cl_cpos2struc(corpus = .Object@corpus, s_attribute = s_attribute[i], cpos = DT[["cpos_left"]], registry = registry()), registry = registry())
      s_attr_values <- as.nativeEnc(s_attr_values, from = .Object@encoding)
      DT[, eval(s_attribute[i]) := s_attr_values]
    }
    TF <- DT[, .N, by = c(eval(c("query", s_attribute))), with = TRUE]
    setnames(TF, old = "N", new = "count")
    if (size){
      SIZE <- size(.Object, s_attribute = s_attribute)
      setkeyv(TF, cols = s_attribute)
      TF <- TF[SIZE]
      TF[, count := sapply(TF[["count"]], function(x) ifelse(is.na(x), 0, x))]
      if (freq) TF[, "freq" := count / size]
    }
  } else {
    TF <- DT
  }
  new("hits", stat = TF, corpus = .Object@corpus, query = query)
})

#' @rdname hits
setMethod("hits", "subcorpus", function(.Object, query, cqp = FALSE, s_attribute = NULL, p_attribute = "word", size = FALSE, freq = FALSE, mc = FALSE, progress = FALSE, verbose = TRUE, ...){
  callNextMethod()
})


#' @rdname hits
setMethod("hits", "partition", function(.Object, query, cqp = FALSE, s_attribute = NULL, p_attribute = "word", size = FALSE, freq = FALSE, mc = FALSE, progress = FALSE, verbose = TRUE, ...){
  callNextMethod()
})

#' @rdname hits
setMethod("hits", "partition_bundle", function(
  .Object, query, cqp = FALSE, check = TRUE, p_attribute = getOption("polmineR.p_attribute"), size = TRUE, freq = FALSE,
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
  strucs <- cl_cpos2struc(corpus = corpus_id, s_attribute = s_attribute_strucs, cpos = count_dt[["V1"]], registry = registry())
  count_dt[, "struc" := strucs, with = TRUE][, "V1" := NULL][, "V2" := NULL]
  setkeyv(count_dt, cols = "struc")
  setkeyv(struc_dt, cols = "struc")
  dt <- struc_dt[count_dt] # merge
  nas <- which(is.na(dt[["partition"]]) == TRUE)
  if (length(nas) > 0) dt <- dt[-nas] # remove hits that are not in partition_bundle
  tf <- dt[, .N, by = c("partition", "query")]
  setnames(tf, old = "N", new = "count")
  if (freq) size <- TRUE
  if (size) tf[, size := sapply(.Object@objects, function(x) x@size)[tf[["partition"]]]]
  if (freq) tf[, freq := count / size]
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
    corpus = x@corpus, query = x@query
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
    strucs <- cl_cpos2struc(corpus = .Object@corpus, s_attribute = x, cpos = DT[["cpos_left"]], registry = registry())
    str <- cl_struc2str(corpus = .Object@corpus, s_attribute = x, struc = strucs, registry = registry())
    DT[, eval(x) := str]
  }
  
  new("hits", stat = DT, corpus = .Object@corpus, query = .Object@query)
})
