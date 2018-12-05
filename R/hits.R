#' @include S4classes.R
NULL

#' Get Hits.
#' 
#' Get hits for a (set of) queries, optionally with s-attribute values.
#' 
#' If the query character vector is named, the names of the query occurr in
#' the data.table that is returned rather than the queries.
#' 
#' If freq is TRUE, the data.table returned in the DT-slot will deliberately
#' include the subsets of the partition/corpus with no hits (query is NA,
#' count is 0).
#' @param query a (optionally named, see datails) character vector with one or more queries
#' @param cqp either logical (TRUE if query is a CQP query), or a
#'   function to check whether query is a CQP query or not
#' @param check A \code{logical} value, whether to check validity of CQP query
#'   using \code{check_cqp_query}.
#' @param s_attribute s-attributes
#' @param p_attribute p-attribute
#' @param size logical - return size of subcorpus
#' @param freq logcial - return relative frequencies
#' @param .Object a character, \code{partition} or \code{partition_bundle} object
#' @param mc logical, whether to use multicore
#' @param progress logical, whether to show progress bar
#' @param verbose logical
#' @param ... further parameters
#' @rdname hits
#' @exportMethod hits
setGeneric("hits", function(.Object, ...) standardGeneric("hits"))

#' @rdname hits
setMethod("hits", "character", function(.Object, query, cqp = FALSE, check = TRUE, s_attribute = NULL, p_attribute = "word", size = FALSE, freq = FALSE, mc = FALSE, verbose = TRUE, progress = TRUE, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]

  stopifnot(.Object %in% CQI$list_corpora(), length(.Object) == 1)
  if (!is.null(s_attribute)) stopifnot(all(s_attribute %in% s_attributes(.Object)))
  
  cpos_list <- blapply(
    x = as.list(query),
    f = function(query, .Object, cqp, p_attribute){
      cpos(.Object = .Object, query = query, cqp = cqp, check = check, p_attribute = p_attribute)
      },
    .Object = .Object, cqp = cqp, p_attribute = p_attribute,
    verbose = FALSE, mc = mc, progress = progress
  )
  
  if (is.null(names(query))) names(cpos_list) <- query else names(cpos_list) <- names(query)

  for (i in length(query):1){
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
    for (i in 1:length(s_attribute)){
      sAttributeValues <- CQI$struc2str(.Object, s_attribute[i], CQI$cpos2struc(.Object, s_attribute[i], DT[["cpos_left"]]))
      sAttributeValues <- as.nativeEnc(sAttributeValues, from = registry_get_encoding(.Object))
      DT[, eval(s_attribute[i]) := sAttributeValues]
    }
    TF <- DT[, .N, by = c(eval(c("query", s_attribute))), with = TRUE]
    setnames(TF, old = "N", new = "count")
    
    if (freq) size <- TRUE
    if (size){
      .message("getting sizes", verbose = verbose)
      SIZE <- size(.Object, s_attribute = s_attribute)
      setkeyv(TF, cols = s_attribute)
      TF <- TF[SIZE]
      TF <- TF[is.na(TF[["query"]]) == FALSE]
      if (freq == TRUE){
        .message("frequencies", verbose = verbose)
        TF[, freq := count / size]
      }
    }
  } else {
    TF <- DT
  }
  new("hits", stat = TF, corpus = .Object, query = query)
})


#' @rdname hits
setMethod("hits", "partition", function(.Object, query, cqp = FALSE, s_attribute = NULL, p_attribute = "word", size = FALSE, freq = FALSE, mc = FALSE, progress = FALSE, verbose = TRUE, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  stopifnot(all(s_attribute %in% s_attributes(.Object@corpus)))
  if (freq) size <- TRUE
  
  DT <- hits(.Object@corpus, query = query, cqp = cqp, s_attribute = NULL, p_attribute = p_attribute, mc = mc, progress = progress)@stat
  DT[, "struc" := CQI$cpos2struc(.Object@corpus, .Object@s_attribute_strucs, DT[["cpos_left"]]), with = TRUE]
  DT <- subset(DT, DT[["struc"]] %in% .Object@strucs)
  
  if (!is.null(s_attribute)){
    for (i in 1L:length(s_attribute)){
      sAttrString <- CQI$struc2str(.Object@corpus, s_attribute[i], CQI$cpos2struc(.Object@corpus, s_attribute[i], DT[["cpos_left"]]))
      sAttrString <- as.nativeEnc(sAttrString, from = .Object@encoding)
      DT[, eval(s_attribute[i]) := sAttrString]
    }
    TF <- DT[, .N, by = c(eval(c("query", s_attribute))), with = TRUE]
    setnames(TF, old = "N", new = "count")
    if (size){
      SIZE <- size(.Object, s_attribute = s_attribute)
      setkeyv(TF, cols = s_attribute)
      TF <- TF[SIZE]
      TF[, count := sapply(TF[["count"]], function(x) ifelse(is.na(x), 0, x))]
      if (freq) TF[, freq := count / size]
    }
  } else {
    TF <- DT
  }
  new("hits", stat = TF, corpus = .Object@corpus, query = query)
})


#' @rdname hits
setMethod("hits", "partition_bundle", function(
  .Object, query, cqp = FALSE, check = TRUE, p_attribute = getOption("polmineR.p_attribute"), size = TRUE, freq = FALSE,
  mc = getOption("polmineR.mc"), progress = FALSE, verbose = TRUE, ...
){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]

  corpus <- unique(unlist(lapply(.Object@objects, function(x) x@corpus)))
  if (length(corpus) > 1) stop("partiton_bundle not derived from one corpus")
  corpusEncoding <- .Object@objects[[1]]@encoding
  s_attribute_strucs <- unique(unlist(lapply(.Object@objects, function(x) x@s_attribute_strucs)))
  stopifnot(length(s_attribute_strucs) == 1)
  # combine strucs and partition names into an overall data.table
  .message("preparing struc table", verbose = verbose)
  strucDT <- data.table(
    struc = unlist(lapply(.Object@objects, function(x) x@strucs)),
    partition = unlist(lapply(.Object@objects, function(x) rep(x@name, times = length(x@strucs))))
  )
  setkeyv(strucDT, cols = "struc")
  # perform counts
  
  .message("now performing counts", verbose = verbose)
  if (any(is.na(query))) stop("Please check your queries - there is an NA among them!")
  .query <- function(toFind, corpus, encoding, ...) {
    cposMatrix <- cpos(.Object = corpus, query = toFind, cqp = cqp, check = check, encoding = encoding, verbose = verbose)
    if (!is.null(cposMatrix)){
      dt <- data.table(cposMatrix)
      dt[, query := toFind]
      return(dt)
    } else {
      return(NULL)
    }
  }
  countDTlist <- blapply(
    as.list(query), f = .query,
    corpus = corpus, encoding = corpusEncoding,
    mc = mc, progress = progress, verbose = F
  )
  countDT <- rbindlist(countDTlist)
  .message("matching data.tables", verbose = verbose)
  countDT[, "struc" := CQI$cpos2struc(corpus, s_attribute_strucs, countDT[["V1"]]), with = TRUE]
  countDT[, "V1" := NULL, with = TRUE][, "V2" := NULL, with = TRUE]
  setkeyv(countDT, cols = "struc")
  setkeyv(strucDT, cols = "struc")
  DT <- strucDT[countDT] # merge
  nas <- which(is.na(DT[["partition"]]) == TRUE)
  if (length(nas) > 0) DT <- DT[-nas] # remove hits that are not in partition_bundle
  TF <- DT[, .N, by = c("partition", "query")]
  setnames(TF, old = "N", new = "count")
  if (freq) size <- TRUE
  if (size){
    TF[, size := sapply(.Object@objects, function(x) x@size)[TF[["partition"]]]]
  }
  if (freq == TRUE) TF[, freq := count / size]
  new("hits", stat = TF, corpus = corpus)
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
  DT <- ctxtMin[, .makeCpos(.SD), by = "hit_no"]
  
  .message("adding s_attributes", verbose = verbose)
  for (x in s_attribute){
    strucs <- CQI$cpos2struc(.Object@corpus, x, DT[["cpos_left"]])
    str <- CQI$struc2str(.Object@corpus, x, strucs)
    DT[, eval(x) := str]
  }
  
  new("hits", stat = DT, corpus = .Object@corpus, query = .Object@query)
})
