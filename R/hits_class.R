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
#' 
#' @slot stat a \code{"data.table"}
#' @slot corpus a \code{"character"} vector
#' @slot query Object of class \code{"character"}
#' @slot pAttribute p-attribute that has been queried
#' @slot encoding encoding of the corpus
#' @slot name name of the object
#' @param query a (optionally named, see datails) character vector with one or more queries
#' @param cqp either logical (TRUE if query is a CQP query), or a
#'   function to check whether query is a CQP query or not
#' @param sAttribute s-attributes
#' @param pAttribute p-attribute (will be passed into cpos)
#' @param size logical - return size of subcorpus
#' @param freq logcial - return relative frequencies
#' @param x a hits object
#' @param .Object a character, partition or partitionBundle object
#' @param mc logical, whether to use multicore
#' @param progress logical, whether to show progress bar
#' @param verbose logical
#' @param ... further parameters
#' @exportClass hits
#' @rdname hits
setClass(
  "hits",
  representation(query = "character"),
  contains = "textstat"
)


#' @rdname hits
#' @exportMethod hits
setGeneric("hits", function(.Object, ...) standardGeneric("hits"))

#' @rdname hits
setMethod("hits", "character", function(.Object, query, cqp = FALSE, sAttribute = NULL, pAttribute = "word", size = FALSE, freq = FALSE, mc = FALSE, verbose = TRUE, progress = TRUE){
  stopifnot(.Object %in% CQI$list_corpora(), length(.Object) == 1)
  if (!is.null(sAttribute)) stopifnot(all(sAttribute %in% sAttributes(.Object)))
  
  cpos_list <- blapply(
    x = as.list(query),
    f = function(query, .Object, cqp, pAttribute){
      cpos(.Object = .Object, query = query, cqp = cqp, pAttribute = pAttribute)
      },
    .Object = .Object, cqp = cqp, pAttribute = pAttribute,
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
  
  if (!is.null(sAttribute)){
    for (i in 1:length(sAttribute)){
      sAttributeValues <- CQI$struc2str(.Object, sAttribute[i], CQI$cpos2struc(.Object, sAttribute[i], DT[["cpos_left"]]))
      sAttributeValues <- as.nativeEnc(sAttributeValues, from = getEncoding(.Object))
      DT[, eval(sAttribute[i]) := sAttributeValues]
    }
    TF <- DT[, .N, by = c(eval(c("query", sAttribute))), with = TRUE]
    setnames(TF, old = "N", new = "count")
    
    if (freq) size <- TRUE
    if (size){
      .message("getting sizes", verbose = verbose)
      SIZE <- size(.Object, sAttribute = sAttribute)
      setkeyv(TF, cols = sAttribute)
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
setMethod("hits", "partition", function(.Object, query, cqp = FALSE, sAttribute = NULL, pAttribute = "word", size = FALSE, freq = FALSE, mc = FALSE, progress = FALSE, verbose = TRUE){
  stopifnot(all(sAttribute %in% sAttributes(.Object@corpus)))
  if (freq) size <- TRUE
  
  DT <- hits(.Object@corpus, query = query, cqp = cqp, sAttribute = NULL, pAttribute = pAttribute, mc = mc, progress = progress)@stat
  DT[, "struc" := CQI$cpos2struc(.Object@corpus, .Object@sAttributeStrucs, DT[["cpos_left"]]), with = TRUE]
  DT <- subset(DT, DT[["struc"]] %in% .Object@strucs)
  
  if (!is.null(sAttribute)){
    for (i in c(1:length(sAttribute))){
      sAttrString <- CQI$struc2str(.Object@corpus, sAttribute[i], CQI$cpos2struc(.Object@corpus, sAttribute[i], DT[["cpos_left"]]))
      sAttrString <- as.nativeEnc(sAttrString, from = .Object@encoding)
      DT[, eval(sAttribute[i]) := sAttrString]
    }
    TF <- DT[, .N, by = c(eval(c("query", sAttribute))), with = TRUE]
    setnames(TF, old = "N", new = "count")
    if (size){
      SIZE <- size(.Object, sAttribute = sAttribute)
      setkeyv(TF, cols = sAttribute)
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
setMethod("hits", "partitionBundle", function(
  .Object, query, cqp = FALSE, pAttribute = getOption("polmineR.pAttribute"), size = TRUE, freq = FALSE,
  mc = getOption("polmineR.mc"), progress = FALSE, verbose = TRUE
){
  corpus <- unique(unlist(lapply(.Object@objects, function(x) x@corpus)))
  if (length(corpus) > 1) stop("partitonBundle not derived from one corpus")
  corpusEncoding <- .Object@objects[[1]]@encoding
  sAttributeStrucs <- unique(unlist(lapply(.Object@objects, function(x) x@sAttributeStrucs)))
  stopifnot(length(sAttributeStrucs) == 1)
  # combine strucs and partition names into an overall data.table
  .message("preparing struc table", verbose = verbose)
  strucDT <- data.table(
    struc = unlist(lapply(.Object@objects, function(x) x@strucs)),
    partition = unlist(lapply(.Object@objects, function(x) rep(x@name, times = length(x@strucs))))
  )
  setkey(strucDT, cols = "struc")
  # perform counts
  
  .message("now performing counts", verbose = verbose)
  if (any(is.na(query))) stop("Please check your queries - there is an NA among them!")
  .query <- function(toFind, corpus, encoding, ...) {
    cposMatrix <- cpos(.Object = corpus, query = toFind, cqp = cqp, encoding = encoding, verbose = verbose)
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
  countDT[, "struc" := CQI$cpos2struc(corpus, sAttributeStrucs, countDT[["V1"]]), with = TRUE]
  countDT[, "V1" := NULL, with = TRUE][, "V2" := NULL, with = TRUE]
  setkeyv(countDT, cols = "struc")
  setkeyv(strucDT, cols = "struc")
  DT <- strucDT[countDT] # merge
  nas <- which(is.na(DT[["partition"]]) == TRUE)
  if (length(nas) > 0) DT <- DT[-nas] # remove hits that are not in partitionBundle
  TF <- DT[, .N, by = c("partition", "query")]
  setnames(TF, old = "N", new = "count")
  if (freq) size <- TRUE
  if (size){
    TF[, size := sapply(.Object@objects, function(x) x@size)[TF[["partition"]]]]
  }
  if (freq == TRUE) TF[, freq := count / size]
  new("hits", stat = TF, corpus = corpus)
})

#' @rdname hits
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
setMethod("hits", "context", function(.Object, sAttribute = NULL, verbose = TRUE){
  ctxtMin <- .Object@cpos[which(.Object@cpos[["position"]] == 0)]
  
  .message("compressing data.table", verbose = verbose)
  .makeCpos <- function(.SD){
    list(
      cpos_left = min(.SD[["cpos"]]),
      cpos_right = max(.SD[["cpos"]])
      )
  }
  DT <- ctxtMin[, .makeCpos(.SD), by = "hit_no"]
  
  .message("adding sAttributes", verbose = verbose)
  for (x in sAttribute){
    strucs <- CQI$cpos2struc(.Object@corpus, x, DT[["cpos_left"]])
    str <- CQI$struc2str(.Object@corpus, x, strucs)
    DT[, eval(x) := str]
  }
  
  new("hits", stat = DT, corpus = .Object@corpus, query = .Object@query)
})
