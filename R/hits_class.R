#' Hits Class
#' 
#' Backend for dispersion method.
#' 
#' @slot dt a \code{"data.table"}
#' @slot corpus a \code{"character"} vector
#' @slot query Object of class \code{"character"}
#' @param query character vector
#' @param cqp either logical (TRUE if query is a CQP query), or a
#'   function to check whether query is a CQP query or not
#' @param sAttribute s-attributes
#' @param pAttribute p-attribute (will be passed into cpos)
#' @param size logical
#' @param freq locial
#' @param x a hits object
#' @param .Object a character, partition or partitionBundle object
#' @param mc logical
#' @param progress logical
#' @param verbose logical
#' @param ... further parameters
#' @exportClass hits
#' @rdname hits
setClass("hits",
         representation(
           dt="data.table",
           corpus="character",
           query="character"
         )
)


#' @rdname hits
#' @exportMethod hits
setGeneric("hits", function(.Object, ...) standardGeneric("hits"))

#' @rdname hits
setMethod("hits", "character", function(.Object, query, cqp = FALSE, sAttribute = NULL, pAttribute = "word", size = FALSE, freq = FALSE, mc = FALSE, verbose = TRUE, progress = TRUE){
  stopifnot(.Object %in% CQI$list_corpora())
  # check availability of sAttributes before proceeding
  if (!is.null(sAttribute)) {
    stopifnot(all(sAttribute %in% sAttributes(.Object)))
    sAttrs <- paste(.Object, sAttribute, sep=".")
    names(sAttrs) <- sAttribute
  }
  corpusPositions <- blapply(
    as.list(query), f=cpos,
    .Object=.Object, cqp=cqp, pAttribute=pAttribute,
    verbose=F, mc=mc, progress=progress
    )
  corpusPositions <- lapply(corpusPositions, function(x) x[,1])
  names(corpusPositions) <- query
  for (i in c(length(query):1)) {
    if (is.null(corpusPositions[[i]])) corpusPositions[[i]] <- NULL
  }
  DT <- data.table(cpos=unlist(corpusPositions))
  DT[, query := unlist(lapply(names(corpusPositions), function(x) rep(x, times=length(corpusPositions[[x]]))))]
  if (!is.null(sAttribute)){
    for (i in c(1:length(sAttribute))){
      DT[, eval(sAttribute[i]) := CQI$struc2str(.Object, sAttribute[i], CQI$cpos2struc(.Object, sAttribute[i], DT[["cpos"]]))]
    }
    count <- function(x) return(x)
    TF <- DT[, count(.N), by=c(eval(c("query", sAttribute))), with=TRUE]
    setnames(TF, old="V1", new="count")
    if (freq == TRUE) size <- TRUE
    if (size == TRUE){
      if (verbose) message("... getting sizes")
      META <- as.data.table(
        lapply(setNames(sAttribute, sAttribute), function(sAttr) CQI$struc2str(.Object, sAttr, c(1:(CQI$attribute_size(.Object, sAttr) -1))))
      )
      cposMatrix <- do.call(
        rbind,
        lapply(
          c(1:(CQI$attribute_size(.Object, sAttribute[1]) -1)),
          function(x) CQI$struc2cpos(.Object, sAttribute[1], x))
        )
      META[, size := cposMatrix[,2] - cposMatrix[,1] + 1]
      SIZE <- META[, sum(size), by=eval(sAttribute), with=TRUE]
      setkeyv(SIZE, cols=sAttribute)
      setkeyv(TF, cols=sAttribute)
      TF <- TF[SIZE]
      TF <- TF[is.na(TF[["query"]]) == FALSE]
      setnames(TF, old="V1", new="size")
      if (freq == TRUE){
        if (verbose) message("... frequencies")
        TF[, freq := count / size]
      }
    }
  } else {
    TF <- DT
  }
  new("hits", dt=TF, corpus=.Object, query=query)
})


#' @rdname hits
setMethod("hits", "partition", function(.Object, query, cqp = FALSE, sAttribute = NULL, pAttribute = "word", size = FALSE, freq = FALSE, mc = FALSE, progress = FALSE, verbose = TRUE){
  stopifnot(all(sAttribute %in% sAttributes(.Object@corpus)))
  if (freq == TRUE) size <- TRUE
  sAttrs <- paste(.Object@corpus, sAttribute, sep=".")
  DT <- hits(.Object@corpus, query = query, cqp = cqp, sAttribute = NULL, pAttribute = pAttribute, mc = mc, progress = progress)@dt
  DT[, "struc" := CQI$cpos2struc(.Object@corpus, .Object@sAttributeStrucs, DT[["cpos"]]), with=TRUE]
  DT <- subset(DT, DT[["struc"]] %in% .Object@strucs)
  if (!is.null(sAttribute)){
    for (i in c(1:length(sAttribute))){
      DT[, eval(sAttribute[i]) := CQI$struc2str(.Object@corpus, sAttribute[i], CQI$cpos2struc(.Object@corpus, sAttribute[i], DT[["cpos"]]))]
    }
    count <- function(x) ifelse(is.na(x), 0, x)
    TF <- DT[, count(.N), by=c(eval(c("query", sAttribute))), with=TRUE]
    setnames(TF, old="V1", new="count")
    if (size == TRUE){
      META <- as.data.table(
        lapply(setNames(sAttribute, sAttribute), function(sAttr) CQI$struc2str(.Object@corpus, sAttr, .Object@strucs))
      )
      META[, size := .Object@cpos[,2] - .Object@cpos[,1] + 1]
      SIZE <- META[, sum(size), by=eval(sAttribute), with=TRUE]
      setkeyv(SIZE, cols=sAttribute)
      setkeyv(TF, cols=sAttribute)
      TF <- TF[SIZE]
      setnames(TF, old="V1", new="size")
      TF[, count := sapply(TF[["count"]], function(x) ifelse(is.na(x), 0, x))]
      if (freq == TRUE) TF[, freq := count / size]
    }
  } else {
    TF <- DT
    # if (size == TRUE) TF[, size := .Object@size]
    # if (freq == TRUE) TF[, freq := count / size]
  }
  new("hits", dt = TF, corpus = .Object@corpus, query = query)
})


#' @rdname hits
setMethod("hits", "partitionBundle", function(
  .Object, query, pAttribute = getOption("polmineR.pAttribute"), size = TRUE, freq = FALSE,
  mc = getOption("polmineR.mc"), progress = FALSE, verbose = TRUE
  ){
  corpus <- unique(unlist(lapply(.Object@objects, function(x) x@corpus)))
  if (length(corpus) == 1){
    corpusEncoding <- .Object@objects[[1]]@encoding
    sAttributeStrucs <- unique(unlist(lapply(.Object@objects, function(x) x@sAttributeStrucs)))
    stopifnot(length(sAttributeStrucs) == 1)
    # combine strucs and partition names into an overall data.table
    if (verbose == TRUE) message("... preparing struc table")
    strucDT <- data.table(
      struc=unlist(lapply(.Object@objects, function(x) x@strucs)),
      partition=unlist(lapply(.Object@objects, function(x) rep(x@name, times=length(x@strucs))))
    )
    setkey(strucDT, cols="struc")
    # perform counts
    if (verbose == TRUE) message("... performing counts")
    .query <- function(toFind, corpus, encoding, ...) {
      cposMatrix <- cpos(.Object=corpus, query=toFind, encoding=encoding)
      if (!is.null(cposMatrix)){
        dt <- data.table(cposMatrix)
        dt[, query := toFind]
        return(dt)
      } else {
        return(NULL)
      }
    }
    countDTlist <- blapply(
      as.list(query), f=.query,
      corpus=corpus, encoding=corpusEncoding,
      mc=mc, progress=progress, verbose=F
      )
    countDT <- rbindlist(countDTlist)
    if (verbose == TRUE) message("... matching data.tables")
    countDT[, "struc" := CQI$cpos2struc(corpus, sAttributeStrucs, countDT[["V1"]]), with=TRUE]
    setkeyv(countDT, cols="struc")
    DT <- strucDT[countDT] # merge
    # DT[, "dummy" := 1, with=TRUE]
    # count <- function(x) return(x)
    # TF <- DT[, count(.N), by=c("partition", "query"), with=TRUE]
    TF <- DT[, .N, by=c("partition", "query")]
    # TF <- DT[, count(.N), by=.(partition, query)]
    # setnames(TF, old="V1", new="count")
    setnames(TF, old="N", new="count")
    if (freq == TRUE) size <- TRUE
    if (size == TRUE){
      TF[, size := sapply(.Object@objects, function(x) x@size)[TF[["partition"]]]]
    }
    if (freq == TRUE) TF[, freq := count / size]
    return(new("hits", dt=TF, corpus=corpus))
  } else {
    message("not implemented")
  }
})

#' @rdname hits
setMethod("sample", "hits", function(x, size){
  if (size > nrow(x@dt)) size <- nrow(x@dt)
  new(
    "hits",
    dt=x@dt[sample(c(1:nrow(x@dt)), size=size)],
    corpus=x@corpus, query=x@query
  )
})
