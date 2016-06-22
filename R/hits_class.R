#' hits class
#' 
#' A potentially useful worker. 
#' 
#' @slot dt a \code{"data.table"}
#' @slot corpus a \code{"character"} vector
#' @slot query Object of class \code{"character"}
#' @param query character vector
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
setMethod("hits", "character", function(.Object, query, sAttribute=NULL, pAttribute="word", size=TRUE, freq=FALSE, mc=FALSE, verbose=TRUE, progress=TRUE){
  stopifnot(.Object %in% CQI$list_corpora())
  # check availability of sAttributes before proceeding
  if (!is.null(sAttribute)) {
    stopifnot(all(sAttribute %in% sAttributes(.Object)))
    sAttrs <- paste(.Object, sAttribute, sep=".")
    names(sAttrs) <- sAttribute
  }
  if (mc == FALSE){
    if (progress == TRUE) pb <- txtProgressBar(max=length(query), style=3)
    corpusPositions <- lapply(
      c(1:length(query)),
      function(i){
        if (progress == TRUE) setTxtProgressBar(pb, i)
        cpos(.Object, query[i], pAttribute=pAttribute, verbose=FALSE)[,1]
      })
  } else if (mc == TRUE){
    corpusPositions <- mclapply(
      c(1:length(query)),
      function(i){
        cpos(.Object, query[i], pAttribute=pAttribute, verbose=FALSE)[,1]
      }, mc.cores=getOption("polmineR.cores"))
  }
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
      META <- as.data.table(
        lapply(setNames(sAttribute, sAttribute), function(sAttr) CQI$struc2str(.Object, sAttr, c(1:(CQI$attribute_size(.Object, sAttr) -1))))
      )
      cposMatrix <- do.call(
        rbind,
        mclapply(
          c(1:(CQI$attribute_size(.Object, sAttribute[1]) -1)),
          function(x) CQI$struc2cpos(.Object, sAttribute[1], x), mc.cores=getOption("polmineR.cores"))
        )
      META[, size := cposMatrix[,2] - cposMatrix[,1] + 1]
      SIZE <- META[, sum(size), by=eval(sAttribute), with=TRUE]
      setkeyv(SIZE, cols=sAttribute)
      setkeyv(TF, cols=sAttribute)
      TF <- TF[SIZE]
      TF <- TF[is.na(TF[["query"]]) == FALSE]
      setnames(TF, old="V1", new="size")
      if (freq == TRUE) TF[, freq := count / size]
    }
  } else {
    TF <- DT
  }
  new("hits", dt=TF, corpus=.Object, query=query)
})


#' @rdname hits
setMethod("hits", "partition", function(.Object, query, sAttribute=NULL, pAttribute="word", size=FALSE, freq=FALSE, mc=FALSE, progress=FALSE, verbose=TRUE){
  stopifnot(all(sAttribute %in% sAttributes(.Object@corpus)))
  if (freq == TRUE) size <- TRUE
  sAttrs <- paste(.Object@corpus, sAttribute, sep=".")
  DT <- hits(.Object@corpus, query=query, sAttribute=NULL, pAttribute=pAttribute, mc=mc, progress=progress)@dt
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
  new("hits", dt=TF, corpus=.Object@corpus, query=query)
})


#' @rdname hits
setMethod("hits", "partitionBundle", function(
  .Object, query, pAttribute="word", size=TRUE, freq=FALSE,
  mc=FALSE, progress=FALSE, verbose=TRUE
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
    
    if (mc == FALSE){
      if (progress == TRUE) pb <- txtProgressBar(max=length(query), style=3)
      countDTlist <- lapply(
        c(1:length(query)),
        function(i) {
          if (progress == TRUE) pb <- setTxtProgressBar(pb, i)
          queryToPerform <- query[i]
          cposMatrix <- cpos(
            corpus, query=queryToPerform, encoding=corpusEncoding,
            verbose=ifelse(progress == TRUE, FALSE, verbose)
          )
          if (!is.null(cposMatrix)){
            dt <- data.table(cposMatrix)
            dt[, "query" := queryToPerform, with = TRUE]
            return(dt)
          } else {
            return(NULL)
          }
        }
      )
    } else if (mc == TRUE){
      countDTlist <- mclapply(
        query,
        function(queryToPerform) {
          cposMatrix <- cpos(corpus, query=queryToPerform, encoding=corpusEncoding)
          if (!is.null(cposMatrix)){
            dt <- data.table()
            dt[, query := queryToPerform]
            return(dt)
          } else {
            return(NULL)
          }
        }, mc.cores=getOption("polmineR.cores")
      )
    }
    countDT <- rbindlist(countDTlist)
    if (verbose == TRUE) message("... matching data.tables")
    countDT[, "struc" := CQI$cpos2struc(corpus, sAttributeStrucs, countDT[["V1"]]), with=TRUE]
    setkeyv(countDT, cols="struc")
    DT <- strucDT[countDT] # merge
    DT[, "dummy" := 1, with=TRUE]
    count <- function(x) return(x)
    TF <- DT[, count(.N), by=c("partition", "query"), with=TRUE]
    # TF <- DT[, count(.N), by=.(partition, query)]
    setnames(TF, old="V1", new="count")
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
