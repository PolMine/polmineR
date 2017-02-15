#' @include dispersion_class.R
NULL

#' Get counts.
#' 
#' Count number of occurrences of a query (CQP syntax may be used) or, if query is NULL (default),
#' of all tokens.
#' 
#' @seealso  For a metadata-based breakdown of counts
#' (i.e. a differentiation by s-attributes), see \code{"dispersion"}.
#' 
#' @param .Object a \code{"partition"} or \code{"partitionBundle"} object, or a
#'   character vector (length 1) providing the name of a corpus
#' @param query a character vector (one or multiple terms to be looked up), CQP
#'   syntax can be used.
#' @param cqp either logical (TRUE if query is a CQP query), or a
#'   function to check whether query is a CQP query or not (defaults to is.query
#'   auxiliary function)
#' @param pAttribute the p-attribute(s) to use
#' @param corpus name of CWB corpus
#' @param id2str logical, whether to add rownames (only if query is NULL)
#' @param mc logical, whether to use multicore (defaults to FALSE)
#' @param verbose logical, whether to be verbose
#' @param freq logical, if FALSE, counts will be reported, if TRUE, frequencies
#' @param total defaults to FALSE, if TRUE, the added value of counts (column:
#'   TOTAL) will be amended to the data.table that is returned
#' @param progress logical, whether to show progress
#' @param ... further parameters
#' @return a \code{"data.table"}
#' @exportMethod count
#' @docType methods
#' @rdname count-method
#' @name count
#' @aliases count-method
#' @seealso count
#' @examples
#' \dontrun{
#' \dontrun{
#'   use("polmineR.sampleCorpus")
#'   debates <- partition("PLPRBTTXT", list(text_id=".*"), regex=TRUE)
#'   x <- count(debates, "Arbeit") # get frequencies for one token
#'   x <- count(debates, c("Arbeit", "Freizeit", "Zukunft")) # get frequencies for multiple tokens
#'   x <- count("PLPRBTTXT", c("Migration", "Integration"), "word")
#' 
#'   debates <- partitionBundle(
#'     .Object="PLPRBTTXT",
#'     def=list(text_date=sAttributes("PLPRBTTXT", "text_date")),
#'     regex=TRUE, mc=FALSE, verbose=FALSE
#'   )
#'   aiu <- count(debates, c("Arbeit", "Integration", "Umwelt"))
#' }
#' }
setGeneric("count", function(.Object, ...){standardGeneric("count")})

#' @rdname count-method
setMethod("count", "partition", function(
  .Object, query = NULL, cqp = is.cqp, 
  id2str = TRUE, pAttribute = getOption("polmineR.pAttribute"),
  mc = getOption("polmineR.cores"), verbose = TRUE, progress = FALSE
  ){
  if (!is.null(query)){
    if (progress) verbose <- FALSE
    .getNumberOfHits <- function(query, partition, cqp, pAttribute, ...) {
      if (verbose) message("... processing query ", query)
      cposResult <- cpos(.Object=.Object, query=query, cqp=cqp, pAttribute=pAttribute, verbose=FALSE)
      ifelse(is.null(cposResult), 0, nrow(cposResult))
    }
    no <- as.integer(blapply(
      as.list(query),
      f=.getNumberOfHits,
      partition=.Object, cqp=cqp, pAttribute=pAttribute,
      mc=mc, verbose=verbose, progress=progress
    ))
    data.table(query = query, count = no, freq = no/.Object@size)
  } else {
    cpos <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
    pAttr_id <- paste(pAttribute, "_id", sep="")
    if (length(pAttribute) == 1){
      TF <- count(cpos, .Object@corpus, pAttribute)
    } else {
      idList <- lapply(pAttribute, function(p) CQI$cpos2id(.Object@corpus, p, cpos))
      names(idList) <- paste(pAttribute, "_id", sep="")
      ID <- as.data.table(idList)
      setkeyv(ID, cols=names(idList))
      count <- function(x) return(x)
      TF <- ID[, count(.N), by=c(eval(names(idList))), with=TRUE]
      setnames(TF, "V1", "count")
    }
    if (id2str == TRUE){
      dummy <- lapply(
        c(1:length(pAttribute)),
        function(i){
          str <- as.utf8(CQI$id2str(.Object@corpus, pAttribute[i], TF[[pAttr_id[i]]]), from=.Object@encoding)
          TF[, eval(pAttribute[i]) := str , with=TRUE] 
        })
      dummy <- lapply(pAttr_id, function(x) TF[, eval(x) := NULL, with=TRUE])
      setcolorder(TF, neworder=c(pAttribute, "count"))
    } else {
      setcolorder(TF, neworder=c(pAttr_id, "count"))
    }
    return(TF)
  }
})


#' @rdname count-method
#' @docType methods
setMethod("count", "partitionBundle", function(.Object, query, pAttribute = NULL, freq = FALSE, total = T, mc = F, progress = T, verbose = FALSE){
  if (verbose == TRUE) message("... preparatory work")
  DT <- hits(.Object, query=query, pAttribute=pAttribute, mc=mc, progress=progress, verbose=verbose)@dt
  if (verbose == TRUE) message("... wrapping things up")
  DT_cast <- dcast.data.table(DT, partition~query, value.var="count")
  DT_cast2 <- DT_cast[is.na(DT_cast[["partition"]]) == FALSE] # remove counts that are not in one of the partitions
  for (q in query){
    DT_cast2[, eval(q) := sapply(DT_cast2[[q]], function(x) ifelse(is.na(x), 0, x)), with=FALSE]
  }
  if (total == TRUE) DT_cast2[, "TOTAL" := rowSums(.SD), by=partition, with=TRUE]
  DT_cast2
})

#' @rdname count-method
setMethod("count", "character", function(.Object, query = NULL, pAttribute = getOption("polmineR.pAttribute"), verbose = TRUE){
  if (is.null(query)){
    if (length(system("which cwb-lexdecode", intern = TRUE) > 0)){
      # check whether cwb-lexdecode command line tool is available
      # cwb-lexdecode will be significantly faster than using rcqp
      cmd <- paste(c("cwb-lexdecode", "-f", "-P", pAttribute, .Object), collapse = " ")
      lexdecodeResult <- system(cmd, intern = TRUE)
      Encoding(lexdecodeResult) <- getEncoding(.Object)
      lexdecodeList <- strsplit(lexdecodeResult, "\t")
      TF <- data.table(
        token = sapply(lexdecodeList, function(x) x[2]),
        count = as.integer(sapply(lexdecodeList, function(x) x[1]))
      )
      Encoding(TF[["token"]]) <- "unknown"
      colnames(TF) <- c(pAttribute, "count")
      # TF[[pAttribute]] <- enctutf8(TF[[pAttribute]])
      setkeyv(TF, pAttribute)
      setorderv(TF, cols = pAttribute)
    } else {
      TF <- count(1:(size(.Object) - 1), .Object, pAttribute = pAttribute)
    }
    return(TF)
  } else {
    stopifnot(.Object %in% CQI$list_corpora())
    total <- CQI$attribute_size(.Object, pAttribute)
    count <- sapply(
      query,
      function(query)
        CQI$id2freq(
          .Object,
          pAttribute,
          CQI$str2id(.Object, pAttribute, query)
        )
    )
    freq <- count/total
    return(data.table(query = query, count = count, freq = freq))
  }
})

#' @rdname context-class
setMethod("count", "context", function(.Object) {
  .Object@count
})

#' @rdname count-method
setMethod("count", "vector", function(.Object, corpus, pAttribute){
  ids <- CQI$cpos2id(corpus, pAttribute, .Object)
  count <- tabulate(ids)
  TF <- data.table(
    id = c(0:length(count)),
    count = c(length(which(ids == 0)), count)
  )
  setkey(TF, "id")
  setnames(TF, "id", paste(pAttribute, "_id", sep=""))
  TF[count > 0]
})


#' @rdname dispersion-class
setMethod("count", "dispersion", function(.Object) .Object@count)
